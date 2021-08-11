#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "option.h"
#include "compiler.h"
#include "object.h"
#include "memory.h"
#include "debug.h"
#include "vm.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value printNative(int argCount, Value* args) {
    int i = 0;
    for (; i < argCount - 1; i++) {
        printValue(args[i]);
        putchar(' ');
    }
    if (argCount > 0) {
        printValue(args[i]);
    }
    return NIL_VAL;
}

static Value printlnNative(int argCount, Value* args) {
    printNative(argCount, args);
    putchar('\n');
    return NIL_VAL;
}

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", getLine(&function->chunk.lineArray, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        }
        else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function, int arity) {
    push(OBJ_VAL(makeString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function, arity)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();

    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;
    vm.objects = NULL;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.strings);
    initTable(&vm.globals);

    vm.initString = NULL;
    vm.initString = makeString("init", 4);

    defineNative("clock", clockNative, 0);
    defineNative("print", printNative, -1);
    defineNative("println", printlnNative, -1);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    vm.initString = NULL;
    freeObjects();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

Value popn(uint8_t n) {
    vm.stackTop -= n;
    return *vm.stackTop;
}

static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

static bool call(ObjClosure* closure, int argCount) {    
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                if (klass->initializer != NULL) {
                    return call(klass->initializer, argCount);
                }
                return true;
            }

            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }

            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);

            case OBJ_NATIVE: {
                ObjNative* native = AS_NATIVE(callee);
                if (native->arity > 0 && native->arity != argCount) {
                    runtimeError("Expected %d arguments but got %d.", native->arity, argCount);
                    return false;
                }
                Value result = native->function(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }

            default:
                break; // Non-callable object type.
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(ObjClass* klass, ObjString* name,
    int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }
    return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, int argCount) {
    Value receiver = peek(argCount);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        vm.stackTop[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));
    pop();
    push(OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    }
    else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value method = peek(0);
    ObjClass* klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static ObjString* concatenate(ObjString* a, ObjString* b) {
    int length = a->length + b->length;
    char chars[length + 1];
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    return makeString(chars, length);
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];
    register uint8_t* ip = frame->ip;

#define READ_BYTE() (*ip++)
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
    do { \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(pop()); \
        double a = AS_NUMBER(pop()); \
        push(valueType(a op b)); \
    } while (false)



    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->closure->function->chunk, (int)(ip - frame->closure->function->chunk.code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case LOAD_CONST: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }

            case LOAD_NIL:
                push(NIL_VAL);
                break;

            case LOAD_TRUE:
                push(BOOL_VAL(true));
                break;

            case LOAD_FALSE:
                push(BOOL_VAL(false));
                break;

            case POP_TOP:
                pop();
                break;

            case POP_N: {
                uint8_t n = READ_BYTE();
                popn(n);
                break;
            }

            case GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }

            case SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }

            case DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }

            case GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }

            case SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }

            case SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }

            case GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // Instance.
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));
                Value value = pop();
                pop();
                push(value);
                break;
            }

            case GET_SUPER: {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop());

                if (!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case COMPARE_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }

            case COMPARE_NOT_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(!valuesEqual(a, b)));
                break;
            }

            case COMPARE_GREATER_THAN:
                BINARY_OP(BOOL_VAL, > );
                break;

            case COMPARE_GREATER_OR_EQUAL:
                BINARY_OP(BOOL_VAL, >= );
                break;

            case COMPARE_LESS_THAN:
                BINARY_OP(BOOL_VAL, < );
                break;

            case COMPARE_LESS_OR_EQUAL:
                BINARY_OP(BOOL_VAL, <= );
                break;

            case BINARY_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    ObjString* b = AS_STRING(peek(0));
                    ObjString* a = AS_STRING(peek(1));
                    ObjString* r = concatenate(a, b);
                    pop();
                    pop();
                    push(OBJ_VAL(r));
                }
                else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                }
                else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case BINARY_SUBTRACT:
                BINARY_OP(NUMBER_VAL, -);
                break;

            case BINARY_MULTIPLY:
                BINARY_OP(NUMBER_VAL, *);
                break;

            case BINARY_DIVIDE:
                BINARY_OP(NUMBER_VAL, / );
                break;

            case UNARY_NOT:
                push(BOOL_VAL(isFalsey(pop())));
                break;

            case UNARY_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;

            case JUMP_FORWARD: {
                int16_t offset = READ_SHORT();
                ip += offset;
                break;
            }

            case POP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(pop())) ip += offset;
                break;
            }

            case POP_JUMP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(pop())) ip += offset;
                break;
            }

            case JUMP_IF_FALSE_OR_POP: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) {
                    ip += offset;
                    break;
                }
                pop();
                break;
            }

            case JUMP_IF_TRUE_OR_POP: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(peek(0))) {
                    ip += offset;
                    break;
                }
                pop();
                break;
            }

            case LOOP_BACK: {
                uint16_t offset = READ_SHORT();
                ip -= offset;
                break;
            }

            case CALL: {
                int argCount = READ_BYTE();
                frame->ip = ip;
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }

            case INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                frame->ip = ip;
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }

            case SUPER_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                frame->ip = ip;
                ObjClass* superclass = AS_CLASS(pop());
                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }

            case MAKE_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] =
                            captureUpvalue(frame->slots + index);
                    }
                    else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }

            case CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;

            case MAKE_CLASS:
                push(OBJ_VAL(newClass(READ_STRING())));
                break;

            case INHERIT: {
                Value superclass = peek(1);
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass* subclass = AS_CLASS(peek(0));
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop(); // Subclass.
                break;
            }

            case MAKE_METHOD:
                defineMethod(READ_STRING());
                break;

            case MAKE_SPECIAL_METHOD: {
                SpecialMethodType type = READ_BYTE();
                ObjClosure* method = AS_CLOSURE(pop());
                ObjClass* klass = AS_CLASS(peek(0));
                switch (type) {
                    case INITIALIZER:
                        klass->initializer = method;
                        break;

                    default:
                        runtimeError("Unknown special method %d", type);
                        break;
                }
                break;
            }

            case RETURN: {
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }

            default:
                runtimeError("Unknown opcode %d", instruction);
                return INTERPRET_RUNTIME_ERROR;
        }
    }

#undef BINARY_OP
#undef READ_STRING
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_BYTE
}

InterpretResult interpret(const char* source) {    
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    if (config.compileOnly) {
        for (int i = 0; i < function->chunk.constants.count; i++) {
            Value value = function->chunk.constants.values[i];
            if (IS_FUNCTION(value)) {
                ObjFunction* f = AS_FUNCTION(function->chunk.constants.values[i]);
                disassembleChunk(&f->chunk, f->name->chars);
            }
        }
        disassembleChunk(&function->chunk, "<script>");
        return INTERPRET_OK;
    }

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}
