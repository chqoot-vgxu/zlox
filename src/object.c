#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

#define ALLOCATE_STRING(length, objectType) \
    (ObjString*)allocateObject(sizeof(ObjString) + length + 1, objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->isMarked = !vm.mark;

    object->next = vm.nursery;
    vm.nursery = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

    return object;
}

ObjClass* newClass(ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klass->name = name;
    klass->initializer = NULL;
    initTable(&klass->methods);
    return klass;
}

ObjInstance* newInstance(ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    initTable(&instance->fields);
    return instance;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function, int arity) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    native->arity = arity;
    return native;
}

static ObjString* allocateString(const char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_STRING(length, OBJ_STRING);
    string->length = length;
    string->hash = hash;
    memcpy(string->chars, chars, length);
    string->chars[length] = 0;

    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL);
    pop();

    return string;
}

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString* makeString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    if (interned != NULL) {
        return interned;
    }

    return allocateString(chars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->closed = NIL_VAL;
    upvalue->next = NULL;
    return upvalue;
}

static ObjString* classToString(ObjClass* klass) {
    int length = klass->name->length + 8;
    char string[length + 1];
    sprintf(string, "<class %s>", klass->name->chars);
    return makeString(string, length);
}

static ObjString* instanceToString(ObjInstance* instance) {
    int length = instance->klass->name->length + 9;
    char string[length + 1];
    sprintf(string, "%s instance", instance->klass->name->chars);
    return makeString(string, length);
}

static ObjString* functionToString(ObjFunction* function) {
    if (function->name == NULL) {
        return makeString("<script>", 8);
    }

    int length = function->name->length + 5;
    char name[length + 1];
    sprintf(name, "<fn %s>", function->name->chars);
    return makeString(name, length);
}

ObjString* objectToString(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_CLASS:
            return classToString(AS_CLASS(value));

        case OBJ_INSTANCE:
            return instanceToString(AS_INSTANCE(value));

        case OBJ_BOUND_METHOD:
            return functionToString(AS_BOUND_METHOD(value)->method->function);

        case OBJ_CLOSURE:
            return functionToString(AS_CLOSURE(value)->function);

        case OBJ_FUNCTION:
            return functionToString(AS_FUNCTION(value));

        case OBJ_NATIVE:
            return makeString("<native fn>", 11);

        case OBJ_STRING:
            return AS_STRING(value);

        case OBJ_UPVALUE:
            return makeString("upvalue", 7);

        default:
            return NULL; // Unreachable
    }
}

void printObject(Value value) {
    printf("%s", objectToString(value)->chars);
}
