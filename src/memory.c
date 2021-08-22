#include <stdlib.h>

// #ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
// #endif

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    size_t delta = newSize - oldSize;
    vm.bytesAllocated += delta;
    vm.nurserySize += delta;

    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        if (vm.nurserySize > vm.nextGC) {
            collectGarbage();
        }
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL)
        exit(1);

    return result;
}

void markObject(Obj* object) {
    if (object == NULL) return;
    if (IS_MARKED(object)) return;

#ifdef DEBUG_LOG_GC
    printf("%p mark\n", (void*)object);
#endif

    MARK(object);

    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
        if (vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken\n", (void*)object);
#endif

    switch (object->type) {
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            markObject((Obj*)klass->name);
            markObject((Obj*)klass->initializer);
            markTable(&klass->methods);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            markObject((Obj*)instance->klass);
            markTable(&instance->fields);
            break;
        }

        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = (ObjBoundMethod*)object;
            markValue(bound->receiver);
            markObject((Obj*)bound->method);
            break;
        }

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }

        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }

        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;

        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    }
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            freeTable(&klass->methods);
            FREE(ObjClass, object);
            break;
        }

        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
            break;
        }

        case OBJ_BOUND_METHOD: {
            FREE(ObjBoundMethod, object);
            break;
        }

        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }

        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }

        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_STRING(object, string->length);
            break;
        }

        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
    }
}

static void markRoots() {
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    for (ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue != NULL;
        upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals);
    markCompilerRoots();
    markObject((Obj*)vm.initString);
}

static void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

static Obj* sweepGen(Obj** generation) {
    Obj* previous = NULL;
    Obj* object = *generation;
    while (object != NULL) {
        if (IS_MARKED(object)) {
            previous = object;
            object = object->next;
        }
        else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            }
            else {
                *generation = object;
            }

            freeObject(unreached);
        }
    }

    return previous;
}

static void sweep() {
    Obj* lastSurvivor = sweepGen(&vm.nursery);

    if (vm.bytesAllocated > vm.nextFullGC) {
        sweepGen(&vm.tenured);
    }

    if (lastSurvivor != NULL) {
        lastSurvivor->next = vm.survivors;
        vm.survivors = vm.nursery;
        vm.nursery = NULL;
    }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();

    size_t nextGC = vm.nurserySize * GC_HEAP_GROW_FACTOR + vm.minNurserySize;
    vm.nextGC = nextGC < vm.maxNurserySize ? nextGC : vm.maxNurserySize;
    vm.nurserySize = 0;

    size_t nextFullGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;
    vm.nextFullGC = nextFullGC < vm.maxHeapSize ? nextFullGC : vm.maxHeapSize;

    vm.mark = !vm.mark;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
        before - vm.bytesAllocated, before, vm.bytesAllocated,
        vm.nextGC);
#endif
}

void freeObjects() {
    Obj* object = vm.nursery;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    object = vm.survivors;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    object = vm.tenured;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
