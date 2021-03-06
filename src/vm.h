#ifndef zlox_vm_h
#define zlox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;

    bool mark;
    size_t maxHeapSize;
    int minNurserySizePercent;
    int maxNurserySizePercent;
    size_t maxNurserySize;
    size_t minNurserySize;
    size_t nurserySize;
    size_t bytesAllocated;
    size_t nextGC;
    size_t nextFullGC;
    Obj* nursery;
    Obj* tenured;
    int grayCount;
    int grayCapacity;
    Obj** grayStack;

    Table globals;
    Table strings;
    ObjString* initString;
    ObjUpvalue* openUpvalues;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif