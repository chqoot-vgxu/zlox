#ifndef zlox_value_h
#define zlox_value_h

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;


#ifdef NAN_BOXING


#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL  ((uint64_t)0x0000000100000000)
#define TAG_BOOL ((uint64_t)0x0000000200000000)
#define TAG_INT  ((uint64_t)0x0000000300000000)

typedef uint64_t Value;

#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)
#define IS_NIL(value)       ((value) == NIL_VAL)
#define IS_INT(value)       ((value & (SIGN_BIT | QNAN | TAG_INT)) == (QNAN | TAG_INT))
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)
#define IS_OBJ(value)       (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value)      ((value) == TRUE_VAL)
#define AS_INT(value)       ((int)(value))
#define AS_NUMBER(value)    valueToNum(value)
#define AS_OBJ(value)       ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)         ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL           ((Value)(uint64_t)(QNAN | TAG_BOOL))
#define TRUE_VAL            ((Value)(uint64_t)(QNAN | TAG_BOOL | true))
#define NIL_VAL             ((Value)(uint64_t)(QNAN | TAG_NIL))
#define INT_VAL(num)        ((Value)(uint64_t)(QNAN | TAG_INT | ((uint64_t)(uint32_t)(num))))
#define NUMBER_VAL(num)     numToValue(num)
#define OBJ_VAL(obj)        (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

#else
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_INT,
    VAL_NUMBER,
    VAL_OBJ
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        int integer;
        double number;
        Obj* obj;
    } as;
} Value;

#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define INT_VAL(value)    ((Value){VAL_INT, {.integer = value}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#define AS_BOOL(value)    ((value).as.boolean)
#define AS_INT(value)     ((value).as.integer)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)

#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_INT(value)     ((value).type == VAL_INT)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

#endif

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void printValue(Value value);

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
bool valuesEqual(Value a, Value b);

#endif
