#ifndef zlox_chunk_h
#define zlox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    LOAD_CONST,
    LOAD_NIL,
    LOAD_TRUE,
    LOAD_FALSE,
    POP_TOP,
    POP_N,
    GET_LOCAL,
    SET_LOCAL,
    DEFINE_GLOBAL,
    GET_GLOBAL,
    SET_GLOBAL,
    COMPARE_EQUAL,
    COMPARE_GREATER_THAN,
    COMPARE_LESS_THAN,
    COMPARE_NOT_EQUAL,
    COMPARE_GREATER_OR_EQUAL,
    COMPARE_LESS_OR_EQUAL,
    BINARY_ADD,
    BINARY_SUBTRACT,
    BINARY_MULTIPLY,
    BINARY_DIVIDE,
    UNARY_NOT,
    UNARY_NEGATE,
    JUMP_FORWARD,
    POP_JUMP_IF_FALSE,
    POP_JUMP_IF_TRUE,
    JUMP_IF_FALSE_OR_POP,
    JUMP_IF_TRUE_OR_POP,
    LOOP_BACK,
    RETURN,
} OpCode;


typedef struct {
    int length;
    int number;
} Line;

typedef struct {
    int count;
    int capacity;
    Line* lines;
} LineArray;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    LineArray lineArray;
    ValueArray constants;
} Chunk;

void initLineArray(LineArray* lineArray);
void freeLineArray(LineArray* lineArray);
void writeLineArray(LineArray* lineArray, int line);

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif