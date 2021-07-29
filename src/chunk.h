#ifndef zlox_chunk_h
#define zlox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    CONSTANT,
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
    LineArray lines;
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