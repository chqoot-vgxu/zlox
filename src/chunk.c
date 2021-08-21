#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

SpecialMethodName specialMethodNames[] = {
    [INITIALIZER] = {"init", 4},
    {NULL, 0}
};

void initLineArray(LineArray* lineArray) {
    lineArray->count = 1;
    lineArray->capacity = 0;
    lineArray->lines = NULL;
}

void freeLineArray(LineArray* lineArray) {
    FREE_ARRAY(Line, lineArray->lines, lineArray->capacity);
    initLineArray(lineArray);
}

void writeLineArray(LineArray* lineArray, int line) {
    if (lineArray->capacity < lineArray->count + 1) {
        int oldCapacity = lineArray->capacity;
        lineArray->capacity = GROW_CAPACITY(oldCapacity);
        lineArray->lines = GROW_ARRAY(Line, lineArray->lines, oldCapacity, lineArray->capacity);

        // zero out new entries
        for (int i = oldCapacity; i < lineArray->capacity; i++) {
            lineArray->lines[i].length = 0;
            lineArray->lines[i].number = 0;
        }
    }

    int count = lineArray->count;
    Line* lines = lineArray->lines;

    if (lines[count - 1].number == line) {
        lines[count - 1].length++;
    } else {
        lines[count].length = 1;
        lines[count].number = line;
        lineArray->count++;
    }
}

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    initLineArray(&chunk->lineArray);
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    freeLineArray(&chunk->lineArray);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->count++;
    writeLineArray(&chunk->lineArray, line);
}

int addConstant(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}
