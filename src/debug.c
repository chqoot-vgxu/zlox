#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "value.h"

static int getLine(LineArray* lineArray, int offset) {
    int expandedIndex = 0;
    for (int index = 0; index < lineArray->count; index++) {
        for (int i = 0; i < lineArray->lines[index].length; i++) {
            if (expandedIndex == offset) {
                return lineArray->lines[index].number;
            }
            expandedIndex++;
        }
    }
    return -1;
}

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && getLine(&chunk->lineArray, offset) == getLine(&chunk->lineArray, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", getLine(&chunk->lineArray, offset));
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case LOAD_CONST:
            return constantInstruction("LOAD_CONST", chunk, offset);
        case RETURN:
            return simpleInstruction("RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
