#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "value.h"

static int * expandLines(LineArray* lineArray) {
    size_t expandedLength = 0;
    for (int i = 0; i < lineArray->count; i++) {
        expandedLength += lineArray->lines[i].length;
    }

    int * expandedLines = (int*) malloc(expandedLength);
    int expandedIndex = 0;
    for (int index = 0; index < lineArray->count; index++) {
        for (int i = 0; i < lineArray->lines[index].length; i++) {
            expandedLines[expandedIndex] = lineArray->lines[index].number;
            expandedIndex++;
        }
    }

    return expandedLines;
}

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);

    int * lines = expandLines(&chunk->lines);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, lines, offset);
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

int disassembleInstruction(Chunk* chunk, int* lines, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && lines[offset] == lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }

    free(lines);
}
