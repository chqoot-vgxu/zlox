#include <stdio.h>
#include <stdlib.h>

#include "debug.h"
#include "object.h"
#include "value.h"

int getLine(LineArray* lineArray, int offset) {
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

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-24s %4d\n", name, slot);
    return offset + 2;
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-24s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-24s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int invokeInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    printf("%-15s (%d args) %4d '", name, argCount, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 3;
}

static int makeSpecialMethodInstruction(const char* name, Chunk* chunk, int offset) {
    const char * specialName;
    SpecialMethodType type = (SpecialMethodType) chunk->code[offset + 1];
    switch (type) {
        case INITIALIZER:
            specialName = "initializer";
            break;
        
        default:
            specialName = "unknown";
    }

    printf("%-27s %d (%s)\n", name, type, specialName);
    return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && getLine(&chunk->lineArray, offset) == getLine(&chunk->lineArray, offset - 1)) {
        printf("   | ");
    }
    else {
        printf("%4d ", getLine(&chunk->lineArray, offset));
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case LOAD_CONST:
            return constantInstruction("LOAD_CONST", chunk, offset);

        case LOAD_NIL:
            return simpleInstruction("LOAD_NIL", offset);

        case LOAD_TRUE:
            return simpleInstruction("LOAD_TRUE", offset);

        case LOAD_FALSE:
            return simpleInstruction("LOAD_FALSE", offset);

        case POP_TOP:
            return simpleInstruction("POP_TOP", offset);

        case POP_N:
            return byteInstruction("POP_N", chunk, offset);

        case GET_LOCAL:
            return byteInstruction("GET_LOCAL", chunk, offset);

        case SET_LOCAL:
            return byteInstruction("SET_LOCAL", chunk, offset);

        case DEFINE_GLOBAL:
            return constantInstruction("DEFINE_GLOBAL", chunk, offset);

        case GET_GLOBAL:
            return constantInstruction("GET_GLOBAL", chunk, offset);

        case SET_GLOBAL:
            return constantInstruction("SET_GLOBAL", chunk, offset);

        case GET_UPVALUE:
            return byteInstruction("GET_UPVALUE", chunk, offset);

        case SET_UPVALUE:
            return byteInstruction("SET_UPVALUE", chunk, offset);

        case GET_PROPERTY:
            return constantInstruction("GET_PROPERTY", chunk, offset);

        case SET_PROPERTY:
            return constantInstruction("SET_PROPERTY", chunk, offset);

        case GET_SUPER:
            return constantInstruction("GET_SUPER", chunk, offset);

        case COMPARE_EQUAL:
            return simpleInstruction("COMPARE_EQUAL", offset);

        case COMPARE_NOT_EQUAL:
            return simpleInstruction("COMPARE_NOT_EQUAL", offset);

        case COMPARE_GREATER_THAN:
            return simpleInstruction("COMPARE_GREATER_THAN", offset);

        case COMPARE_GREATER_OR_EQUAL:
            return simpleInstruction("COMPARE_GREATER_OR_EQUAL", offset);

        case COMPARE_LESS_THAN:
            return simpleInstruction("COMPARE_LESS_THAN", offset);

        case COMPARE_LESS_OR_EQUAL:
            return simpleInstruction("COMPARE_LESS_OR_EQUAL", offset);

        case BINARY_ADD:
            return simpleInstruction("BINARY_ADD", offset);

        case BINARY_SUBTRACT:
            return simpleInstruction("BINARY_SUBTRACT", offset);

        case BINARY_MULTIPLY:
            return simpleInstruction("BINARY_MULTIPLY", offset);

        case BINARY_DIVIDE:
            return simpleInstruction("BINARY_DIVIDE", offset);

        case UNARY_NOT:
            return simpleInstruction("UNARY_NOT", offset);

        case UNARY_NEGATE:
            return simpleInstruction("UNARY_NEGATE", offset);

        case JUMP_FORWARD:
            return jumpInstruction("JUMP_FORWARD", 1, chunk, offset);

        case POP_JUMP_IF_FALSE:
            return jumpInstruction("POP_JUMP_IF_FALSE", 1, chunk, offset);

        case POP_JUMP_IF_TRUE:
            return jumpInstruction("POP_JUMP_IF_TRUE", 1, chunk, offset);

        case JUMP_IF_FALSE_OR_POP:
            return jumpInstruction("JUMP_IF_FALSE_OR_POP", 1, chunk, offset);

        case JUMP_IF_TRUE_OR_POP:
            return jumpInstruction("JUMP_IF_TRUE_OR_POP", 1, chunk, offset);

        case LOOP_BACK:
            return jumpInstruction("LOOP_BACK", -1, chunk, offset);

        case CALL:
            return byteInstruction("CALL", chunk, offset);

        case INVOKE:
            return invokeInstruction("INVOKE", chunk, offset);

        case SUPER_INVOKE:
            return invokeInstruction("SUPER_INVOKE", chunk, offset);

        case MAKE_CLOSURE: {
            offset++;
            uint8_t constant = chunk->code[offset++];
            printf("%-24s %4d ", "MAKE_CLOSURE", constant);
            printValue(chunk->constants.values[constant]);
            printf("\n");

            ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
            for (int j = 0; j < function->upvalueCount; j++) {
                int isLocal = chunk->code[offset++];
                int index = chunk->code[offset++];
                printf("%04d      | %25s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
            }

            return offset;
        }

        case CLOSE_UPVALUE:
            return simpleInstruction("CLOSE_UPVALUE", offset);

        case MAKE_CLASS:
            return constantInstruction("MAKE_CLASS", chunk, offset);

        case INHERIT:
            return simpleInstruction("INHERIT", offset);

        case MAKE_METHOD:
            return constantInstruction("MAKE_METHOD", chunk, offset);

        case MAKE_SPECIAL_METHOD:
            return makeSpecialMethodInstruction("MAKE_SPECIAL_METHOD", chunk, offset);

        case RETURN:
            return simpleInstruction("RETURN", offset);

        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
