#ifndef zlox_debug_h
#define zlox_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
int disassembleInstruction(Chunk* chunk, int offset);
int getLine(LineArray* lineArray, int offset);

#endif