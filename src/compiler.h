#ifndef zlox_compiler_h
#define zlox_compiler_h

#include "vm.h"

bool compile(const char* source, Chunk* chunk);

#endif