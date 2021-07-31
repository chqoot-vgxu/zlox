#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main() {
    initVM();
    Chunk chunk;
    initChunk(&chunk);
    int constant = addConstant(&chunk, 1.2);
    writeChunk(&chunk, LOAD_CONST, 123);
    writeChunk(&chunk, constant, 123);
    writeChunk(&chunk, RETURN, 123);
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);
    return 0;
}
