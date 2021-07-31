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
    constant = addConstant(&chunk, 3.4);
    writeChunk(&chunk, LOAD_CONST, 123);
    writeChunk(&chunk, constant, 123);

    writeChunk(&chunk, BINARY_ADD, 123);

    constant = addConstant(&chunk, 5.6);
    writeChunk(&chunk, LOAD_CONST, 123);
    writeChunk(&chunk, constant, 123);

    writeChunk(&chunk, BINARY_DIVIDE, 123);
    writeChunk(&chunk, UNARY_NEGATE, 123);
    writeChunk(&chunk, RETURN, 123);
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);
    return 0;
}
