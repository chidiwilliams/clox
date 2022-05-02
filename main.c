#include <printf.h>
#include "chunk.h"
#include "debug.h"

int main(int argc, const char *argv[]) {
    Chunk chunk;
    initChunk(&chunk);

//    int constant = addConstant(&chunk, 1.2);
//    writeChunk(&chunk, OP_CONSTANT, 123);
//    writeChunk(&chunk, constant, 123);
//
//    writeChunk(&chunk, OP_RETURN, 123);

    for (int i = 0; i < 258; ++i) {
        writeConstant(&chunk, i, 1);
    }

    disassembleChunk(&chunk, "test chunk");

    freeChunk(&chunk);
    return 0;
}
