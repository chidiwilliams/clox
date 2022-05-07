#include <printf.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk *chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk *chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
}

void writeChunk(Chunk *chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/**
 * Adds a constant to the chunk and returns the index of the stored constant.
 *
 * @param chunk
 * @param value
 * @return
 */
int addConstant(Chunk *chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}

/**
 * Chapter 14: Challenge #2
 *
 * Adds a constant to the chunk. If the index of the constant in the chunk is
 * less than or equal to one byte, it writes an OP_CONSTANT instruction. If
 * the index is greater than one byte, it writes and OP_CONSTANT_LONG instruction
 * and stores the operand as a 24-bit number, lowest-order byte first.
 *
 * @param chunk
 * @param value
 * @param line
 */
void writeConstant(Chunk *chunk, Value value, int line) {
    int constant = addConstant(chunk, value);
    if (constant <= UINT8_MAX) {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, constant, line);
    } else {
        writeChunk(chunk, OP_CONSTANT_LONG, line); // 24-bit number
        writeChunk(chunk, constant & 0xFF, line); // lowest order byte
        writeChunk(chunk, (constant >> 8) & 0xFF, line);
        writeChunk(chunk, (constant >> 16) & 0xFF, line); // highest order byte
    }
}
