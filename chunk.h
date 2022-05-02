//
// Created by Chidi Williams on 4/5/22.
//

#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H


#include "common.h"
#include "value.h"

typedef enum {
    OP_RETURN,
    OP_CONSTANT,
    OP_CONSTANT_LONG,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t *code;
    int *lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);

void freeChunk(Chunk *chunk);

void writeChunk(Chunk *chunk, uint8_t byte, int line);

int addConstant(Chunk *chunk, Value value);

void writeConstant(Chunk *chunk, Value value, int line);

#endif //CLOX_CHUNK_H
