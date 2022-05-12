#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "table.h"

#define STACK_MAX 256

typedef struct {
    Chunk *chunk;
    uint8_t *ip; // instruction pointer
    Value stack[STACK_MAX];
    Value *stackTop; // points to the location where the next item in the stack goes
    Table strings;
    Obj *objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();

void freeVM();

InterpretResult interpret(const char *source);

void push(Value value);

Value pop();

#endif //CLOX_VM_H
