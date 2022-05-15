//
// Created by Chidi Williams on 5/9/22.
//

#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H


#include "object.h"

typedef struct {
    Value key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry *entries;
} Table;

void initTable(Table *table);

void freeTable(Table *table);

bool tableGet(Table *table, Value key, Value *value);

bool tableSet(Table *table, Value key, Value value);

tableDelete(Table *table, Value key);

void tableAddAll(Table *from, Table *to);

ObjString *tableFindString(Table *table, const char *chars,
                           int length, uint32_t hash);

void printTable(Table *table);

#endif //CLOX_TABLE_H
