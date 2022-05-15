#include <string.h>
#include <printf.h>
#include "table.h"
#include "memory.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table *table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

uint32_t getKeyIndex(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            return AS_BOOL(value);
        case VAL_NIL:
            return 0;
        case VAL_NUMBER:
            return (int) AS_NUMBER(value);
        case VAL_OBJ:
            switch (AS_OBJ(value)->type) {
                case OBJ_STRING:
                    return AS_STRING(value)->hash;
            }
    }
}

/**
 * Implements linear probing search. It looks for a stored entry based
 * on the hash of the key. If it finds an entry with the key (i.e. a
 * value has already been stored with the key) or with a NULL key (i.e.
 * no value has been stored with the key), it returns the entry. If it
 * finds an entry with a different key (i.e. a value has been stored
 * with a key whose hash code resulted in the same bucket as this key),
 * it continues searching in the next bucket.
 *
 * This function compares the keys by equality "==", which returns true
 * only if the ObjString keys are the same object in memory. Hence,
 * this table implementation assumes that the ObjStrings have already
 * been interned.
 * @param entries
 * @param capacity
 * @param key
 * @return
 */
Entry *findEntry(Entry *entries, int capacity, Value key) {
    uint32_t index = getKeyIndex(key) % capacity;
    Entry *tombstone = NULL;

    for (;;) {
        Entry *entry = &entries[index];
        if (IS_NIL(entry->key)) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (IS_STRING(entry->key) && IS_STRING(key) && AS_STRING(entry->key) == AS_STRING(key)) {
            // We found the key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

bool tableGet(Table *table, Value key, Value *value) {
    if (table->count == 0) return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (IS_NIL(entry->key)) return false;

    *value = entry->value;
    return true;
}

void adjustCapacity(Table *table, int capacity) {
    // Get entries with new capacity
    Entry *entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; ++i) {
        entries[i].key = NIL_VAL();
        entries[i].value = NIL_VAL();
    }

    // Copy over existing entries
    for (int i = 0; i < table->capacity; ++i) {
        Entry *entry = &table->entries[i];
        if (IS_NIL(entry->key)) continue;

        Entry *dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
    }

    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(Table *table, Value key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry *entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = IS_NIL(entry->key);
    // Increase the table count for a new insertion into an empty bucket.
    // Note that we don't increase the count for "tombstone buckets"
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return true;
}

void tableAddAll(Table *from, Table *to) {
    for (int i = 0; i < from->capacity; ++i) {
        Entry *entry = &from->entries[i];
        if (!IS_NIL(entry->key)) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool tableDelete(Table *table, Value key) {
    if (table->count == 0) return false;

    // Find the entry
    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (IS_NIL(entry->key)) return false;

    // Place a tombstone in the entry
    entry->key = NIL_VAL();
    entry->value = BOOL_VAL(true);
    return true;
}

// This is a version of tableGet() that checks the table's keys by comparing the
// characters of the string rather than by equality, "=="
ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry *entry = &table->entries[index];
        if (IS_NIL(entry->key)) {
            // Stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value)) return NULL;
        } else if (IS_STRING(entry->key)) {
            ObjString *stringKey = AS_STRING(entry->key);
            if (stringKey->length == length && stringKey->hash == hash
                && memcmp(stringKey->chars, chars, length) == 0) {
                // We found it
                return stringKey;
            }
        }

        index = (index + 1) % table->capacity;
    }
}

void printTable(Table *table) {
    for (int i = 0; i < table->capacity; ++i) {
        printf("(%d) ", i);
        Entry entry = table->entries[i];
        if (IS_NIL(entry.key)) {
            printf("null");
        } else {
            printValue(entry.key);
        }
        printf(" ");
        printValue(entry.value);
        printf("\n");
    }
    printf("\n");
}
