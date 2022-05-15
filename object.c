#include <string.h>
#include <printf.h>
#include "object.h"
#include "memory.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

#define ALLOCATE_STRING(length) \
    (ObjString *) allocateObject(sizeof(ObjString) + length * sizeof(char), OBJ_STRING)

static Obj *allocateObject(size_t size, ObjType type) {
    Obj *object = (Obj *) reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

ObjString *allocateString(char *chars, int length, uint32_t hash) {
    ObjString *string = ALLOCATE_STRING(length);
    string->length = length;
    string->hash = hash;
    strcpy(string->chars, chars);
    tableSet(&vm.strings, OBJ_VAL(string), NIL_VAL());
    return string;
}

uint32_t hashString(const char *key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; ++i) {
        hash ^= (uint8_t) key[i];
        hash *= 16777619;
    }
    return hash;
}

/**
 * Copies the given character array into a newly allocated ObjString.
 * @param chars
 * @param length
 * @return
 */
ObjString *copyString(const char *chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    char *heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}

/**
 * Allocates and returns a new ObjString with the given characters.
 * After allocation, it frees the passed character array.
 * @param chars
 * @param length
 * @return
 */
ObjString *takeString(char *chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    ObjString *string = allocateString(chars, length, hash);
    FREE_ARRAY(char, chars, length + 1);
    return string;
}
