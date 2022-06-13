#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "value.h"
#include "chunk.h"

#define OBJ_TYPE(value)   (AS_OBJ(value)->type)

#define IS_CLOSURE(value)  isObjType(value, OBJ_CLOSURE)
#define IS_STRING(value)   isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)   isObjType(value, OBJ_NATIVE)

#define AS_CLOSURE(value)  ((ObjClosure*) AS_OBJ(value))
#define AS_STRING(value)   ((ObjString*) AS_OBJ(value))
#define AS_CSTRING(value)  (((ObjString*) AS_OBJ(value))->chars)
#define AS_FUNCTION(value) ((ObjFunction*) AS_OBJ(value))
#define AS_NATIVE(value)   (((ObjNative*) AS_OBJ(value)))

typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

struct Obj {
    ObjType type;
    struct Obj *next;
    bool isMarked;
};

typedef struct {
    Obj obj;
    int arity;
    Chunk chunk;
    ObjString *name;
    int upvalueCount;
} ObjFunction;

// TODO: Can I use InterpretResult here instead?
// Also is there a better way to return this value
// than by passing it as an argument to the function?
typedef enum {
    RESULT_OK,
    RESULT_RUNTIME_ERROR
} NativeFnResult;

typedef Value (*NativeFn)(int argCount, Value *args, NativeFnResult *result);

typedef struct {
    Obj obj;
    NativeFn function;
    int arity;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    uint32_t hash;
    // Chapter 19, Challenge #1
    // chars[] is a flexible array member instead of a
    // pointer to a character array, to improve performance
    char chars[];
};

typedef struct ObjUpvalue ObjUpvalue;

struct ObjUpvalue {
    Obj obj;
    // points to the value of the upvalue. If the upvalue is
    // open, it points to the value on the stack. If closed,
    // it points to the value in the closed field.
    Value *location;
    // points to the next open upvalue farther down the stack
    ObjUpvalue *next;
    Value closed; // closed-over variables live here
};

typedef struct {
    Obj obj;
    ObjFunction *function;
    ObjUpvalue **upvalues;
    int upvalueCount;
} ObjClosure;

ObjClosure *newClosure(ObjFunction *function);

ObjFunction *newFunction();

ObjNative *newNative(NativeFn function, int arity);

ObjString *takeString(char *chars, int length);

ObjString *copyString(const char *chars, int length);

ObjUpvalue *newUpvalue(Value *slot);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif //CLOX_OBJECT_H
