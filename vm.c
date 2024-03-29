#include <printf.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "vm.h"
#include "common.h"
#include "debug.h"
#include "compiler.h"
#include "object.h"
#include "time.h"
#include "memory.h"

VM vm;

void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

static void runtimeError(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    CallFrame *frame = &vm.frames[vm.frameCount - 1];
    size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
    int line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame *frame = &vm.frames[i];
        ObjFunction *function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static Value clockNative(int argCount, Value *args, NativeFnResult *result) {
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

static Value deleteNative(int argCount, Value *args, NativeFnResult *result) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("Argument must be an object instance.");
        *result = RESULT_RUNTIME_ERROR;
        return NIL_VAL();
    }

    ObjInstance *instance = AS_INSTANCE(args[0]);

    Value key = args[1];
    if (!IS_STRING(key)) {
        runtimeError("Key must be a string.");
        *result = RESULT_RUNTIME_ERROR;
        return NIL_VAL();
    }

    return BOOL_VAL(tableDelete(&instance->fields, key));
}

static Value hasNative(int argCount, Value *args, NativeFnResult *result) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("Argument must be an object instance.");
        *result = RESULT_RUNTIME_ERROR;
        return NIL_VAL();
    }

    ObjInstance *instance = AS_INSTANCE(args[0]);

    Value key = args[1];
    if (!IS_STRING(key)) {
        runtimeError("Key must be a string.");
        *result = RESULT_RUNTIME_ERROR;
        return NIL_VAL();
    }

    return BOOL_VAL(tableHasKey(&instance->fields, key));
}

static Value sqrtNative(int argCount, Value *args, NativeFnResult *result) {
    if (!IS_NUMBER(*args)) {
        runtimeError("Argument must be a number.");
        *result = RESULT_RUNTIME_ERROR;
        return NIL_VAL();
    }
    return NUMBER_VAL(sqrt(AS_NUMBER(*args)));
}

static void defineNative(const char *name, NativeFn function, int arity) {
    push(OBJ_VAL(copyString(name, (int) strlen(name))));
    push(OBJ_VAL(newNative(function, arity)));
    tableSet(&vm.globals, vm.stack[0], vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.strings);
    initTable(&vm.globals);

    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative, 0);
    defineNative("sqrt", sqrtNative, 1);
    defineNative("has", hasNative, 2);
    defineNative("delete", deleteNative, 2);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    vm.initString = NULL;
    freeObjects();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}


static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString *b = AS_STRING(peek(0));
    ObjString *a = AS_STRING(peek(1));

    int length = a->length + b->length;
    char *chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString *result = takeString(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

static bool call(ObjClosure *closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame *frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1; // -1 so that the first item in the frame is the function
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass *klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));

                Value initializer;
                if (tableGet(&klass->methods, OBJ_VAL(vm.initString), &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.", argCount);
                    return false;
                }
                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                ObjNative *native = AS_NATIVE(callee);
                if (argCount != native->arity) {
                    runtimeError("Expected %d arguments but got %d", native->arity, argCount);
                    return false;
                }

                NativeFnResult fnResult = RESULT_OK;
                Value result = native->function(argCount, vm.stackTop - argCount, &fnResult);
                if (fnResult != RESULT_OK) {
                    return false;
                }

                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // non-callable object type
        }
    }
    runtimeError("Can only call objects and classes.");
    return false;
}

static ObjUpvalue *captureUpvalue(Value *local) {
    // Starting from the upvalue closest to the top of the stack,
    // we walk through the list of upvalues till we find an
    // upvalue pointing to a location <= the local or reach the
    // end of the list of upvalues
    ObjUpvalue *prevUpvalue = NULL;
    ObjUpvalue *upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    // If the upvalue we found points to this local, return
    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    // If the upvalue we found has a location less than local,
    // then there's no existing upvalue for local. We create one
    // and then fix the upvalue list
    ObjUpvalue *createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    // If we're inserting the very first upvalue to the list,
    // prevUpvalue will be null, and we should update the vm field
    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        // Else, the previous upvalue should point to the newly
        // created upvalue as its "next"
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value *last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue *upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static void defineMethod(ObjString *name) {
    Value method = peek(0);
    ObjClass *klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, OBJ_VAL(name), method);
    pop(); // closure
}

static bool bindMethod(ObjClass *klass, ObjString *name) {
    Value method;
    if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod *bound = newBoundMethod(peek(0), AS_CLOSURE(method));
    pop(); // instance
    push(OBJ_VAL(bound));
    return true;
}

static bool invokeFromClass(ObjClass *klass, ObjString *name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }
    return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString *name, int argCount) {
    Value receiver = peek(argCount);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance *instance = AS_INSTANCE(receiver);

    // Invoke field containing a function
    Value value;
    if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
        vm.stackTop[-argCount - 1] = value; // Replace stack slot 0 with the caller
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

static InterpretResult run() {
    CallFrame *frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] >> 8) | frame->ip[-1]))

#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() (AS_STRING(READ_CONSTANT()))
#define BINARY_OP(valueType, op) \
    do {              \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers.");    \
            return INTERPRET_RUNTIME_ERROR;               \
        }             \
        double b = AS_NUMBER(pop());    \
        double a = AS_NUMBER(pop());                                 \
        push(valueType(a op b)); \
    } while(false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->closure->function->chunk,
                               (int) (frame->ip - frame->closure->function->chunk.code));
#endif

        switch (READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                // Check the result of the if condition at the top of the stack
                // and apply the jump offset to the ip.
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1]; // now in the frame of the called function
                break;
            }
            case OP_INVOKE: {
                ObjString *method = READ_STRING();
                int argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_SUPER_INVOKE: {
                ObjString *method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass *superclass = AS_CLASS(pop());
                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure *closure = newClosure(function);
                push(OBJ_VAL(closure));

                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);;
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_CLOSE_UPVALUE: {
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            }
            case OP_RETURN: {
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_NIL:
                push(NIL_VAL());
                break;
            case OP_TRUE:
                push(BOOL_VAL(true));
                break;
            case OP_FALSE:
                push(BOOL_VAL(false));
                break;
            case OP_POP:
                pop();
                break;
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString *name = READ_STRING();
                tableSet(&vm.globals, OBJ_VAL(name), peek(0));
                pop();
                break;
            }
            case OP_DEFINE_GLOBAL_CONST: {
                ObjString *name = READ_STRING();
                tableSet(&vm.globals, OBJ_VAL(name), peek(0));
                tableSet(&vm.constants, OBJ_VAL(name), NIL_VAL());
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString *name = READ_STRING();

                Value value;
                if (tableGet(&vm.constants, OBJ_VAL(name), &value)) {
                    runtimeError("Cannot re-assign constant '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableSet(&vm.globals, OBJ_VAL(name), peek(0))) {
                    tableDelete(&vm.globals, OBJ_VAL(name));
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString *name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, OBJ_VAL(name), &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_GREATER:
                BINARY_OP(BOOL_VAL, >);
                break;
            case OP_LESS:
                BINARY_OP(BOOL_VAL, <);
                break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance *instance = AS_INSTANCE(peek(0));
                ObjString *name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
                    pop(); // instance
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance *instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, OBJ_VAL(READ_STRING()), peek(0));
                Value value = pop();
                pop(); // instance
                push(value);
                break;
            }
            case OP_GET_INDEX: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance *instance = AS_INSTANCE(peek(1));

                if (!IS_STRING(peek(0))) {
                    runtimeError("Index key must be a string.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjString *key = AS_STRING(peek(0));

                Value value;
                if (tableGet(&instance->fields, OBJ_VAL(key), &value)) {
                    pop(); // key
                    pop(); // instance
                    push(value);
                    break;
                }

                runtimeError("Undefined property '%s'.", key->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            case OP_SET_INDEX: {
                if (!IS_INSTANCE(peek(2))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance *instance = AS_INSTANCE(peek(2));

                if (!IS_STRING(peek(1))) {
                    runtimeError("Index name must be a string.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjString *name = AS_STRING(peek(1));

                tableSet(&instance->fields, OBJ_VAL(name), peek(0));
                Value value = pop();
                pop(); // key
                pop(); // instance
                push(value);
                break;
            }
            case OP_GET_SUPER: {
                ObjString *name = READ_STRING();
                ObjClass *superclass = AS_CLASS(pop());

                if (!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_COMPARE: {
                Value b = pop();
                Value a = peek(0);
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_SUBTRACT:
                BINARY_OP(NUMBER_VAL, -);
                break;
            case OP_MULTIPLY:
                BINARY_OP(NUMBER_VAL, *);
                break;
            case OP_DIVIDE:
                BINARY_OP(NUMBER_VAL, /);
                break;
            case OP_NOT:
                push(BOOL_VAL(isFalsey(pop())));
                break;
            case OP_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push((NUMBER_VAL(-AS_NUMBER(pop()))));
                break;
            case OP_CLASS:
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            case OP_INHERIT: {
                Value superclass = peek(1);
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass *subclass = AS_CLASS(peek(0));
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop(); // subclass
                break;
            }
            case OP_METHOD:
                defineMethod(READ_STRING());
                break;
        }
    }
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
    ObjFunction *function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;
    push(OBJ_VAL(function));
    ObjClosure *closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);
    return run();
}
