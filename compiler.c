#include <printf.h>
#include <stdlib.h>
#include <string.h>
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE

#include "debug.h"
#include "object.h"
#include "memory.h"

#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_TERNARY, // ?:
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

typedef struct {
    Token name;
    int depth;
    bool isConst;
    bool isCaptured;
} Local;

typedef struct {
    // index of the local variable/upvalue
    // this upvalue points to in the enclosing
    // compiler's array of locals/upvalues
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef struct Compiler Compiler;

struct Compiler {
    Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;
    Local locals[UINT8_COUNT]; // holds the token data of local variables
    int localCount; // number of locals currently in scope
    int scopeDepth; // number of surrounding blocks
    Upvalue upvalues[UINT8_COUNT];
};

Parser parser;

Compiler *current = NULL;

Chunk *compilingChunk;

static void errorAt(Token *token, const char *message) {
    if (parser.panicMode) return;

    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void errorAtCurrent(const char *message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }

}

static void consume(TokenType type, const char *message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static Chunk *currentChunk() {
    return &current->function->chunk;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Placeholder operand. 16-bit offset means
    // we can jump over 65,535 bytes of code.
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        errorAtCurrent("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xFF;
    currentChunk()->code[offset + 1] = jump & 0xFF;
}

static void emitReturn() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

static ObjFunction *endCompiler() {
    emitReturn();
    ObjFunction *function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(),
                         function->name != NULL ? function->name->chars : "<script>");
    }
#endif
    current = (Compiler *) current->enclosing;
    return function;
}

static void expression();

static void statement(int loopStart, int loopScopeDepth);

static void declaration(int loopStart, int loopScopeDepth);

static ParseRule *getRule(TokenType type);

static void parsePrecedence(Precedence precedence);

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule *rule = getRule(operatorType);
    parsePrecedence((Precedence) (rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            emitBytes(OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            emitByte(OP_EQUAL);
            break;
        case TOKEN_GREATER:
            emitByte(OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytes(OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            emitByte(OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            emitBytes(OP_GREATER, OP_NOT);
            break;
        case TOKEN_PLUS:
            emitByte(OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(OP_DIVIDE);
            break;
        default:
            return; // Unreachable
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE:
            emitByte(OP_FALSE);
            break;
        case TOKEN_NIL:
            emitByte(OP_NIL);
            break;
        case TOKEN_TRUE:
            emitByte(OP_TRUE);
            break;
        default:
            return; // Unreachable
    }
}

static void ternary(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence((Precedence) (PREC_TERNARY + 1));
    int endJump = emitJump(OP_JUMP);

    consume(TOKEN_COLON, "Expect ':' after ternary.");

    patchJump(elseJump);
    emitByte(OP_POP);
    parsePrecedence((Precedence) (PREC_TERNARY + 1));

    patchJump(endJump);
}

void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                errorAtCurrent("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        errorAtCurrent("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t) constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler *compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    Local *local = &current->locals[current->localCount++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
    local->isCaptured = false;
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}

static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

// If left operand expression is false, jumps to right operand.
// If true, jumps to after right operand.
// a [left operand]
// b OP_JUMP_IF_FALSE d
// c OP_JUMP f
// d OP_POP
// e [right operand]
// f ...continues
static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static uint8_t identifierConstant(Token *name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                errorAtCurrent("Can't read local variable in its own initializer.");
            } else if (local->isConst) {
                errorAtCurrent("Cannot re-assign constant local variable.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        errorAtCurrent("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler *compiler, Token *name) {
    // Exit if we're in the global scope
    if (compiler->enclosing == NULL) return -1;

    // If the local variable was declared in the enclosing compiler,
    // add a new upvalue pointing to that local variable
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t) local, true);
    }

    // Else, try to resolve the upvalue in the enclosing compiler.
    int upValue = resolveUpvalue(compiler->enclosing, name);
    if (upValue != -1) {
        return addUpvalue(compiler, (uint8_t) upValue, false);
    }

    // If unable to resolve in the enclosing compiler,
    // it implies we're in the global scope
    return -1;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t) arg);
    } else {
        emitBytes(getOp, (uint8_t) arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG:
            emitByte(OP_NOT);
            break;
        case TOKEN_MINUS:
            emitByte(OP_NEGATE);
            break;
        default:
            return; // Unreachable
    }
}

ParseRule rules[] = {
        [TOKEN_LEFT_PAREN]    = {grouping, call, PREC_CALL},
        [TOKEN_RIGHT_PAREN]   = {NULL, NULL, PREC_NONE},
        [TOKEN_LEFT_BRACE]    = {NULL, NULL, PREC_NONE},
        [TOKEN_RIGHT_BRACE]   = {NULL, NULL, PREC_NONE},
        [TOKEN_COMMA]         = {NULL, NULL, PREC_NONE},
        [TOKEN_DOT]           = {NULL, NULL, PREC_NONE},
        [TOKEN_MINUS]         = {unary, binary, PREC_TERM},
        [TOKEN_PLUS]          = {NULL, binary, PREC_TERM},
        [TOKEN_SEMICOLON]     = {NULL, NULL, PREC_NONE},
        [TOKEN_SLASH]         = {NULL, binary, PREC_FACTOR},
        [TOKEN_STAR]          = {NULL, binary, PREC_FACTOR},
        [TOKEN_BANG]          = {unary, NULL, PREC_NONE},
        [TOKEN_BANG_EQUAL]    = {NULL, binary, PREC_EQUALITY},
        [TOKEN_EQUAL]         = {NULL, NULL, PREC_NONE},
        [TOKEN_EQUAL_EQUAL]   = {NULL, binary, PREC_EQUALITY},
        [TOKEN_GREATER]       = {NULL, binary, PREC_COMPARISON},
        [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
        [TOKEN_LESS]          = {NULL, binary, PREC_COMPARISON},
        [TOKEN_LESS_EQUAL]    = {NULL, binary, PREC_COMPARISON},
        [TOKEN_IDENTIFIER]    = {variable, NULL, PREC_NONE},
        [TOKEN_STRING]        = {string, NULL, PREC_NONE},
        [TOKEN_NUMBER]        = {number, NULL, PREC_NONE},
        [TOKEN_AND]           = {NULL, and_, PREC_AND},
        [TOKEN_CLASS]         = {NULL, NULL, PREC_NONE},
        [TOKEN_ELSE]          = {NULL, NULL, PREC_NONE},
        [TOKEN_FALSE]         = {literal, NULL, PREC_NONE},
        [TOKEN_FOR]           = {NULL, NULL, PREC_NONE},
        [TOKEN_FUN]           = {NULL, NULL, PREC_NONE},
        [TOKEN_IF]            = {NULL, NULL, PREC_NONE},
        [TOKEN_NIL]           = {literal, NULL, PREC_NONE},
        [TOKEN_OR]            = {NULL, or_, PREC_OR},
        [TOKEN_PRINT]         = {NULL, NULL, PREC_NONE},
        [TOKEN_RETURN]        = {NULL, NULL, PREC_NONE},
        [TOKEN_SUPER]         = {NULL, NULL, PREC_NONE},
        [TOKEN_THIS]          = {NULL, NULL, PREC_NONE},
        [TOKEN_TRUE]          = {literal, NULL, PREC_NONE},
        [TOKEN_VAR]           = {NULL, NULL, PREC_NONE},
        [TOKEN_WHILE]         = {NULL, NULL, PREC_NONE},
        [TOKEN_ERROR]         = {NULL, NULL, PREC_NONE},
        [TOKEN_EOF]           = {NULL, NULL, PREC_NONE},
        [TOKEN_QUESTION_MARK] = {NULL, ternary, PREC_TERNARY},
};

#ifdef DEBUG_TRACE_EXECUTION
typedef struct {
    ParseFn fn;
    char *name;
} FunctionMeta;

FunctionMeta meta[] = {
        {number,   "number"},
        {grouping, "grouping"},
        {unary,    "unary"},
        {binary,   "binary"},
        {literal,  "literal"}
};

char *functionName(ParseFn parseFn) {
    for (int i = 0; i < sizeof(meta) / sizeof(meta[0]); ++i) {
        if (meta[i].fn == parseFn) return meta[i].name;
    }
    return NULL;
}

#endif

/**
 * Parses the next tokens at or below the given precedence.
 * First, it gets the *prefix* parser for the current token,
 * and calls the parser. While the next token has a higher
 * precedence than the given precedence, it gets the *infix*
 * parser for the token and calls the parser.
 * @param precedence
 */
static void parsePrecedence(Precedence precedence) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("Parsing with precedence: %d\n", precedence);
#endif

    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        errorAtCurrent("Expect expression.");
        return;
    }

#ifdef DEBUG_TRACE_EXECUTION
    printf("Calling prefix rule: %s\n", functionName(prefixRule));
#endif
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
#ifdef DEBUG_TRACE_EXECUTION
        printf("Calling infix rule: %s\n", functionName(infixRule));
#endif
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        errorAtCurrent("Invalid assignment target");
    }
}

static ParseRule *getRule(TokenType type) {
    return &rules[type];
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void ifStatement(int loopStart, int loopScopeDepth) {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);

    // Pops the if condition value in the then branch
    emitByte(OP_POP);
    statement(loopStart, loopScopeDepth);

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);

    // Pop the if condition value in the else branch
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement(loopStart, loopScopeDepth);
    patchJump(elseJump);
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static void endScopeUntil(int scopeDepth) {
    current->scopeDepth = scopeDepth;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        errorAtCurrent("Too many variables in function.");
        return;
    }
    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isConst = false;
    local->isCaptured = false;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return;

    // Report an error if a variable with the same name
    // has already been declared in this local scope
    Token *name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local *local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            errorAtCurrent("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char *errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized(bool isConst) {
    if (current->scopeDepth == 0) return;
    Local *local = &current->locals[current->localCount - 1];
    local->depth = current->scopeDepth;
    local->isConst = isConst;
}

static void defineVariable(uint8_t global, bool isConst) {
    // If we're inside a block, the variable definition has
    // already been handled by the expression parsing
    if (current->scopeDepth > 0) {
        markInitialized(isConst);
        return;
    }
    uint8_t instruction = isConst ? OP_DEFINE_GLOBAL_CONST : OP_DEFINE_GLOBAL;
    emitBytes(instruction, global);
}

static void block(int loopStart, int loopScopeDepth) {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration(loopStart, loopScopeDepth);
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    // We create a new compiler for each function being compiled
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant, false);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body");
    block(-1, -1);

    // At the end of the function, we end the compiler completely.
    // So there's no need to end scope.
    ObjFunction *function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized(false);
    function(TYPE_FUNCTION);
    defineVariable(global, false);
}

static void varDeclaration(bool isConst) {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        if (isConst) {
            errorAtCurrent("Missing initializer in const declaration.");
        }
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global, isConst);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) errorAtCurrent("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Pop condition value
    statement(loopStart, current->scopeDepth);
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP); // Pop condition value
}

static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    if (match(TOKEN_SEMICOLON)) {
        // No initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration(false);
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Pop condition value
    }

    // Increment expression.
    if (!match(TOKEN_RIGHT_PAREN)) {
        // Even though the increment expression appears before the body
        // textually, it is executed after the body. If there's an
        // increment, we jump to the body, and assign loopStart =
        // incrementStart, so that when the body ends with emitLoop(loopStart),
        // it continues at from the start of the increment.
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP); // Pop condition value
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // After the increment is done, we loop back to
        // loopStart, the point after the initializer clause.
        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement(loopStart, current->scopeDepth);
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Pop condition value
    }

    endScope();
}

// A switch body contains a case statement or a default statement.
// To handle multiple case statements, the case is followed by a
// recursive switch body.
static void switchBody(int loopStart, int loopScopeDepth) {
    if (match(TOKEN_CASE)) {
        expression();
        emitByte(OP_COMPARE);
        int caseConditionJump = emitJump(OP_JUMP_IF_FALSE);

        consume(TOKEN_COLON, "Expect ':' after case condition.");

        emitByte(OP_POP); // Pop comparison result of current case

        while (!check(TOKEN_CASE) && !check(TOKEN_DEFAULT) &&
               !check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
            declaration(loopStart, loopScopeDepth);
        }

        // At the end of the case body, jump outside the switch body
        int endJump = emitJump(OP_JUMP);

        // The case condition jump resumes at the start of the next case condition
        // if any, or at the end of the switch block if none.
        patchJump(caseConditionJump);

        // Pop comparison result of current case
        emitByte(OP_POP);

        // Continuation of switch body which may
        // contain another case statement and an
        // optional default case
        switchBody(loopStart, loopScopeDepth);

        patchJump(endJump);
    } else if (match(TOKEN_DEFAULT)) {
        consume(TOKEN_COLON, "Expect ':' after default condition");

        while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
            declaration(loopStart, loopScopeDepth);
        }
    }
}

/**
 * switchStmt -> "switch" "(" expression ")"
 *               "{" switchCase* defaultCase? "}"
 * switchCase -> "case" expression ":" statement*
 * defaultCase -> "default" ":" statement*
 */
static void switchStatement(int loopStart, int loopScopeDepth) {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after switch expression.");

    consume(TOKEN_LEFT_BRACE, "Expect '{' after switch expression.");

    switchBody(loopStart, loopScopeDepth);

    emitByte(OP_POP); // Pop switch condition value
    consume(TOKEN_RIGHT_BRACE,
            "Expect '}' after block.");
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        errorAtCurrent("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn(); // return with implicit nil
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void statement(int loopStart, int loopScopeDepth) {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement(loopStart, loopScopeDepth);
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_SWITCH)) {
        switchStatement(loopStart, loopScopeDepth);
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block(loopStart, loopScopeDepth);
        endScope();
    } else if (match(TOKEN_CONTINUE)) {
        if (loopStart == -1) {
            errorAtCurrent("Cannot call continue outside loop statement.");
        } else {
            endScopeUntil(loopScopeDepth);
            emitLoop(loopStart);
        }
        consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");
    } else {
        expressionStatement();
    }
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_CONST:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:; // Do nothing.
        }

        advance();
    }
}


static void declaration(int loopStart, int loopScopeDepth) {
    if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration(false);
    } else if (match(TOKEN_CONST)) {
        varDeclaration(true);
    } else {
        statement(loopStart, loopScopeDepth);
    }

    if (parser.panicMode) synchronize();
}

ObjFunction *compile(const char *source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration(-1, -1);
    }

    consume(TOKEN_EOF, "Expect end of expression.");

    ObjFunction *function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler *compiler = current;
    while (compiler != NULL) {
        markObject((Obj *) compiler->function);
        compiler = compiler->enclosing;
    }
}
