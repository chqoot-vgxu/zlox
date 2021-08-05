#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "table.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef struct {
    Token name;
    int depth;
    bool isValue;
} Local;

typedef struct {
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Table globalValNames;

typedef void (*ParseFn)(bool canAssign);

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    }
    else if (token->type == TOKEN_ERROR) {
        // Nothing.
    }
    else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
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

static void consume(TokenType type, const char* message) {
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

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    emitByte(RETURN);
}

static uint8_t makeConstant(Value value) {
    // check if constant already exists
    for (int i = 0; i < currentChunk()->constants.count; i++) {
        if (valuesEqual(value, currentChunk()->constants.values[i])) {
            return i;
        }
    }

    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(LOAD_CONST, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    if (current->localCount > 0) {
        emitBytes(POP_N, current->localCount);
        current->localCount = 0;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitByte(COMPARE_NOT_EQUAL); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(COMPARE_EQUAL); break;
        case TOKEN_GREATER:       emitByte(COMPARE_GREATER_THAN); break;
        case TOKEN_GREATER_EQUAL: emitByte(COMPARE_GREATER_OR_EQUAL); break;
        case TOKEN_LESS:          emitByte(COMPARE_LESS_THAN); break;
        case TOKEN_LESS_EQUAL:    emitByte(COMPARE_LESS_OR_EQUAL); break;
        case TOKEN_PLUS:          emitByte(BINARY_ADD); break;
        case TOKEN_MINUS:         emitByte(BINARY_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(BINARY_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(BINARY_DIVIDE); break;
        default: return; // Unreachable.
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(LOAD_FALSE); break;
        case TOKEN_NIL: emitByte(LOAD_NIL); break;
        case TOKEN_TRUE: emitByte(LOAD_TRUE); break;
        default: return; // Unreachable.
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(makeString(parser.previous.start + 1, parser.previous.length - 2)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                // look in the outer scopes
                continue;
            }
            return i;
        }
    }

    return -1;
}

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(makeString(name->start, name->length)));
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = GET_LOCAL;
        setOp = SET_LOCAL;
    }
    else {
        arg = identifierConstant(&name);
        getOp = GET_GLOBAL;
        setOp = SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        if (setOp == SET_LOCAL && current->locals[arg].isValue) {
            error("Can't reassign to contant value");
            return;
        }
        
        ObjString* globalName = AS_STRING(currentChunk()->constants.values[arg]);
        if (setOp == SET_GLOBAL && tableFindString(&globalValNames, globalName->chars, globalName->length, globalName->hash) != NULL) {
            error("Can't reassign to contant value");
            return;
        }

        expression();
        emitBytes(setOp, arg);
    }
    else {
        emitBytes(getOp, arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG: emitByte(UNARY_NOT); break;
        case TOKEN_MINUS: emitByte(UNARY_NEGATE); break;
        default: return; // Unreachable.
    }
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_VAL]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized(bool isValue) {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
    current->locals[current->localCount - 1].isValue = isValue;
}

static void defineVariable(uint8_t global, bool isValue) {
    if (current->scopeDepth > 0) {
        markInitialized(isValue);
        return;
    }

    if (isValue) {
        tableSet(&globalValNames, AS_STRING(compilingChunk->constants.values[global]), NIL_VAL);
    }
    emitBytes(DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void varDeclaration(bool isValue) {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    }
    else {
        error("Expect expression after constant value declaration.");
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global, isValue);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(POP_TOP);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                ; // Do nothing.
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration(false);
    }
    else if (match(TOKEN_VAL)) {
        varDeclaration(true);
    }
    else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    }
    else {
        expressionStatement();
    }
}

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    initTable(&globalValNames);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    freeTable(&globalValNames);
    return !parser.hadError;
}
