//
// Created by Chidi Williams on 5/5/22.
//

#include <printf.h>
#include "compiler.h"
#include "scanner.h"

void compile(const char *source) {
    initScanner(source);

    int line = -1;
    for (;;) {
        Token token = scanToken();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("   | ");
        }
        printf("%2d '%.*s'\n", token.type, token.length, token.start);

        if (token.type == TOKEN_EOF) break;
    }
}
