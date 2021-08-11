#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "option.h"

Config config = {
    .fileName = NULL,
    .quiet = false,
    .compileOnly= false,
};

static void helpAction();
static void versionAction();
static void quietAction();
static void compileOnlyAction();

Option optionList[] = {
    {"help",        'h', helpAction,        NULL},
    {"version",     'V', versionAction,     VERSION_NUMBER},
    {"quiet",       'q', quietAction,       "Don't print version number on iteractive shell"},
    {"compileOnly",  0,  compileOnlyAction, ""},
    {NULL, 0, NULL, NULL}
};

static void helpAction() {
    printf("zlox version %s\n", optionList[1].description);
    puts("\nOPTIONS\n");
    for (int i = 2; optionList[i].longName; i++) {
        Option option = optionList[i];

        printf("  --%s", option.longName);
        if (option.shortName != 0) printf(" -%c", option.shortName);
        printf("\n      %s\n", option.description);
        putchar('\n');
    }

    exit(0);
}

static void versionAction() {
    printf("zlox version %s\n", VERSION_NUMBER);
    exit(0);
}

static void quietAction() {
    config.quiet = true;
}

static int findArg(bool useLongName, const char* name) {
    for (int i = 0; optionList[i].longName != NULL; i++) {
        Option option = optionList[i];

        if ((useLongName && strcmp(option.longName, name) == 0) || option.shortName == name[0]) {
            return i;
        }
    }

    return -1;
}

static void parseArg(bool useLongName, const char* name) {
    int index = findArg(useLongName, name);
    if (index == -1) {
        fprintf(stderr, "Invalid option %s\n", name);
        exit(-1);
    }

    Option option = optionList[index];
    if (option.action != NULL) {
        option.action(option);
    }
}

void parseArgs(int argc, const char* argv[]) {
    for (int i = 1; i < argc; i++) {
        const char* arg = argv[i];
        if (arg[0] == '-') {
            bool useLongName = arg[1] == '-';
            const char* name = &arg[1 + useLongName];
            parseArg(useLongName, name);
        }
        else if (i != argc - 1) {
            fprintf(stderr, "File name must be the last option\n");
            exit(-1);
        }
        else {
            config.fileName = arg;
        }
    }
}

static void compileOnlyAction() {
    config.compileOnly = true;
}
