#ifndef zlox_option_h
#define zlox_option_h

typedef struct {
    const char* fileName;
    bool quiet;
    bool compileOnly;
} Config;

extern Config config;

typedef void (*Action)();

typedef struct {
    const char* longName;
    char shortName;
    Action action;
    const char* description;
} Option;

extern Option argList[];

void parseArgs(int argc, const char* argv[]);

#endif