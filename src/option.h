#ifndef zlox_option_h
#define zlox_option_h

typedef struct {
    const char* fileName;
    bool quiet;
    bool compileOnly;
    size_t maxHeapSize;
    int maxNurserySizePercent;
    int minNurserySizePercent;
} Config;

extern Config config;

typedef struct Option Option;
typedef void (*Action)(Option option, const char* arg);

typedef struct Option {
    const char* longName;
    int length;
    char shortName;
    Action action;
    const char* description;
} Option;

extern Option argList[];

void parseArgs(int argc, const char* argv[]);

#endif