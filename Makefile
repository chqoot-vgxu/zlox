BUILD_DIR := build
TOOL_SOURCES := tool/pubspec.lock $(shell find tool -name '*.dart')
BUILD_SNAPSHOT := $(BUILD_DIR)/build.dart.snapshot
TEST_SNAPSHOT := $(BUILD_DIR)/test.dart.snapshot

default: test release

# Run dart pub get on tool directory.
get:
	@ cd ./tool; dart pub get

# Remove all build outputs and intermediate files.
clean:
	@ rm -rf $(BUILD_DIR)

# Run the tests for the final version of clox.
test: debug $(TEST_SNAPSHOT)
	@ dart $(TEST_SNAPSHOT) clox

$(TEST_SNAPSHOT): $(TOOL_SOURCES)
	@ mkdir -p build
	@ echo "Compiling Dart snapshot..."
	@ dart --snapshot=$@ --snapshot-kind=app-jit tool/bin/test.dart clox

# Compile a debug build of clox.
debug:
	@ $(MAKE) -f src/Makefile NAME=cloxd MODE=debug

# Compile the C interpreter.
release:
	@ $(MAKE) -f src/Makefile NAME=clox MODE=release
	@ cp build/clox clox # For convenience, copy the interpreter to the top level.

.PHONY: clean debug default get test
