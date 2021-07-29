
release:
	$(MAKE) -f src/Makefile MODE=release NAME=lox

debug:
	$(MAKE) -f src/Makefile MODE=debug NAME=loxd

clean:
	@ rm -rf build

.PHONY: debug release clean
