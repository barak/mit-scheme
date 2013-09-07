#-*-Makefile-*-
# tests/ffi/Makefile

# Build a test library and wrapper.

all: ffi-test-shim.so

HOST=../../src/microcode/scheme --library ../../src/lib --batch-mode

ffi-test-shim.so: ffi-test-shim.o ffi-test.o
	echo "(link-shim)" | $(HOST) -- -o $@ $^

ffi-test-shim.o: ffi-test-shim.c ffi-test.h
	echo "(compile-shim)" | $(HOST) -- -c $<

ffi-test-shim.c: ffi-test.cdecl
	echo '(generate-shim "ffi-test" "#include \"ffi-test.h\"")' | $(HOST)

# The test library itself.

ffi-test.o: ffi-test.c ffi-test.h
	echo "(compile-shim)" | $(HOST) -- -o $@ -c $<

.PHONY: build
