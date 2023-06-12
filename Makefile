UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
endif

tests/%.s: tests/%.snek src/main.rs
	cargo run -- $< tests/$*.s --emit=assembly --backend=llvm

tests/%.o: tests/%.snek src/main.rs
	cargo run -- $< tests/$*.o --emit=object --backend=llvm

tests/%.run: tests/%.o runtime/start.rs
	# cargo run -- $< tests/$*.o --emit=object --backend=llvm
	ar rcs tests/lib$*.a tests/$*.o
	rustc -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

input/%.s: input/%.snek src/main.rs
	cargo run -- $< input/$*.s --emit=assembly --backend=llvm

input/%.o: input/%.snek src/main.rs
	cargo run -- $< input/$*.o --emit=object --backend=llvm

input/%.run: input/%.o runtime/start.rs
	# cargo run -- $< tests/$*.o --emit=object --backend=llvm
	ar rcs input/lib$*.a input/$*.o
	rustc -L input/ -lour_code:$* runtime/start.rs -o input/$*.run
# tests/%.x86: clean tests/%.snek runtime/start.rs
# 	cargo run -- $< tests/$*.s
# 	nasm -f $(ARCH) tests/$*.s -o tests/$*_x86.o
# 	ar rcs tests/lib$*.a tests/$*_x86.o
# 	rustc --target x86_64-apple-darwin -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.x86

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o
	rm -f input/*.a input/*.s input/*.run input/*.o
