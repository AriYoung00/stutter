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

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o
