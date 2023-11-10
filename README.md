# Stutter Language Compiler

## Overview

Stutter is a unique, Lisp-inspired programming language designed for the modern era. It seamlessly compiles to machine code on multiple architectures leveraging the LLVM project's powerful infrastructure. Developed as part of a compilers course, Stutter stands out for its simplicity and flexibility, embodying the essence of Lisp while introducing its own distinctive features.

## Features

- **Lisp-like Syntax**: Stutter provides a minimalist, yet powerful syntax reminiscent of Lisp, promoting a clear programming paradigm.
- **LLVM-Powered**: At its core, Stutter uses LLVM to ensure efficient, cross-platform compilation to optimized machine code.
- **Cross-Architecture Support**: Designed with portability in mind, Stutter targets multiple CPU architectures, facilitating a wide range of applications.

## Installation

To get started with Stutter, clone the repository and build the compiler following these steps:

```bash
git clone https://github.com/AriYoung00/stutter.git
cd stutter/compiler
# Follow specific build instructions for your platform
```

## Usage

To compile a Stutter program, run:

```bash
./stutterc my_program.stt
```

This will generate the corresponding machine code for your architecture. For detailed usage and options, refer to the `--help` command.

## Project Structure

- **compiler/**: Contains the core compiler source code, continuously iterated beyond assignment 4 to enhance functionality and performance.
- **proj1_adder/**, **proj2_boa/**, **proj4/**: Sample projects illustrating various features and use cases for Stutter.

## Contributing

Contributions to Stutter are welcome! Please refer to the contribution guidelines for more information on how to submit pull requests, report issues, and request features.

## License

Stutter is licensed under the MIT License. See the LICENSE file for more details.

## Acknowledgments

Special thanks to the UCSD CSE131 course staff and the LLVM community for their support and resources that made this project possible.

## Contact

For any inquiries or collaboration proposals, please contact [Ariel Young](mailto:ariyoung00@example.com).

We hope you enjoy experimenting with Stutter as much as we enjoyed creating it!

