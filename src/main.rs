mod ast;
mod assembly;
mod parse;
mod util;
mod x86;
mod llvm;

use std::fs::File;
use std::io::prelude::*;

use clap::{Parser, ValueEnum};

use assembly::Emit;
use ast::Program;
use inkwell::OptimizationLevel;
use util::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Backend {
    /// Use LLVM for optimizations and codegen
    LLVM,
    /// Use zero-optimization x86 assembly backend
    X86,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum OptLevel {
    /// Perform no optimizations
    None,
    /// Perform few optimizations
    Less,
    /// Perform some optimizations
    Default,
    /// Perform many optimizations
    Aggressive,
}
impl Into<OptimizationLevel> for OptLevel {
    fn into(self) -> OptimizationLevel {
        match self {
            OptLevel::None => OptimizationLevel::None,
            OptLevel::Less => OptimizationLevel::Less,
            OptLevel::Default => OptimizationLevel::Default,
            OptLevel::Aggressive => OptimizationLevel::Aggressive,
        }
    }
}


#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// The name of the .snek file to compile.
    pub input_file_name:  String,
    /// The name of the file to output to.
    pub output_file_name: String,

    /// Which backend should be used to generate code.
    #[arg(long, short, value_enum, default_value_t=Backend::X86)]
    pub backend: Backend,

    /// How much optimization should occur. Has no effect unless using the LLVM backend.
    #[arg(long, short, value_enum, default_value_t=OptLevel::Default)]
    pub optlevel: OptLevel,

    /// What type of file we should emit. Has no effect unless using the LLVM backend.
    #[arg(long, short, value_enum, default_value_t=EmitTarget::Object)]
    pub emit: EmitTarget,
}


fn main() -> std::io::Result<()> {
    let args = Cli::parse();

    let in_name  = &args.input_file_name;
    let out_name = &args.output_file_name;

    let mut infile = File::open(in_name)?;
    let mut contents = String::new();
    infile.read_to_string(&mut contents)?;

    let prog: Program = contents.parse().expect("[[invalid Invalid]]");

    match args.backend {
        Backend::X86  => emit_x86(prog, out_name),
        Backend::LLVM => emit_llvm(prog, out_name, args.optlevel.into(), args.emit),
    }
}


fn emit_x86(prog: Program, out_file_name: &str) -> std::io::Result<()> {
    let result_asm = x86::compile_program(prog).expect("[[invalid Invalid]]");
    let result = result_asm.into_iter()
        .map(|l| l.emit())
        .fold("".to_owned(), |a, b| a + "\n" + &b);

    let asm_program = format!(
        "
section .text
extern snek_error
extern snek_print
global our_code_starts_here

{EXIT_EXPECTED_NUM}:
    mov rdi, 7
    jmp exit_err

{EXIT_EXPECTED_BOOL}:
    mov rdi, 8
    jmp exit_err

{EXIT_OVERFLOW}:
    mov rdi, 9
    jmp exit_err

{EXIT_OPERAND_MISMATCH}:
    mov rdi, 10
    jmp exit_err

exit_err:
    push rsp
    mov rax, rsp
    and rax, 0xF
    jz aligned

    ; If it is not aligned, correct it
    sub rsp, rax
aligned:
    call snek_error

{result}
");

    let mut out_file = File::create(out_file_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

#[allow(dead_code)]
#[allow(unused_variables)]
fn emit_llvm(prog: Program, out_file_name: &str, opt_level: OptimizationLevel, target: EmitTarget) -> std::io::Result<()> {
    let compiled = llvm::compile_program(prog, out_file_name, opt_level, target.into()).expect("[[invalid Invalid]]");
    Ok(())
}


#[cfg(test)]
mod test {
    fn parse_input(input: &str) -> i64 {
        // TODO: parse the input string into internal value representation
        if input == "true" {
            0b11
        }
        else if input == "false" {
            0b01
        }
        else {
            let val: i64 = input.parse().expect("invalid argument");
            if val > 4611686018427387903 || val < -4611686018427387904 {
                panic!("invalid argument")
            }

            let preserve_sign_mask = val & (1 << 63);
            let shifted = val << 1;

            shifted | preserve_sign_mask// as u64
        }
    }

    #[test]
    fn test_parse_num() {
        assert_eq!(parse_input("0"), 0);
        assert_eq!(parse_input("1"), 0b010);
        assert_eq!(parse_input("2"), 0b100);
        assert_eq!(parse_input("3"), 0b110);
        assert_eq!(parse_input("-1"), -2);
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_input("false"), 0b01);
        assert_eq!(parse_input("true"), 0b11);
    }
}
