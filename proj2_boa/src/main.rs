#![feature(assert_matches)]
mod parse;
mod emit;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;
use parse::Expr;

use im::HashMap;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
}


fn compile_expr(prog: Box<Expr>) -> String {
    todo!("compile_expr");
}


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // let expr = parse_expr(&parse(&in_contents).unwrap());
    let expr: Box<Expr> = in_contents.parse().unwrap();
    let result = compile_expr(expr);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
