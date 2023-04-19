#![feature(assert_matches)]
mod parse;
mod emit;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use parse::Expr;


fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // let expr = parse_expr(&parse(&in_contents).unwrap());
    let expr: Box<Expr> = in_contents.parse().unwrap();
    let init_ctx = (1, im::HashMap::new());
    let result = emit::compile(expr, init_ctx).unwrap();

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
