mod ast;
mod assembly;
mod parse;
mod util;
mod compile;

use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

use assembly::Emit;
use ast::Expr;
use compile::{compile, Ctx};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut infile = File::open(in_name)?;
    let mut contents = String::new();
    infile.read_to_string(&mut contents)?;

    let expr: Box<Expr> = contents.parse().unwrap();

    // start stack index at 2, since stack index 1 holds input
    let init_ctx = Ctx::new(2, None, im::HashMap::new());
    let result_asm = compile(expr, init_ctx).unwrap();
    let result = result_asm.into_iter()
        .map(|l| l.emit())
        .fold("".to_owned(), |a, b| a + "\n" + &b);

    

    let asm_program = format!(
        "
section .text
extern snek_error
global our_code_starts_here

exit_expected_number:
    mov rdi, 7
    jmp exit_err

exit_expected_bool:
    mov rdi, 8
    jmp exit_err

exit_overflow:
    mov rdi, 9
    jmp exit_err

exit_err:
    push rsp
    call snek_error

our_code_starts_here:
    mov [rsp - 8], rdi
{}
    ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

#[cfg(test)]
mod test {
    fn parse_input(input: &str) -> u64 {
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

            (shifted | preserve_sign_mask) as u64
        }
    }
    
    #[test]
    fn test_parse_num() {
        assert_eq!(parse_input("0"), 0);
        assert_eq!(parse_input("1"), 0b010);
        assert_eq!(parse_input("2"), 0b100);
        assert_eq!(parse_input("3"), 0b110);
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_input("false"), 0b01);
        assert_eq!(parse_input("true"), 0b11);
    }
}