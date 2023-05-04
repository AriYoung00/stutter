use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::{hashmap, HashMap};

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    JMP(String),
    SAR(Val),
    LABEL(String),
    CMP(Val, Val),
    JE(String),
    JG(String),
    JL(String),
    JGE(String),
    JLE(String),
    JO(),
    JNZ(String),
    TEST(Val, Val),
    CMOVE(Val, Val),
    XOR(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

// For times, am i supposed to add it for multiple times
#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => match i64::try_from(*n) {
            Ok(x) => {
                match x.checked_mul(2){
                    Some(c) => Expr::Number(c),
                    None => panic!("Invalid"),
                }
            },
            Err(_) => panic!("Invalid"),
        },
        Sexp::Atom(S(name)) if name == "true" => Expr::Boolean(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Boolean(false),
        Sexp::Atom(S(op)) if op_permit(op) == false => panic!("keyword"),
        Sexp::Atom(S(op)) => Expr::Id(op.to_string()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
                let mut inner_vec: Vec<(String, Expr)> = Vec::new();
                match e1 {
                    Sexp::List(vec1) => {
                        if vec1.len() == 0 {
                            panic!("Invalid");
                        }
                        for x in vec1 {
                            inner_vec.push(parse_bind(x));
                        }
                    }
                    _ => panic!("Invalid"),
                }
                Expr::Let(inner_vec, Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e]
                if op == "add1" || op == "sub1" || op == "isnum" || op == "isbool" =>
            {
                if op == "add1" {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                } else if op == "sub1" {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                } else if op == "isnum" {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                } else {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                }
            }
            [Sexp::Atom(S(op)), e1, e2]
                if op == "+"
                    || op == "-"
                    || op == "*"
                    || op == "="
                    || op == ">"
                    || op == "<"
                    || op == ">="
                    || op == "<=" =>
            {
                if op == "+" {
                    Expr::BinOp(
                        Op2::Plus,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == "-" {
                    Expr::BinOp(
                        Op2::Minus,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == "*" {
                    Expr::BinOp(
                        Op2::Times,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == "=" {
                    Expr::BinOp(
                        Op2::Equal,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == ">" {
                    Expr::BinOp(
                        Op2::Greater,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == "<" {
                    Expr::BinOp(
                        Op2::Less,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else if op == ">=" {
                    Expr::BinOp(
                        Op2::GreaterEqual,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                } else {
                    // must be <=
                    Expr::BinOp(
                        Op2::LessEqual,
                        Box::new(parse_expr(e1)),
                        Box::new(parse_expr(e2)),
                    )
                }
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.len() == 0 {
                    panic!("Invalid");
                }
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

// helpful for let
fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op_permit(op) && op != "input" => (op.to_string(), parse_expr(e)),
            _ => panic!("keyword"),
        },
        _ => panic!("Invalid"),
    }
}

fn op_permit(s: &String) -> bool {
    if s != "let"
        && s != "add1"
        && s != "sub1"
        && s != "isnum"
        && s != "isbool"
        && s != "true"
        && s != "false"
        && s != "set!"
        && s != "block"
        && s != "loop"
        && s != "break"
        && s != "+"
        && s != "-"
        && s != "*"
        && s != "<"
        && s != "<="
        && s != ">"
        && s != ">="
        && s != "="
    {
        return true;
    }
    false
}

fn compile_expr(
    e: &Expr,
    v: &mut Vec<Instr>,
    env: &HashMap<String, i32>,
    si: i32,
    brake: &String,
    l: &mut i32,
) {
    match e {
        Expr::Id(n) => {
            if n == "input" {
                v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)))
            } else if env.contains_key(n) {
                v.push(Instr::IMov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *env.get(n).unwrap()),
                ))
            } else {
                panic!("Unbound variable identifier {}", n);
            }
        }
        Expr::Let(n, e1) => {
            let mut counter = 0;
            let mut added_env: HashMap<String, i32> = hashmap! {};
            let mut new_env = env.clone();

            for x in n {
                if added_env.contains_key(&x.0) {
                    panic!("Duplicate binding");
                }
                compile_expr(&x.1, v, &new_env, si + counter, brake, l);
                new_env = new_env.update(x.0.to_string(), (si + counter) * 8);
                added_env = added_env.update(x.0.to_string(), (si + counter) * 8);
                v.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, (si + counter) * 8),
                    Val::Reg(Reg::RAX),
                ));
                counter += 1;
            }
            compile_expr(&e1, v, &new_env, si + counter, brake, l);
        }
        Expr::Number(n) => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))),
        Expr::Boolean(t) => match t {
            true => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),
            false => v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        },
        Expr::UnOp(subexpr, e1) => {
            compile_expr(&e1, v, env, si, brake, l);
            match subexpr {
                Op1::Add1 => {
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::JNZ("throw_error_i".to_string()));
                    v.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    v.push(Instr::JO());
                }
                Op1::Sub1 => {
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1))); // need this if assignment operators contains both
                    v.push(Instr::JNZ("throw_error_i".to_string()));
                    v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    v.push(Instr::JO());
                }
                Op1::IsNum => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::JNZ(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    v.push(Instr::JMP(end_label.to_string()));
                    v.push(Instr::LABEL(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::LABEL(end_label.to_string()));
                }
                Op1::IsBool => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    v.push(Instr::XOR(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::JNZ(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    v.push(Instr::JMP(end_label.to_string()));
                    v.push(Instr::LABEL(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::LABEL(end_label.to_string()));
                }
            }
        }
        Expr::BinOp(subexpr, e1, e2) => {
            if matches!(subexpr, Op2::Equal) {
                compile_expr(&e1, v, env, si, brake, l);
                v.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si * 8),
                    Val::Reg(Reg::RAX),
                ));
                compile_expr(e2, v, env, si + 1, brake, l);
                v.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                v.push(Instr::XOR(
                    Val::Reg(Reg::RBX),
                    Val::RegOffset(Reg::RSP, si * 8),
                ));
                v.push(Instr::TEST(Val::Reg(Reg::RBX), Val::Imm(1)));
                v.push(Instr::JNZ("throw_error_i".to_string()));
                v.push(Instr::CMP(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si * 8),
                ));
                v.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
                v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                v.push(Instr::CMOVE(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            } else {
                compile_expr(&e2, v, env, si, brake, l);
                v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                v.push(Instr::JNZ("throw_error_i".to_string()));
                v.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, si * 8),
                    Val::Reg(Reg::RAX),
                ));
                compile_expr(&e1, v, env, si + 1, brake, l);
                v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                v.push(Instr::JNZ("throw_error_i".to_string()));
                match subexpr {
                    Op2::Plus => {
                        v.push(Instr::IAdd(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JO());
                    }
                    Op2::Minus => {
                        v.push(Instr::ISub(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JO());
                    }
                    Op2::Times => {
                        v.push(Instr::SAR(Val::Reg(Reg::RAX)));
                        v.push(Instr::IMul(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JO());
                    }
                    Op2::Equal => panic!("should never happen"),
                    Op2::Greater => {
                        let end_label = new_label(l, "ifend");
                        let else_label = new_label(l, "ifelse");
                        v.push(Instr::CMP(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JG(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        v.push(Instr::JMP(end_label.to_string()));
                        v.push(Instr::LABEL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        v.push(Instr::LABEL(end_label.to_string()));
                    }
                    Op2::GreaterEqual => {
                        let end_label = new_label(l, "ifend");
                        let else_label = new_label(l, "ifelse");
                        v.push(Instr::CMP(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JGE(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        v.push(Instr::JMP(end_label.to_string()));
                        v.push(Instr::LABEL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        v.push(Instr::LABEL(end_label.to_string()));
                    }
                    Op2::Less => {
                        let end_label = new_label(l, "ifend");
                        let else_label = new_label(l, "ifelse");
                        v.push(Instr::CMP(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        v.push(Instr::JMP(end_label.to_string()));
                        v.push(Instr::LABEL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        v.push(Instr::LABEL(end_label.to_string()));
                    }
                    Op2::LessEqual => {
                        let end_label = new_label(l, "ifend");
                        let else_label = new_label(l, "ifelse");
                        v.push(Instr::CMP(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JLE(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        v.push(Instr::JMP(end_label.to_string()));
                        v.push(Instr::LABEL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        v.push(Instr::LABEL(end_label.to_string()));
                    }
                }
            }
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_expr(&cond, v, env, si, brake, l);
            v.push(Instr::CMP(Val::Reg(Reg::RAX), Val::Imm(1)));
            v.push(Instr::JE(else_label.to_string()));
            compile_expr(&thn, v, env, si, brake, l);
            v.push(Instr::JMP(end_label.to_string()));
            v.push(Instr::LABEL(else_label.to_string()));
            compile_expr(&els, v, env, si, brake, l);
            v.push(Instr::LABEL(end_label.to_string()));
        }
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            v.push(Instr::LABEL(startloop.to_string()));
            compile_expr(&e, v, env, si, &endloop, l);
            v.push(Instr::JMP(startloop.to_string()));
            v.push(Instr::LABEL(endloop.to_string()));
        }
        Expr::Break(e) => {
            if brake.is_empty() {
                panic!("break")
            }
            compile_expr(&e, v, env, si, brake, l);
            v.push(Instr::JMP(brake.to_string()));
        }
        Expr::Set(name, val) => {
            if env.contains_key(name) {
                compile_expr(&val, v, env, si, brake, l);
                v.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, *env.get(name).unwrap()),
                    Val::Reg(Reg::RAX),
                ));
            } else {
                panic!("Unbound variable identifier {}", name);
            }
        }
        Expr::Block(es) => es
            .into_iter()
            .for_each(|e| compile_expr(&e, v, env, si, brake, l)),
    }
}

fn compile_to_instrs(e: &Expr) -> Vec<Instr> {
    let mut v: Vec<Instr> = Vec::new();
    let map: HashMap<String, i32> = hashmap! {};
    let mut labels = 0;
    compile_expr(e, &mut v, &map, 2, &String::from(""), &mut labels);
    v
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) => {
            return format!("mov {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::ISub(v1, v2) => {
            return format!("sub {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::IAdd(v1, v2) => {
            return format!("add {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::IMul(v1, v2) => {
            return format!("imul {}, {}", val_to_str(&v1), val_to_str(&v2));
        }
        Instr::JMP(e) => format!("jmp {}", e),
        Instr::LABEL(e) => format!("{e}:"),
        Instr::CMP(v1, v2) => format!("cmp {}, {}", val_to_str(&v1), val_to_str(&v2)),
        Instr::JE(e) => format!("je {}", e),
        Instr::JNZ(e) => format!("jnz {}", e),
        Instr::TEST(v1, v2) => format!("test {}, {}", val_to_str(&v1), val_to_str(&v2)),
        Instr::CMOVE(v1, v2) => format!("cmove {}, {}", val_to_str(&v1), val_to_str(&v2)),
        Instr::XOR(v1, v2) => format!("xor {}, {}", val_to_str(&v1), val_to_str(&v2)),
        Instr::JO() => format!("jo throw_error_o"),
        Instr::SAR(v) => format!("sar {}, 1", val_to_str(&v)),
        Instr::JG(e) => format!("jg {}", e),
        Instr::JL(e) => format!("jl {}", e),
        Instr::JGE(e) => format!("jge {}", e),
        Instr::JLE(e) => format!("jle {}", e),
    }
}

// Is it necessary to compare RAX/RSP here?
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(rax) => match rax {
            Reg::RAX => String::from("rax"),
            Reg::RSP => String::from("rsp"),
            Reg::RDI => String::from("rdi"),
            Reg::RBX => String::from("rbx"),
        },
        Val::Imm(n) => format!("{}", *n),
        Val::RegOffset(rax, n) => match rax {
            Reg::RAX => {
                // if *n >= 0 {
                //     format!("[RAX+{}]", *n)
                // } else {
                //     format!("[RAX-{}]", *n)
                // }
                format!("[rax-{}]", *n)
            }
            Reg::RSP => {
                // if *n >= 0 {
                //     format!("[RSP+{}]", *n)
                // } else {
                //     format!("[RSP-{}]", *n)
                // }
                format!("[rsp-{}]", *n)
            }
            Reg::RDI => format!("[rdi-{}]", *n),
            Reg::RBX => format!("[rbx-{}]", *n),
        },
    }
}

fn compile(e: &Expr) -> String {
    return (&compile_to_instrs(e))
        .iter()
        .map(|c| instr_to_str(c))
        .collect::<Vec<_>>()
        .join("\n");
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexpr_result = parse(&in_contents);
    let expr;
    match sexpr_result {
        Ok(re) => expr = parse_expr(&re),
        Err(_) => panic!("Invalid"),
    }
    let result = compile(&expr);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
throw_error_i:
  mov rdi, 3
  push rsp
  call snek_error
  ret
throw_error_o:
  mov rdi, 7
  push rsp
  call snek_error
  ret
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
