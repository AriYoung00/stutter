use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;

use sexp::Atom::*;
use sexp::*;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    Label(String),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
    R12,
    R13,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    CMovg(Val, Val),
    CMovl(Val, Val),
    CMove(Val, Val),
    CMovnz(Val, Val),
    CMovne(Val, Val),
    CMovo(Val, Val),
    JNE(Val),
    JE(Val),
    JMP(Val),
    JNZ(Val),
    JO(Val),
    Label(Val),
    LeftShift(Val, Val),
    RightShift(Val, Val),
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

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                panic!("Invalid number, out of range")
            }
            Expr::Number(i64::try_from(*n).unwrap())
        }
        Sexp::Atom(S(bool)) if bool.to_lowercase() == "false" => Expr::Boolean(false),
        Sexp::Atom(S(bool)) if bool.to_lowercase() == "true" => Expr::Boolean(true),
        Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
        Sexp::List(vec) =>
        match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), condition_expr, then_expr, else_expr] if op == "if" => {
                Expr::If(Box::new(parse_expr(condition_expr)), Box::new(parse_expr(then_expr)), Box::new(parse_expr(else_expr)))
            }
            [Sexp::Atom(S(op)), loop_expr] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(loop_expr)))
            }
            [Sexp::Atom(S(op)), break_expr] if op == "break" => {
                Expr::Break(Box::new(parse_expr(break_expr)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), expr] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(expr)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
                match e1 {
                    Sexp::List(vec) => {
                        Expr::Let(convert_bindings(vec), Box::new(parse_expr(e2)))
                    }
                    _ => panic!("Invalid expression")
                }
            }
            _ => panic!("Invalid expression")
            }
        _ => panic!("Invalid expression")
    }
}

fn compile_to_instrs(e: &Expr, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    match e {
        Expr::Number(n) => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)));
            vec
        }
        Expr::Boolean(n) if *n == true => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec
        }
        Expr::Boolean(n) if *n == false => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec
        }
        Expr::Id(id) => {
            if id == "input" {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
                return vec
            }
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {id}")
            }
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(id).unwrap() * 8)));
            vec
        }
        Expr::UnOp(op, subexpr) => {
            compile_op1(op, subexpr, si, env, current_break, label_count)
        }
        Expr::BinOp(op, subexpr1, subexpr2) => {
            compile_op2(op, subexpr1, subexpr2, si, env, current_break, label_count)
        }
        Expr::Let(bindings, body) => {
            complie_let(bindings, body, si, env, current_break, label_count)
        }
        Expr::If(condition_expr, then_expr, else_expr) => {
            compile_if(condition_expr, then_expr, else_expr, si, env, current_break, label_count)
        }
        Expr::Loop(condition_expr) => {
            compile_loop(condition_expr, si, env, current_break, label_count)
        }
        Expr::Break(condition_expr) => {
            compile_break(condition_expr, si, env, current_break, label_count)
        }
        Expr::Set(name, expr) => {
            compile_set(name, expr, si, env, current_break, label_count)
        }
        Expr::Block(exprs) => {
            compile_block(exprs, si, env, current_break, label_count)
        }
        _ => panic!("Invalid expression")
    }
}

fn compile_op1(op: &Op1, e: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    match op {
        Op1::Add1 => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count));
            check_is_number(&mut vec);
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            check_overflow(&mut vec);
        }
        Op1::Sub1 => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count));
            check_is_number(&mut vec);
            vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            check_overflow(&mut vec);
        }
        Op1::IsBool => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count));
            vec.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::LeftShift(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
        }
        Op1::IsNum => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count));
            vec.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::Xor(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::LeftShift(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
        }
    }
    vec
}

fn compile_op2(op: & Op2, e1: &Box<Expr>, e2: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let e1_instr = compile_to_instrs(e1, si, env, current_break, label_count);
    let e2_instr = compile_to_instrs(e2, si + 1, env, current_break, label_count);
    let mut vec: Vec<Instr> = vec![];
    
    match op {
        Op2::Plus => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
            check_overflow(&mut vec);
        }
        Op2::Minus => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::ISub(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            check_overflow(&mut vec);
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
        }
        Op2::Times => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::RightShift(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
            check_overflow(&mut vec);
        }
        Op2::Greater => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // set to false
            vec.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if greater, set to true
        }
        Op2::GreaterEqual => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to true
            vec.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if less, set to false
        }
        Op2::Less => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // set to false
            vec.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if less, set to true
        }
        Op2::LessEqual => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to true
            vec.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if greater, set to false
        }
        Op2::Equal => {
            append_instr(&mut vec, e1_instr);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_equal_type(&mut vec, Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX));
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))); // set to false
            vec.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if equal, set to true
        }
    }
    vec
}

fn complie_let(bindings: &Vec<(String, Expr)>, body: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let mut set: HashSet<String> = HashSet::new();
    let mut current_env = env.clone();
    let length = bindings.len() as i32;
    for binding in bindings {
        if set.contains(&binding.0) {
            panic!("Duplicate binding")
        }
        if binding.0.eq("let") || binding.0.eq("add1") || binding.0.eq("sub1")
        ||
        binding.0.eq("break") || binding.0.eq("set!") || binding.0.eq("loop")
        ||
        binding.0.eq("if") || binding.0.eq("block") || binding.0.eq("input") {
            panic!("Invalid variable name, can't use the {} keyword", binding.0)
        }
        set.insert(binding.0.clone());
    }
    for (i, binding) in bindings.iter().enumerate() {
        let mut nenv = current_env.clone();
        append_instr(&mut vec, compile_to_instrs(&binding.1, si + i as i32, &mut nenv, current_break, label_count));
        current_env.insert(binding.0.clone(), si + i as i32);
        vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + i as i32) * 8), Val::Reg(Reg::RAX)));
    }
    append_instr(&mut vec, compile_to_instrs(body, si + length, &mut current_env, current_break, label_count));
    vec
}

fn compile_if(condition_expr: &Box<Expr>, then_expr: &Box<Expr>, else_expr:&Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let else_label = new_label(label_count, "ifelse");
    let end_label = new_label(label_count, "ifend");
    let cond_instrs = compile_to_instrs(condition_expr, si, env, current_break, label_count);
    let then_instrs = compile_to_instrs(then_expr, si, env, current_break, label_count);
    let else_instrs = compile_to_instrs(else_expr, si, env, current_break, label_count);
    append_instr(&mut vec, cond_instrs);
    vec.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::JE(Val::Label(else_label.clone())));
    append_instr(&mut vec, then_instrs);
    vec.push(Instr::JMP(Val::Label(end_label.clone())));
    vec.push(Instr::Label(Val::Label(else_label)));
    append_instr(&mut vec, else_instrs);
    vec.push(Instr::Label(Val::Label(end_label)));
    vec
}

fn compile_loop(expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let loop_label = new_label(label_count, "loop");
    let end_label = new_label(label_count, "loopend");
    let instrs = compile_to_instrs(expr, si, env, &end_label, label_count);
    vec.push(Instr::Label(Val::Label(loop_label.clone())));
    append_instr(&mut vec, instrs);
    vec.push(Instr::JMP(Val::Label(loop_label)));
    vec.push(Instr::Label(Val::Label(end_label)));
    vec
}

fn compile_break(expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    if current_break == "" {
        panic!("Invalid break")
    }
    let mut vec: Vec<Instr> = vec![];
    append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count));
    vec.push(Instr::JMP(Val::Label(current_break.to_string())));
    vec
}

fn compile_set(name: &String, expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    if !env.contains_key(name) {
        panic!("Unbound variable identifier {}", name)
    }
    let mut vec: Vec<Instr> = vec![];
    append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count));
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(name).unwrap() * 8), Val::Reg(Reg::RAX)));
    vec
}

fn compile_block(exprs: &Vec<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    if exprs.is_empty() {
        panic!("Invalid block, no subexpressions")
    }
    for expr in exprs {
        append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count))
    }
    vec
}

fn convert_bindings(vec: &Vec<Sexp>) -> Vec<(String, Expr)> {
    if vec.is_empty() {
        panic!("Invalid, no binding")
    }
    let mut rst:Vec<(String, Expr)> = vec![];
    for exp in vec {
        match exp {
            Sexp::List(v) => {
                match &v[..] {
                    [Sexp::Atom(S(s)), subexpr] => {
                        rst.push((s.to_string(), parse_expr(subexpr)))
                    }
                    _ => panic!("Invalid expression")
                }
            }
            _ => panic!("Invalid expression")
        }
    }
    rst   
}

fn new_label(label_count: &mut i32, label: &str) -> String {
    let current = *label_count;
    *label_count += 1;
    format!("{label}_{current}")
}

fn check_is_number(vec : &mut Vec<Instr>) {
    vec.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::CMovnz(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNZ(Val::Label("throw_error".to_string())));
}

#[allow(dead_code)]
fn check_is_number_debug(vec : &mut Vec<Instr>) {
    vec.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::CMovnz(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNZ(Val::Label("throw_error_debug".to_string())));
}

fn check_equal_type(vec: &mut Vec<Instr>, val1: Val, val2: Val) {
    vec.push(Instr::IMov(Val::Reg(Reg::R12), val1));
    vec.push(Instr::And(Val::Reg(Reg::R12), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::R13), val2));
    vec.push(Instr::And(Val::Reg(Reg::R13), Val::Imm(1)));
    vec.push(Instr::Cmp(Val::Reg(Reg::R12), Val::Reg(Reg::R13)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(2)));
    vec.push(Instr::CMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNE(Val::Label("throw_error".to_string())));
}

fn check_overflow(vec: &mut Vec<Instr>) {
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
    vec.push(Instr::CMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JO(Val::Label("throw_error".to_string())));
}

fn append_instr(vec: &mut Vec<Instr>, other: Vec<Instr>) {
    for instr in other {
        vec.push(instr)
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => {
            format!("mov {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovg(val1, val2) => {
            format!("cmovg {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovl(val1, val2) => {
            format!("cmovl {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMove(val1, val2) => {
            format!("cmove {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovne(val1, val2) => {
            format!("cmovne {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovnz(val1, val2) => {
            format!("cmovnz {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovo(val1, val2) => {
            format!("cmovo {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::IAdd(val1, val2) => {
            format!("add {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::ISub(val1, val2) => {
            format!("sub {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::IMul(val1, val2) => {
            format!("imul {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::And(val1, val2) => {
            format!("and {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Xor(val1, val2) => {
            format!("xor {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Cmp(val1, val2) => {
            format!("cmp {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Test(val1, val2) => {
            format!("test {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::JNE(val) => {
            format!("jne {}", val_to_str(val))
        }
        Instr::JE(val) => {
            format!("je {}", val_to_str(val))
        }
        Instr::JNZ(val) => {
            format!("jnz {}", val_to_str(val))
        }
        Instr::JMP(val) => {
            format!("jmp {}", val_to_str(val))
        }
        Instr::JO(val) => {
            format!("jo {}", val_to_str(val))
        }
        Instr::Label(label) => {
            format!("{}:", val_to_str(label))
        }
        Instr::LeftShift(val1, val2) => {
            format!("sal {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::RightShift(val1, val2) => {
            format!("sar {}, {}", val_to_str(val1), val_to_str(val2))
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => "rax".to_string(),
                Reg::RDI => "rdi".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::R12 => "r12".to_string(),
                Reg::R13 => "r13".to_string(),
                _ => panic!("Invalid register")
            }
        }
        Val::Imm(val) => {
            format!("{val}")
        }
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RSP => {
                    format!("[rsp - {}]", offset)
                }
                _ => panic!("Invalid register")
            }
        }
        Val::Label(label) => {
            label.to_string()
        }
    }
}

fn compile(e: &Expr) -> String {
    let instrs: Vec<Instr> = compile_to_instrs(e, 2, &mut HashMap::new(), &"".to_string(), &mut 0);
    let mut rst: String = String::new();
    for (i, instr) in instrs.iter().enumerate() {
        if i != 0 {
            rst.push_str("\n");
            rst.push_str("\t");
        }
        rst.push_str(&instr_to_str(instr));
    }
    rst
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    env::set_var("RUST_BACKTRACE", "1");

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    
    let expr = parse_expr(&parse(&in_contents).expect("Invalid expression"));
    let result = compile(&expr);

    let asm_program = format!("
    section .text
    extern snek_error
    extern snek_error_debug
    extern snek_print
    throw_error:
    push rsp
    call snek_error
    ret
    throw_error_debug:
    push rsp
    call snek_error_debug
    ret
    global our_code_starts_here
    our_code_starts_here:
    {}
    ret
    ", result);

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
