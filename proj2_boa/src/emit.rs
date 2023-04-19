use crate::parse::*;

type Context = (usize, im::HashMap<String, i32>);
type EmitResult<T> = Result<T, String>;

pub fn compile_let(idents: Vec<(String, Expr)>, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    todo!("compile_let")
}

pub fn compile_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    todo!("compile binary")
}

pub fn compile_unary(op: UOper, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    todo!("compile unary")
}

pub fn compile(expr: Box<Expr>, ctx: Context) -> EmitResult<String> {
    use Expr::*;
    match *expr {
        Number(n) => Ok(format!("mov rax, {}", n)),
        Id(s) => todo!("handle compile identity"),
        Let(ident, rhs) => compile_let(ident, rhs, ctx),
        UnOp(op, rhs) => todo!("handle compile UnOp"),
        BinOp(op, lhs, rhs) => todo!("handle compile BinOp"),
    }
}
