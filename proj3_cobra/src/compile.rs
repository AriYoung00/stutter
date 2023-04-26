use crate::ast::*;
use crate::assembly::*;

// context is (stack index, variable name->offset map)
type Ctx = (usize, im::HashMap<String, usize>);
type EmitResult<T> = Result<T, String>;
type Assembly = Vec<AssemblyLine>;

fn compile_id(ident: String, ctx: Ctx) -> EmitResult<Assembly> {
    use Instr::Mov;
    use self::Reg::RAX;
    use Val::{Reg, StackIndex};
    use AssemblyLine::Instruction;

    let (_, vars) = ctx;

    // the stack index
    let idx = *vars.get(&ident)
        .ok_or(format!("Unbound variable identifier {ident}"))?;

    // Ok(format!("\nmov rax, [rsp - {target_offset}]"))
    Ok(vec![Instruction(Mov(Reg(RAX), StackIndex(idx)))])
}

fn compile_let(idents: Vec<(String, Expr)>, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let (mut si, mut vars) = ctx;
    let mut instrs: Vec<String> = Vec::new();

    let rhs_ctx = (si + idents.len(), vars.clone());
    let rhs_res = compile(rhs, rhs_ctx)?;

    let compiled = idents.into_iter()
        .enumerate()
        .map(|(idx, (ident, expr))| {
            let new_ctx = (si + idx, vars.clone());
            compile(Box::new(expr), new_ctx)
        })
        .collect::<EmitResult<Vec<_>>>()?;
    
    Ok(compiled.into_iter()
        .flatten()
        .chain(rhs_res.into_iter())
        .collect())

    // for (ident, expr) in idents {
    //     let eval_res = compile(Box::new(expr), (si, vars.clone()))?;
    //     let stack_offset = 8 * si;

    //     instrs.push(eval_res);
    //     instrs.push(format!("\nmov [rsp - {stack_offset}], rax"));

    //     vars = vars.update(ident, si);
    //     si += 1;
    // }

    // let binds = instrs.join("");
    // let rhs_res = compile(rhs, (si, vars))?;

    // Ok(format!("\n{binds}\n{rhs_res}"))
}

fn compile_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<String> {
    let (si, vars) = ctx;

    let lhs_res = compile(rhs, (si, vars.clone()))?;
    let rhs_res = compile(lhs, (si + 1, vars))?;
    let stack_offset = si * 8;

    let op_str = match op {
        BOper::Plus => "add",
        BOper::Minus => "sub",
        BOper::Times => "imul",
        _ => todo!("implement remaining binary ops")
    };

    if op == BOper::Times {
        Ok(format!(r#"
{lhs_res}
mov [rsp - {stack_offset}], rax
{rhs_res}
{op_str} QWORD [rsp - {stack_offset}]
        "#))
    }
    else {
    Ok(format!(r#"
{lhs_res}
mov [rsp - {stack_offset}], rax
{rhs_res}
{op_str} rax, [rsp - {stack_offset}]
        "#))
    }
}

fn compile_unary(op: UOper, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<String> {
    let op_str = match op {
        UOper::Add1 => "add rax, 1",
        UOper::Sub1 => "sub rax, 1",
        _ => todo!("implement remaining unary ops")
    };
    let prev_str = compile(rhs, ctx)?;
    
    // Ok(format!("\n{}\n{}", prev_str, op_str))
    Ok(format!(r#"
{prev_str}
{op_str}
    "#))
}

fn compile_if(cond: Box<Expr>, if_arm: Box<Expr>, else_arm: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_if")
}

fn compile_loop(body: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_loop")
}

fn compile_break(body: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_break")
}

fn compile_block(body: Vec<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_block")
}

fn compile_set(id: String, expr: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_set")
}

fn compile_number(n: i64, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_number")
}

fn compile_bool(b: bool, ctx: Ctx) -> EmitResult<Assembly> {
    todo!("compile_bool")
}

pub fn compile(expr: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use Expr::*;
    match *expr {
        Number(n) => compile_number(n, ctx),
        Boolean(b) => compile_bool(b, ctx),

        Id(s) => compile_id(s, ctx),
        Let(ident, rhs) => compile_let(ident, rhs, ctx),
        Set(id, expr) => compile_set(id, expr, ctx),

        UnOp(op, rhs) => compile_unary(op, rhs, ctx),
        BinOp(op, lhs, rhs) => compile_binary(op, lhs, rhs, ctx),

        If(cond, if_arm, else_arm) => compile_if(cond, if_arm, else_arm, ctx),
        Loop(body) => compile_loop(body, ctx),
        Break(body) => compile_break(body, ctx),
        Block(body) => compile_block(body, ctx),
    }
}

