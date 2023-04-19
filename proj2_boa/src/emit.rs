use crate::parse::*;

// context is (stack index, variable name->offset map)
type Context = (usize, im::HashMap<String, usize>);
type EmitResult<T> = Result<T, String>;


fn compile_id(ident: String, ctx: Context) -> EmitResult<String> {
    let (_, vars) = ctx;

    let target_offset = 8 * vars.get(&ident)
        .ok_or(format!("Unbound variable identifier {ident}"))?;
    Ok(format!("\nmov rax, [rsp - {target_offset}]"))
}

fn compile_let(idents: Vec<(String, Expr)>, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    let (mut si, mut vars) = ctx;
    let mut instrs: Vec<String> = Vec::new();

    for (ident, expr) in idents {
        let eval_res = compile(Box::new(expr), (si, vars.clone()))?;
        let stack_offset = 8 * si;
        instrs.push(eval_res);
        instrs.push(format!("\nmov [rsp - {stack_offset}], rax"));

        vars = vars.update(ident, si);
        si += 1;
    }

    let binds = instrs.join("");
    let rhs_res = compile(rhs, (si, vars))?;

    Ok(format!("\n{binds}\n{rhs_res}"))
}

fn compile_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    let (si, vars) = ctx;

    let lhs_res = compile(rhs, (si, vars.clone()))?;
    let rhs_res = compile(lhs, (si + 1, vars))?;
    let stack_offset = si * 8;

    let op_str = match op {
        BOper::Plus => "add",
        BOper::Minus => "sub",
        BOper::Times => "mul"
    };

    Ok(format!(r#"
{lhs_res}
mov [rsp - {stack_offset}], rax
{rhs_res}
{op_str} rax, [rsp - {stack_offset}]
        "#))
}

fn compile_unary(op: UOper, rhs: Box<Expr>, ctx: Context) -> EmitResult<String> {
    let op_str = match op {
        UOper::Add1 => "add rax, 1",
        UOper::Sub1 => "sub rax, 1",
    };
    let prev_str = compile(rhs, ctx)?;
    
    // Ok(format!("\n{}\n{}", prev_str, op_str))
    Ok(format!(r#"
{prev_str}
{op_str}
    "#))
}

pub fn compile(expr: Box<Expr>, ctx: Context) -> EmitResult<String> {
    use Expr::*;
    match *expr {
        Number(n) => Ok(format!("\nmov rax, {n}")),
        Id(s) => compile_id(s, ctx),
        Let(ident, rhs) => compile_let(ident, rhs, ctx),
        UnOp(op, rhs) => compile_unary(op, rhs, ctx),
        BinOp(op, lhs, rhs) => compile_binary(op, lhs, rhs, ctx)
    }
}

