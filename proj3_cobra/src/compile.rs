use std::sync::Mutex;

use crate::ast::*;
use crate::assembly::*;

use Instr::*;
use crate::assembly::Reg::*;
use Val::*;

use lazy_static::lazy_static;

// context is (stack index, variable name->offset map)
// type Ctx = (usize, im::HashMap<String, usize>);
#[derive(Clone)]
pub struct Ctx {
    si: usize,
    li: Option<usize>,
    vars: im::HashMap<String, usize>,
}
impl Ctx {
    pub fn new(si: usize, li: Option<usize>, vars: im::HashMap<String, usize>) -> Self {
        Self {
            si, li, vars
        }
    }
    pub fn with_si(self, si: usize) -> Self {
        let Ctx{li, vars, ..} = self;
        Ctx{si, li, vars}
    }
    pub fn with_li(self, li: usize) -> Self {
        let Ctx{si, vars, ..} = self;
        let li = Some(li);
        Ctx{si, li, vars}
    }
    pub fn with_vars(self, vars: im::HashMap<String, usize>) -> Self {
        let Ctx{si, li, ..} = self;
        Ctx{si, li, vars}
    }
    pub fn si(&self) -> usize { self.si }
    pub fn li(&self) -> Option<usize> { self.li }
    pub fn vars(&self) -> &im::HashMap<String, usize> { &self.vars }
}

type EmitResult<T> = Result<T, String>;
type Assembly = Vec<AssemblyLine>;


lazy_static! {
    static ref LI: Mutex<usize> = Mutex::new(0);
}

const FALSE: Val = Val::Imm(0b01);
const TRUE: Val = Val::Imm(0b11);

fn inc_li() -> usize {
    let mut li_guard = LI.lock().unwrap();
    *li_guard = *li_guard + 1;
    *li_guard
}

fn get_li() -> usize {
    let li_guard = LI.lock().unwrap();
    *li_guard
}

fn compile_id(ident: String, ctx: Ctx) -> EmitResult<Assembly> {
    use Val::Reg;
    use AssemblyLine::Instruction;

    let Ctx{vars, .. } = ctx;

    // the stack index
    let idx = *vars.get(&ident)
        .ok_or(format!("Unbound variable identifier {ident}"))?;

    // Ok(format!("\nmov rax, [rsp - {target_offset}]"))
    Ok(vec![Instruction(Mov(Reg(RAX), StackIndex(idx)))])
}

fn compile_let(idents: Vec<(String, Expr)>, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let Ctx{mut si, li, mut vars} = ctx;
    let mut instrs = Vec::new();

    for (ident, expr) in idents {
        let eval_res = compile(Box::new(expr), Ctx::new(si, li, vars.clone()))?;

        instrs.extend(eval_res);
        instrs.push(AssemblyLine::Instruction(Mov(StackIndex(si), Reg(RAX))));

        vars = vars.update(ident, si);
        si += 1;
    }

    let rhs_res = compile(rhs, Ctx::new(si, li, vars))?;
    instrs.extend(rhs_res);
    Ok(instrs)
}

fn generate_check_num(v: Val) -> Vec<Instr> {
    use Instr::*;
    use Val::*;
    
    if let Imm(_) = v {
        panic!("generate_check_num called with immediate in first pos");
    }
    vec![
        Bt(v, Imm(0)),
        Jc("exit_expected_number".to_owned()),
    ]
}

fn gen_check_bool(v: Val) -> Vec<Instr> {
    use Instr::*;
    use Val::*;

    if let Imm(_) = v {
        panic!("generate_check_bool called with immediate in first pos");
    }
    vec![
        Bt(v, Imm(0)),
        Jnc("exit_expected_bool".to_owned()),
    ]
}

fn compile_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let Ctx{si, li, ..} = ctx.clone();

    let lhs_res = compile(rhs, ctx.clone())?;
    let rhs_res = compile(lhs, ctx.clone().with_si(si + 1))?;
    let op = match op {
        BOper::Plus => generate_check_num(Reg(RAX)).into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Add(Reg(RAX), StackIndex(si)),
            ])
            .collect(),
        BOper::Minus => generate_check_num(Reg(RAX))
            .into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Sub(Reg(RAX), StackIndex(si))
            ])
            .collect(),
        BOper::Times => vec![IMul(Reg(RAX), StackIndex(si))],
        BOper::Equal => vec![Cmp(Reg(RAX), StackIndex(si))],
        BOper::Greater => generate_check_num(Reg(RAX))
            .into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Cmp(Reg(RAX), StackIndex(si)),
                Cmovg(RAX, TRUE),
                Cmovle(RAX, FALSE),
            ])
            .collect(),
        BOper::GreaterEqual => generate_check_num(Reg(RAX))
            .into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Cmp(Reg(RAX), StackIndex(si)),
                Cmovge(RAX, TRUE),
                Cmovl(RAX, FALSE),
            ])
            .collect(),
        BOper::Less => generate_check_num(Reg(RAX))
            .into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Cmp(Reg(RAX), StackIndex(si)),
                Cmovl(RAX, TRUE),
                Cmovge(RAX, FALSE),
            ])
            .collect(),
        BOper::LessEqual => generate_check_num(Reg(RAX))
            .into_iter()
            .chain(generate_check_num(StackIndex(si)))
            .chain(vec![
                Cmp(Reg(RAX), StackIndex(si)),
                Cmovle(RAX, TRUE),
                Cmovg(RAX, FALSE),
            ])
            .collect(),
    };
    let op: Assembly = op.into_iter().map(|v| AssemblyLine::Instruction(v)).collect();

    Ok(lhs_res.into_iter()
        .chain(vec![AssemblyLine::Instruction(Mov(StackIndex(si), Reg(RAX)))].into_iter())
        .chain(rhs_res.into_iter())
        .chain(op.into_iter())
        .collect::<Assembly>())
}

fn overflow_check() -> Instr {
    Jo("exit_overflow".to_string())
}

fn compile_unary(op: UOper, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use Instr::*;
    use Val::Reg;

    let op = match op {
        UOper::Add1 => {
            let mut num_chk = generate_check_num(Reg(RAX));
            // imm is 2 to account for tag
            num_chk.push(Add(Reg(RAX), Imm(2)));
            num_chk.push(overflow_check());
            num_chk
        },
        UOper::Sub1 => {
            let mut num_chk = generate_check_num(Reg(RAX));
            // imm is 2 to account for tag
            num_chk.push(Sub(Reg(RAX), Imm(2)));
            num_chk.push(overflow_check());
            num_chk
        },
        UOper::IsNum => vec![
            // test bit zero
            Bt(Reg(RAX), Imm(0)),
            // if it's 1 then not a number, move false into RAX
            Cmovc(RAX, FALSE),
            // if it's 0 then it is a number, move true into RAX
            Cmovnc(RAX, TRUE),
        ],
        UOper::IsBool => vec![
            // test bit zero
            Bt(Reg(RAX), Imm(0)),
            // if it's 1 then it is a bool, move true into RAX
            Cmovc(RAX, TRUE),
            // if it's 0 then it is a number, move false into RAX
            Cmovnc(RAX, FALSE),
        ],
    };
    let op = op.into_iter().map(|v| AssemblyLine::Instruction(v));
    
    Ok(compile(rhs, ctx)?
        .into_iter()
        .chain(op)
        .collect())
}

fn map_to_lines(a: Vec<Instr>) -> Assembly {
    a.into_iter().map(|v| AssemblyLine::Instruction(v)).collect()
}

fn compile_if(cond: Box<Expr>, if_arm: Box<Expr>, else_arm: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use AssemblyLine::Label;
    // make room for our labels
    let li = inc_li();

    let mut cond_instrs = compile(cond, ctx.clone())?;
    let if_instrs = compile(if_arm, ctx.clone())?;
    let else_instrs = compile(else_arm, ctx)?;
 
    let if_body_str = format!("if_body_{li}");
    let else_body_str = format!("else_body_{li}");
    let end_body_str = format!("end_{li}");

    let i = |v| AssemblyLine::Instruction(v);

    cond_instrs.extend(map_to_lines(gen_check_bool(self::Reg(RAX))));
    cond_instrs.extend(vec![
        i(Bt(Reg(RAX), Imm(1))),
        i(Jnc(else_body_str.clone())),
        Label(if_body_str),
    ]);
    cond_instrs.extend(if_instrs);
    cond_instrs.extend(vec![
        i(Jmp(end_body_str.clone())),
        Label(else_body_str),
    ]);
    cond_instrs.extend(else_instrs);
    cond_instrs.push(Label(end_body_str));
    Ok(cond_instrs)
}

fn compile_loop(body: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use AssemblyLine::*;

    let li = inc_li();
    let body_instrs = compile(body, ctx.with_li(li))?;

    let loop_top_label = format!("loop_start_{li}");
    let loop_bot_label = format!("loop_end_{li}");
    let mut instrs = vec![Label(loop_top_label.clone())];
    instrs.extend(body_instrs);
    instrs.push(Instruction(Jmp(loop_top_label.clone())));
    instrs.push(Label(loop_bot_label));

    Ok(instrs)
}

fn compile_break(body: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    // compile body
    // then, body result should be in rax, so just jump to
    let li = ctx.li().ok_or("Error: break outside of loop".to_string())?;
    let break_label = format!("loop_end_{}", li);
    let mut body_instrs = compile(body, ctx)?;
    body_instrs.push(AssemblyLine::Instruction(Jmp(break_label)));
    Ok(body_instrs)
}

fn compile_block(body: Vec<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    Ok(body.into_iter()
        .map(|e| compile(Box::new(e), ctx.clone()))
        .collect::<EmitResult<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect())
}

fn compile_set(id: String, expr: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let vars = ctx.vars().clone();
    let idx = vars.get(&id).ok_or(format!("Unbound variable identifier {id}"))?;

    let mut body_instrs = compile(expr, ctx)?;
    body_instrs.push(AssemblyLine::Instruction(Mov(StackIndex(*idx), Reg(RAX))));
    Ok(body_instrs)
}

fn compile_number(n: i64, _: Ctx) -> EmitResult<Assembly> {
    if n > 4611686018427387903 || n < -4611686018427387904 {
        panic!("Invalid value {n}, out of range")
    }

    let preserve_sign_mask = n & (1 << 63);
    let shifted = n << 1;
    let imm = (shifted | preserve_sign_mask) as i64;

    Ok(vec![AssemblyLine::Instruction(
            Mov(Reg(RAX), Imm(imm)))
    ])
}

fn compile_bool(b: bool, _: Ctx) -> EmitResult<Assembly> {
    if b {
        Ok(map_to_lines(vec![Mov(Reg(RAX), Imm(0b11))]))
    }
    else {
        Ok(map_to_lines(vec![Mov(Reg(RAX), Imm(0b01))]))
    }
}

fn compile_input(_: Ctx) -> EmitResult<Assembly> {
    Ok(vec![AssemblyLine::Instruction(
        Mov(Reg(RAX), StackIndex(1))
    )])
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
        Input => compile_input(ctx),
    }
}

