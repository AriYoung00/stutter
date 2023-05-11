use std::sync::Mutex;

use crate::ast::*;
use crate::assembly::*;
use crate::util::*;

use Instr::*;
use crate::assembly::Reg::*;
use Val::*;

use lazy_static::lazy_static;


#[derive(Clone)]
pub struct Ctx {
    /// `si` represents the next stack index (with the stack being divided into word-sized chunks)
    /// which can be written to. After being written to, it should be incremented when passed to
    /// future calls to `compile`
    pub si: usize, 

    /// `current_loop_label` is the
    pub current_loop_label: Option<usize>,
    pub vars: im::HashMap<String, usize>,
}

#[allow(dead_code)]
impl Ctx {
    pub fn new(si: usize, li: Option<usize>, vars: im::HashMap<String, usize>) -> Self {
        Self {
            si, current_loop_label: li, vars
        }
    }
    pub fn with_si(self, si: usize) -> Self {
        let Ctx{current_loop_label, vars, ..} = self;
        Ctx{si, current_loop_label, vars}
    }
    pub fn with_li(self, li: usize) -> Self {
        let Ctx{si, vars, ..} = self;
        let li = Some(li);
        Ctx{si, current_loop_label: li, vars}
    }
    pub fn with_vars(self, vars: im::HashMap<String, usize>) -> Self {
        let Ctx{si, current_loop_label: li, ..} = self;
        Ctx{si, current_loop_label: li, vars}
    }
    pub fn si(&self) -> usize { self.si }
    pub fn li(&self) -> Option<usize> { self.current_loop_label }
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
    let Ctx{mut si, current_loop_label: li, mut vars} = ctx;
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

fn append_check_num(v: Val, insts: &mut Vec<Instr>) { 
    use Instr::*;
    use Val::*;
    
    if let Imm(_) = v {
        panic!("generate_check_num called with immediate in first pos");
    }

    // insts.push(Bt(v, Imm(0)));
    insts.push(Test(v, Imm(1)));
    insts.push(Jnz(EXIT_EXPECTED_NUM.to_owned()));
}

/**
 * This method checks that two values are of equal types.
 *
 * Tramples: RBX
 */
fn append_check_eq_type(v1: Val, v2: Val, insts: &mut Vec<Instr>) {
    insts.extend([
        Mov(Reg(RBX), v1),
        Xor(Reg(RBX), v2),
        Test(Reg(RBX), Imm(1)),
        Jnz(EXIT_OPERAND_MISMATCH.to_owned()),
    ])
}

fn compile_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let si = ctx.si;

    let mut body = Vec::new();
    match op {
        BOper::Plus => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.push(Add(Reg(RAX), StackIndex(si)));
            append_overflow_check(&mut body);
        },
        BOper::Minus => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.push(Sub(StackIndex(si), Reg(RAX)));
            body.push(Mov(Reg(RAX), StackIndex(si)));
            append_overflow_check(&mut body);
        },
        BOper::Times => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.extend([
                Sar(Reg(RAX)),
                IMul(Reg(RAX), StackIndex(si))
            ]);
            append_overflow_check(&mut body);
        },
        BOper::Equal =>{
            append_check_eq_type(Reg(RAX), StackIndex(si), &mut body);
            body.extend([
                Cmp(StackIndex(si), Reg(RAX)),
                Cmove(RAX, TRUE),
                Cmovne(RAX, FALSE),
            ]);
        },
        BOper::Greater => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.extend([
                Cmp(StackIndex(si), Reg(RAX)),
                Cmovg(RAX, TRUE),
                Cmovle(RAX, FALSE),
            ]);
        },
        BOper::GreaterEqual => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.extend([
                Cmp(StackIndex(si), Reg(RAX)),
                Cmovge(RAX, TRUE),
                Cmovl(RAX, FALSE),
            ]);
        },
        BOper::Less => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.extend([
                Cmp(StackIndex(si), Reg(RAX)),
                Cmovl(RAX, TRUE),
                Cmovge(RAX, FALSE),
            ]);
        },
        BOper::LessEqual => {
            append_check_num(Reg(RAX), &mut body);
            append_check_num(StackIndex(si), &mut body);
            body.extend([
                Cmp(StackIndex(si), Reg(RAX)),
                Cmovle(RAX, TRUE),
                Cmovg(RAX, FALSE),
            ]);
        },
    };

    let mut out = Vec::new();
    out.extend(compile(lhs, ctx.clone())?);
    out.push(line(Mov(StackIndex(si), Reg(RAX))));
    out.extend(compile(rhs, ctx.with_si(si + 1))?);
    out.extend(body.into_iter().map(line));

    Ok(out)
}

fn line(i: Instr) -> AssemblyLine {
    AssemblyLine::Instruction(i)
}

fn append_overflow_check(instrs: &mut Vec<Instr>) {
    instrs.push(Jo(EXIT_OVERFLOW.to_owned()));
}


/// This method will append an "assembly script" which will ensure 16-byte stack alignment onto the
/// end if `instrs`. It will also push the amount by which it had to adjust the stack alignment
/// onto the stack. This should always be used in conjunction with [`append_stack_alignment_de_fix`]
///
/// *Tramples*: RAX
#[allow(unused_variables)]
fn append_stack_alignment_fix(instrs: &mut Vec<Instr>) {
    // since we need to push an 8-byte value onto the stack, we need to check if it IS already
    // aligned correctly, and add additional space if it is
    // instrs.extend([
    //     // zero RBX in preparation
    //     And(Reg(RBX), Reg(RBX)),
    //     Test(Reg(RSP), Imm(0xF)),
    //     // if (RSP & 0xF) == 0 then it's already aligned
    //     // so if it's already aligned, we want to add 8 to RSP
    //     // so we do it here through some cleverness
    //     Setz(BL),
    //     // if already aligned, add an extra 8 bytes to stack pointer to preserve alignment after
    //     // pushing offset (which we're about to do)
    //     Mul(Reg(RBX), Imm(8)),
    //     Add(Reg(RSP), Reg(RBX)),
    //     // push 64 bit (8 byte) version of register onto stack
    //     Push(Reg(RBX)),
    // ]);

    // for now, do nothing here
}

/// This method will append an "assembly scripts" onto the current program (represented by `instrs`)
/// which will undo the effects of [`append_stack_alignment_fix`]. It pops one 64-bit value off the
/// stack, and then substracts it from RSP. Note that no instructions which effect the stack should
/// be run between [`append_stack_alignment_fix`] and this script.
///
/// *Tramples*: RAX
#[allow(unused_variables)]
fn append_stack_alignment_de_fix(instrs: &mut Vec<Instr>) {
    // here we need to pop 8 bytes of the top of the stack
    // and then subtract whatever value they held from RSP
    // instrs.extend([
    //     Pop(Reg(RAX)),
    //     Sub(Reg(RSP), Reg(RAX)),
    // ]);

    // for now do nothing
}

fn compile_unary(op: UOper, rhs: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use Instr::*;
    use Val::Reg;

    let mut body = Vec::new();
    let op = match op {
        UOper::Add1 => {
            append_check_num(Reg(RAX), &mut body);
            // imm is 2 to account for tag
            body.push(Add(Reg(RAX), Imm(2)));
            append_overflow_check(&mut body);
            body
        },
        UOper::Sub1 => {
            append_check_num(Reg(RAX), &mut body);
            // imm is 2 to account for tag
            body.push(Sub(Reg(RAX), Imm(2)));
            append_overflow_check(&mut body);
            body
        },
        UOper::IsNum => {
            // test bit zero
            body.extend([
                Test(Reg(RAX), Imm(1)),
                // if it's 1 then not a number, move false into RAX
                Cmovnz(RAX, FALSE),
                // if it's 0 then it is a number, move true into RAX
                Cmovz(RAX, TRUE),
            ]);
            body
        },
        UOper::IsBool => {
            // test bit zero
            body.extend([
                Test(Reg(RAX), Imm(1)),
                // if and result is not zero, then it is a bool
                Cmovnz(RAX, TRUE),
                // if and result is zero, then it is a number
                Cmovz(RAX, FALSE),
            ]);
            body
        },
        UOper::Print => {
            body.extend([
                Push(Reg(RDI)), // preserve input value
                Push(Reg(RAX)), // preserve RAX value
                // preserve our stack
                Sub(Reg(RSP), Imm(8 * ctx.si as i64)),
            ]);

            append_stack_alignment_fix(&mut body);
            body.push(
                Call(SNEK_PRINT.into())
            );
            append_stack_alignment_de_fix(&mut body);

            body.extend([
                // restore our stack
                Add(Reg(RSP), Imm(8 * ctx.si as i64)),
                // restore registers
                Pop(Reg(RAX)),
                Pop(Reg(RDI)),
            ]);

            body
        },
    };

    let op = op.into_iter().map(line);
    Ok(compile(rhs, ctx)?
        .into_iter()
        .chain(op)
        .collect())
}

fn compile_if(cond: Box<Expr>, then_arm: Box<Expr>, else_arm: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use AssemblyLine::Label;
    // make room for our labels
    let li = inc_li();
    let i = |v| AssemblyLine::Instruction(v);

    // new implementation
    let mut out = Vec::new();
    let else_label = format!("else_{li}");
    let end_label = format!("endif_{li}");

    out.extend(compile(cond, ctx.clone())?); // insert condition
    // spec says values other than false should go down "then..." branch
    // so remove bool check and just let Cmp/Je take care of it
    // append_check_bool(Reg(RAX), &mut out);
    out.extend([    // add actual conditional
        Cmp(Reg(RAX), FALSE),
        Je(else_label.clone()),
    ].map(line));
    out.extend(compile(then_arm, ctx.clone())?);
    out.push(i(Jmp(end_label.clone())));
    out.push(Label(else_label));
    out.extend(compile(else_arm, ctx.clone())?);
    out.push(Label(end_label));

    Ok(out)
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
        panic!("Invalid value {n}, out of range (overflow)")
    }

    let preserve_sign_mask = n & (1 << 63);
    let shifted = n << 1;
    let imm = (shifted | preserve_sign_mask) as i64;

    Ok(vec![AssemblyLine::Instruction(
            Mov(Reg(RAX), Imm(imm)))
    ])
}

fn compile_bool(b: bool, _: Ctx) -> EmitResult<Assembly> {
    Ok(vec![AssemblyLine::Instruction(match b {
        true => Mov(Reg(RAX), TRUE),
        false => Mov(Reg(RAX), FALSE),
    })])
}

fn compile_input(_: Ctx) -> EmitResult<Assembly> {
    Ok(vec![AssemblyLine::Instruction(
        Mov(Reg(RAX), Reg(RDI))
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

