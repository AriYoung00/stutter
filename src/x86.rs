use std::sync::Mutex;

use crate::ast::*;
use crate::assembly::*;
use crate::util::*;

use Instr::*;
use crate::assembly::Reg::*;
use Val::*;

use lazy_static::lazy_static;


lazy_static! {
    static ref LI: Mutex<usize> = Mutex::new(0);
}

const FALSE: Val = Val::Imm(0b01);
const TRUE: Val = Val::Imm(0b11);

type Assembly = Vec<AssemblyLine>;


#[derive(Clone)]
struct Ctx {
    /// `si` represents the next stack index (with the stack being divided into word-sized chunks)
    /// which can be written to. After being written to, it should be incremented when passed to
    /// future calls to `compile`
    pub si: i64,

    /// `current_loop_label` is the
    pub current_loop_label: Option<usize>,
    pub vars: im::HashMap<String, i64>,
    pub fns:  im::HashMap<String, Vec<String>>,
}

#[allow(dead_code)]
impl Ctx {
    pub fn new(si: i64, li: Option<usize>, vars: im::HashMap<String, i64>, fns: im::HashMap<String, Vec<String>>) -> Self {
        Self {
            si, current_loop_label: li, vars, fns
        }
    }
    pub fn with_si(self, si: i64) -> Self {
        let Ctx{current_loop_label, vars, fns, ..} = self;
        Ctx{si, current_loop_label, vars, fns}
    }
    pub fn with_li(self, li: usize) -> Self {
        let Ctx{si, vars, fns, ..} = self;
        let li = Some(li);
        Ctx{si, current_loop_label: li, vars, fns}
    }
    pub fn with_vars(self, vars: im::HashMap<String, i64>) -> Self {
        let Ctx{si, current_loop_label: li, fns, ..} = self;
        Ctx{si, current_loop_label: li, vars, fns}
    }
    pub fn add_vars(mut self, vars: im::HashMap<String, i64>) -> Self {
        self.vars.extend(vars);
        self
    }
    pub fn si(&self) -> i64 { self.si }
    pub fn li(&self) -> Option<usize> { self.current_loop_label }
    pub fn vars(&self) -> &im::HashMap<String, i64> { &self.vars }

    pub fn compute_offset(&self) -> Val { Imm(self.si * 8) }
}


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
    let Ctx{mut si, current_loop_label: li, mut vars, fns} = ctx;
    let mut instrs = Vec::new();

    for (ident, expr) in idents {
        let eval_res = compile_expr(Box::new(expr), Ctx::new(si, li, vars.clone(), fns.clone()))?;

        instrs.extend(eval_res);
        instrs.push(AssemblyLine::Instruction(Mov(StackIndex(si), Reg(RAX))));

        vars = vars.update(ident, si);
        si += 1;
    }

    let rhs_res = compile_expr(rhs, Ctx::new(si, li, vars, fns))?;
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
        BOper::StructEqual => todo!("compile StructEqual in x86, ignore this"),
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
    out.extend(compile_expr(lhs, ctx.clone())?);
    out.push(line(Mov(StackIndex(si), Reg(RAX))));
    out.extend(compile_expr(rhs, ctx.with_si(si + 1))?);
    out.extend(body.into_iter().map(line));

    Ok(out)
}

fn line(i: Instr) -> AssemblyLine {
    i.into()
}

fn append_overflow_check(instrs: &mut Vec<Instr>) {
    instrs.push(Jo(EXIT_OVERFLOW.to_owned()));
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
//      exit_err:
//          mov rax, rsp
//          and rax, 0xF
//          jz aligned
//
//          ; If it is not aligned, correct it
//          sub rsp, rax
//      aligned:
            let alignment_label = format!("aligned_{}", inc_li());
            let mut body = Vec::new();
            body.extend([
                // preserve our stack
                Sub(Reg(RSP), Imm(8 * ctx.si as i64)),
                Push(Reg(RDI)), // preserve input value
                Push(Reg(RAX)), // preserve RAX value
                Mov(Reg(RDI), Reg(RAX)), // set input to print

                // fix stack pointer
                Mov(Reg(RAX), Reg(RSP)),
                And(Reg(RAX), Imm(0xF)),
                Jz(alignment_label.clone()),
                Sub(Reg(RSP), Reg(RAX)),
            ].map(line));
            body.push(AssemblyLine::Label(alignment_label));

            // append_stack_alignment_fix(&mut body);
            body.extend([
                // finish fixing stack pointer...
                Push(Reg(RAX)),
                Push(Reg(RAX)),
                Call(SNEK_PRINT.into()),
            ].map(line));
            // append_stack_alignment_de_fix(&mut body);

            body.extend([
                // un-fix stack pointer
                Pop(Reg(RAX)),
                Pop(Reg(RAX)),
                Add(Reg(RSP), Reg(RAX)),

                // restore registers
                Pop(Reg(RAX)),
                Pop(Reg(RDI)),
                // restore our stack
                Add(Reg(RSP), Imm(8 * ctx.si as i64)),
            ].map(line));

            return Ok(compile_expr(rhs, ctx)?
                .into_iter()
                .chain(body)
                .collect());
        },
    };

    let op = op.into_iter().map(line);
    Ok(compile_expr(rhs, ctx)?
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

    out.extend(compile_expr(cond, ctx.clone())?); // insert condition
    // spec says values other than false should go down "then..." branch
    // so remove bool check and just let Cmp/Je take care of it
    // append_check_bool(Reg(RAX), &mut out);
    out.extend([    // add actual conditional
        Cmp(Reg(RAX), FALSE),
        Je(else_label.clone()),
    ].map(line));
    out.extend(compile_expr(then_arm, ctx.clone())?);
    out.push(i(Jmp(end_label.clone())));
    out.push(Label(else_label));
    out.extend(compile_expr(else_arm, ctx.clone())?);
    out.push(Label(end_label));

    Ok(out)
}

fn compile_loop(body: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use AssemblyLine::*;

    let li = inc_li();
    let body_instrs = compile_expr(body, ctx.with_li(li))?;

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
    let mut body_instrs = compile_expr(body, ctx)?;
    body_instrs.push(AssemblyLine::Instruction(Jmp(break_label)));
    Ok(body_instrs)
}

fn compile_block(body: Vec<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    Ok(body.into_iter()
        .map(|e| compile_expr(Box::new(e), ctx.clone()))
        .collect::<EmitResult<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect())
}

fn compile_set(id: String, expr: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    let vars = ctx.vars().clone();
    let idx = vars.get(&id).ok_or(format!("Unbound variable identifier {id}"))?;

    let mut body_instrs = compile_expr(expr, ctx)?;
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

fn compile_call(name: String, args: Vec<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    // this function will perform the following:
    // 1. it will look up the function with name `name` in `ctx` and verify it exists
    // 2. it will verify that the number of arguments in `args` matches the expected number of
    //     arguments in `ctx`
    // 3. it will decrement the stack pointer so that variables / temporaries in the current
    //     context are not trampled by the call
    // 4. it will push the arguments of this function call onto the stack from first to last
    // 5. it will call the function using `call`
    // 6. it will pop the arguments back of the stack
    // 7. it will restore rsp to its previous value, so that after this point, stack lookups can
    //     precede as they would have in the enclosing environment

    let Some(ref fn_args) = ctx.fns.get(&name) else {
        return Err(format!("Unable to find function named {name}"));
    };

    if fn_args.len() != args.len() {
        return Err(format!("Found incorrect number of arguments for '{name}' -- expected {}, found {}",
                fn_args.len(), args.len()));
    };

    let (new_si, mut res): (_, Assembly) = args.into_iter()
        .try_fold((ctx.si, Vec::new()), |(si, mut instrs), expr| -> EmitResult<(i64, Vec<AssemblyLine>)> {
            instrs.extend(compile_expr(Box::new(expr), ctx.clone().with_si(si))?);
            instrs.push(Mov(StackIndex(si), Reg(RAX)).into());
            Ok((si + 1, instrs))
        })?;

    let ctx = ctx.with_si(new_si);
    res.extend([
        Sub(Reg(RSP), ctx.compute_offset()),
        Call(format!("snek_fun_{name}")),
        Add(Reg(RSP), ctx.compute_offset()),
    ].map(line));

    Ok(res)
}

fn compile_expr(expr: Box<Expr>, ctx: Ctx) -> EmitResult<Assembly> {
    use Expr::*;
    match *expr {
        Number(n) => compile_number(n, ctx),
        Boolean(b) => compile_bool(b, ctx),

        Id(s) => compile_id(s, ctx),
        Let(ident, rhs) => compile_let(ident, rhs, ctx),
        Set(id, expr) => compile_set(id, expr, ctx),
        Call(name, args) => compile_call(name, args, ctx),

        UnOp(op, rhs) => compile_unary(op, rhs, ctx),
        BinOp(op, lhs, rhs) => compile_binary(op, lhs, rhs, ctx),

        If(cond, if_arm, else_arm) => compile_if(cond, if_arm, else_arm, ctx),
        Loop(body) => compile_loop(body, ctx),
        Break(body) => compile_break(body, ctx),
        Block(body) => compile_block(body, ctx),
        Input => compile_input(ctx),
        Vec(_) => todo!(),
        VecGet(_, _) => todo!(),
        Nil => todo!(),
        VecSet(_, _, _) => todo!(),
        VecLen(_) => todo!(),
    }
}


fn compile_fun_def(def: FnDef, ctx: Ctx) -> EmitResult<Assembly> {
    // this function should:
    // 1. check to make sure the function body does not contain any instances of `input`
    // 2. create variable mappings which correspond to the arguments to the function. these
    //     mappings should:
    //     a) be negative, to account for arguments being below RSP on function entry
    //     b) start from the last argument and count down by 1 for each previous argument
    //     c) account for the return pointer in the stack
    // 3. update the passed-down context with said variable mappings
    // 4. compile the body expression using the initial context
    // 5. return the function label and body in an `Assembly`
    let FnDef { name, args, body } = def;

    // start from -2 to skip return pointer
    let mut var_idx = -2;
    let mut var_map = im::HashMap::new();
    for arg in args.into_iter().rev() {
        var_map.insert(arg, var_idx);
        var_idx -= 1;
    }

    let mut res = vec![
        AssemblyLine::Label(format!("snek_fun_{name}")),
    ];
    res.extend(compile_expr(body, ctx.add_vars(var_map))?);
    res.push(AssemblyLine::Instruction(Ret));
    Ok(res)
}


pub fn compile_program(prog: Program) -> EmitResult<Assembly> {
    // this function should:
    // 1. compile the function definitions
    // 2. create a context containing all of the function definitions
    // 3. compile main using this context
    // 4. prepend the compiled function definitions to main
    // 5. return the result of doing so

    let Program { functions, main } = prog;

    let mut fn_list = im::HashMap::new();
    for f in &functions {
        fn_list.insert(f.name.clone(), f.args.clone());
    }

    let ctx = Ctx::new(2, None, im::HashMap::new(), fn_list);
    let defs_compiled: Vec<_> = functions.into_iter()
        .map(|x| compile_fun_def(x, ctx.clone()))
        .collect::<EmitResult<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    let main_compiled = compile_expr(main, ctx)?;

    // put together result
    let mut res = vec![
        AssemblyLine::Label("our_code_starts_here".into()),
        Push(Reg(RBX)).into(),
    ];
    res.extend(main_compiled);
    res.extend([
        Pop(Reg(RBX)).into(),
        Ret.into(),
    ]);
    res.extend(defs_compiled);
    Ok(res)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::e::*;


    #[test]
    fn test_compile_one_arg_call() {
        let input = call("do_thing", vec![num(5)]);
        let ctx = Ctx::new(0, None, im::HashMap::new(),
            im::hashmap!{ "do_thing".into() => vec!["arg".into()]});

        let res = compile_expr(input, ctx).unwrap();
        let expected = [
            Mov(Reg(RAX), Imm(10)),
            Mov(StackIndex(0), Reg(RAX)),
            Sub(Reg(RSP), Imm(8)),
            Call("snek_fun_do_thing".into()),
            Add(Reg(RSP), Imm(8)),
        ].map(line);

        assert_eq!(res, expected);
    }

    #[test]
    fn test_compile_multi_arg() {
        let input = call("hello_world", vec![num(15), ebool(false), num(25)]);
        // this time try a nonzeo stack index
        let ctx = Ctx::new(10, None, im::HashMap::new(), im::hashmap! {
            "hello_world".into() => vec!["a1".into(), "a2".into(), "a3".into()]
        });

        let res = compile_expr(input, ctx).unwrap();
        let expected = [
            Mov(Reg(RAX), Imm(30)),
            Mov(StackIndex(10), Reg(RAX)),
            Mov(Reg(RAX), Imm(1)),
            Mov(StackIndex(11), Reg(RAX)),
            Mov(Reg(RAX), Imm(50)),
            Mov(StackIndex(12), Reg(RAX)),
            Sub(Reg(RSP), Imm(8 * 13)),
            Call("snek_fun_hello_world".into()),
            Add(Reg(RSP), Imm(8 * 13)),
        ].map(line);

        assert_eq!(res, expected);
    }
}
