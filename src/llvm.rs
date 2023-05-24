use std::cell::RefCell;
use std::path::Path;

use crate::ast::*;
use crate::util::*;

use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Linkage;
use inkwell::targets::CodeModel;
use inkwell::targets::FileType;
use inkwell::targets::RelocMode;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::IntMathValue;
#[allow(unused_imports)]
use inkwell::{
    FloatPredicate,
    builder::Builder,
    context::Context as IContext,
    module::Module,
    passes::PassManager,
    types::BasicMetadataTypeEnum,
    targets::{
        Target, TargetMachine,
        InitializationConfig,
    },
    values::{
        IntValue,
        StructValue,
        FloatValue, FunctionValue, PointerValue,
        BasicMetadataValueEnum::IntValue as EIntValue, BasicValue
    },
};

#[derive(Clone)]
#[allow(dead_code)]
struct LoopCtx<'a, 'ctx> {
    pub exit: &'a BasicBlock<'ctx>,
    // use RefCell here for convenience
    // tired of fighting the compiler
    pub phi_incoming: RefCell<Vec<(IntValue<'ctx>, BasicBlock<'ctx>)>>,
}

#[derive(Clone)]
#[allow(dead_code)]
struct Ctx<'a, 'ctx> {
    pub vars: im::HashMap<String, PointerValue<'ctx>>,

    pub arg: Option<PointerValue<'ctx>>,

    // todo: rename this to something more explicit
    pub parent: FunctionValue<'ctx>,
    pub loop_ctx: Option<LoopCtx<'a, 'ctx>>
}

struct EmitVal<'ctx> {
    pub val: IntValue<'ctx>,
    pub has_terminator: bool,
}

impl<'ctx> From<IntValue<'ctx>> for EmitVal<'ctx> {
    fn from(value: IntValue<'ctx>) -> Self {
        Self{
            val: value,
            has_terminator: false,
        }
    }
}


impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn with_loop_ctx(&self, loop_ctx: LoopCtx<'a, 'ctx>) -> Self {
        let Ctx{vars, parent, arg, ..} = self.clone();
        let loop_ctx = Some(loop_ctx);
        Ctx{ vars, parent, loop_ctx, arg }
    }
}

struct Compiler<'a, 'ctx> {
    pub ink_ctx: &'ctx IContext,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    exit_err_fn: FunctionValue<'ctx>,
    print_fn:    FunctionValue<'ctx>,
}

enum Primitive {
    Bool,
    Int,
}

impl ToString for Primitive {
    fn to_string(&self) -> String {
        match self {
            Primitive::Bool => format!("bool"),
            Primitive::Int  => format!("int"),
        }
    }
}

fn make_fn_name(name: &str) -> String {
    format!("\x01{name}")
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(ink_ctx: &'ctx IContext, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> Self {
        use BasicMetadataTypeEnum::IntType;
        let i64_type = ink_ctx.i64_type();

        let one_arg_void_fn_type = ink_ctx.void_type().fn_type(&[IntType(i64_type)], false);

        let exit_err_fn = module.add_function(&make_fn_name("snek_error"),
            one_arg_void_fn_type, Some(Linkage::External));
        let print_fn = module.add_function(&make_fn_name("snek_print"),
            one_arg_void_fn_type, Some(Linkage::External));

        Self { ink_ctx, builder, module, exit_err_fn, print_fn }
    }
    
    fn build_int_compare_const_conditional<'b>(
        &mut self,
        cond: IntValue<'ctx>,
        true_val: i64,
        false_val: i64,
        label_tag: &str,
        ctx: &Ctx<'b, 'ctx>) -> IntValue<'ctx>
    {
        let eq_block = self.ink_ctx
            .append_basic_block(ctx.parent, &format!("{label_tag}_true_block"));
        let neq_block = self.ink_ctx
            .append_basic_block(ctx.parent, &format!("{label_tag}_false_block"));
        let end_block = self.ink_ctx
            .append_basic_block(ctx.parent, &format!("{label_tag}_end_block"));

        self.builder.build_conditional_branch(cond, eq_block, neq_block);

        self.builder.position_at_end(eq_block);
        let true_val = self.ink_ctx.i64_type().const_int(true_val as u64, true);
        self.builder.build_unconditional_branch(end_block);
        let eq_block = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(neq_block);
        self.builder.build_unconditional_branch(end_block);
        let false_val = self.ink_ctx.i64_type().const_int(false_val as u64, true);
        let neq_block = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(end_block);
        let phi = self.builder.build_phi(self.ink_ctx.i64_type(), "{label_tag}_res");
        phi.add_incoming(&[(&true_val, eq_block), (&false_val, neq_block)]);

        phi.as_basic_value().into_int_value()
    }

    /// This function will take a function value, an argument list, a function name, and a
    /// condititional value, and will call the function if the conditional is true using a
    /// conditional branch.
    fn build_conditional_call<'b>(
        &mut self,
        cond: IntValue<'ctx>,
        function_value: FunctionValue<'ctx>,
        function_args: &[BasicMetadataValueEnum<'ctx>],
        function_name: &str,
        label_tag: &str,
        ctx: &Ctx<'b, 'ctx>)
    {
        let then_block = self.ink_ctx.append_basic_block(ctx.parent, &format!("{label_tag}_succeed"));
        let else_block = self.ink_ctx.append_basic_block(ctx.parent, &format!("{label_tag}_fail"));
        let end_block  = self.ink_ctx.append_basic_block(ctx.parent, &format!("{label_tag}_end"));
        self.builder.build_conditional_branch(cond, then_block, else_block);

        self.builder.position_at_end(then_block);
        self.builder
            .build_call(function_value, function_args, function_name);
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(else_block);
        self.builder.build_unconditional_branch(end_block);

        self.builder.position_at_end(end_block);
    }

    // TODO: collapse the build_checked_... family into one function with enum arg
    fn build_checked_add<'b>(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> IntValue<'ctx> {
        let add_overflow = Intrinsic::find("llvm.sadd.with.overflow")
            .expect("Unable to find add_overflow intrinsic");
        let i64_t = BasicTypeEnum::IntType(self.ink_ctx.i64_type());
        let add_fn = add_overflow.get_declaration(self.module, &[i64_t])
            .expect("Unable to  find add_overflow declaration");

        let args = [EIntValue(lhs), EIntValue(rhs)];
        let add_res = self.builder
            .build_call(add_fn, &args, "add_res")
            .try_as_basic_value()
            .expect_left("Unable to make return type from function call")
            .into_struct_value();

        let sum = self.builder
            .build_extract_value(add_res, 0, "overflow_check_sum_tmp")
            .expect("Unable to extract sum from checked add")
            .into_int_value();
        let did_overflow = self.builder
            .build_extract_value(add_res, 1, "overflow_check_tag_tmp")
            .expect("Unable to extract overflow tag from checked add")
            .into_int_value();

        // error function takes 9 for arithmetic overflow
        let call_args = [EIntValue(self.ink_ctx.i64_type().const_int(9, false))];
        self.build_conditional_call(did_overflow, self.exit_err_fn, &call_args, "snek_error", "checked_add", ctx);
        
        sum
    }

    fn build_checked_sub<'b>(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> IntValue<'ctx> {
        let add_overflow = Intrinsic::find("llvm.ssub.with.overflow")
            .expect("Unable to find add_overflow intrinsic");
        let i64_t = BasicTypeEnum::IntType(self.ink_ctx.i64_type());
        let add_fn = add_overflow.get_declaration(self.module, &[i64_t])
            .expect("Unable to  find add_overflow declaration");

        let args = [EIntValue(lhs), EIntValue(rhs)];
        let add_res = self.builder
            .build_call(add_fn, &args, "add_res")
            .try_as_basic_value()
            .expect_left("Unable to make return type from function call")
            .into_struct_value();

        let sum = self.builder
            .build_extract_value(add_res, 0, "overflow_check_sum_tmp")
            .expect("Unable to extract sum from checked add")
            .into_int_value();
        let did_overflow = self.builder
            .build_extract_value(add_res, 1, "overflow_check_tag_tmp")
            .expect("Unable to extract overflow tag from checked add")
            .into_int_value();

        // error function takes 9 for arithmetic overflow
        let call_args = [EIntValue(self.ink_ctx.i64_type().const_int(9, false))];
        self.build_conditional_call(did_overflow, self.exit_err_fn, &call_args, "snek_error", "checked_add", ctx);
        
        sum
    }

    fn build_checked_mul<'b>(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> IntValue<'ctx> {
        let add_overflow = Intrinsic::find("llvm.smul.with.overflow")
            .expect("Unable to find add_overflow intrinsic");
        let i64_t = BasicTypeEnum::IntType(self.ink_ctx.i64_type());
        let add_fn = add_overflow.get_declaration(self.module, &[i64_t])
            .expect("Unable to  find add_overflow declaration");

        let args = [EIntValue(lhs), EIntValue(rhs)];
        let add_res = self.builder
            .build_call(add_fn, &args, "add_res")
            .try_as_basic_value()
            .expect_left("Unable to make return type from function call")
            .into_struct_value();

        let sum = self.builder
            .build_extract_value(add_res, 0, "overflow_check_sum_tmp")
            .expect("Unable to extract sum from checked add")
            .into_int_value();
        let did_overflow = self.builder
            .build_extract_value(add_res, 1, "overflow_check_tag_tmp")
            .expect("Unable to extract overflow tag from checked add")
            .into_int_value();

        // error function takes 9 for arithmetic overflow
        let call_args = [EIntValue(self.ink_ctx.i64_type().const_int(9, false))];
        self.build_conditional_call(did_overflow, self.exit_err_fn, &call_args, "snek_error", "checked_add", ctx);
        
        sum
    }

    fn compile_unary_op<'b>(&mut self, op: &UOper, operand: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> EmitResult<IntValue<'ctx>> {
        // this is literally the numeric value "one"
        // not a snek tagged integer one
        let one_lit = self.ink_ctx.i64_type().const_int(1, false);
        // this is the tagged Snek value one
        let one_snek = self.ink_ctx.i64_type().const_int(2, false);

        Ok(match op {
            UOper::Add1 => {
                self.add_type_check(operand, Primitive::Int, ctx);
                self.build_checked_add(operand, one_snek, ctx)
            },
            UOper::Sub1 => {
                self.add_type_check(operand, Primitive::Int, ctx);
                self.build_checked_sub(operand, one_snek, ctx)
            },
            UOper::IsNum => {
                // efficient strategy: mask out all but the lower bit and invert
                // then shift left and add one to form boolean result
                let masked = self.builder
                    .build_and(operand, one_lit, "isnum_and_tmp");
                // invert with xor because can't find not
                let inverted = self.builder
                    .build_xor(masked, one_lit, "isnum_xor_tmp");
                let shifted = self.builder
                    .build_left_shift(inverted, one_lit, "isnum_shift_tmp");
                self.builder
                    .build_int_add(shifted, one_lit, "typecheck_result")
            },
            UOper::IsBool => {
                // efficient strategy: mask out all but the lower bit and shift
                // left
                let masked = self.builder
                    .build_and(operand, one_lit, "isbool_and_tmp");
                let shifted = self.builder
                    .build_left_shift(masked, one_lit, "isbool_shift_tmp");
                self.builder
                    .build_int_add(shifted, one_lit, "isbool_add")
            },
            UOper::Print => {
                self.builder
                    .build_call(self.print_fn, &[EIntValue(operand)], "print_tmp");
                operand
            },
        })
    }

    fn add_type_check<'b>(&mut self, operand: IntValue<'ctx>, the_type: Primitive, ctx: &Ctx<'b, 'ctx>) {
        let one = self.ink_ctx.i64_type().const_int(1, false);
        let masked = self.builder
            .build_and(operand, one, "type_check_and");

        // `cond` will be 1 if this is the type we think it is
        let cond = match the_type {
            Primitive::Bool => self.builder
                .build_int_compare(IntPredicate::NE, masked, one, "type_check_cond"),
            Primitive::Int  => self.builder
                .build_int_compare(IntPredicate::EQ, masked, one, "type_check_cond"),
        };

        // in the else block, we will branch to the respective error function
        let error_arg = match the_type {
            // 8 for "invalid argument - expected bool, not number",
            Primitive::Bool => self.ink_ctx.i64_type()
                .const_int(8, false),
            // 7 for "invalid argument - expected number, not bool",
            Primitive::Int  => self.ink_ctx.i64_type()
                .const_int(7, false),
        };
        self.build_conditional_call(cond, self.exit_err_fn, &[EIntValue(error_arg)], "snek_error", "type_check", ctx);
    }

    fn add_same_type_check<'b>(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) {
        let one = self.ink_ctx.i64_type()
            .const_int(1, false);

        let masked = self.builder
            .build_xor(lhs, rhs, "same_type_check_xor");
        let masked = self.builder
            .build_and(masked, one, "same_type_check_and");
        let cond = self.builder
            .build_int_compare(IntPredicate::NE, masked, one, "same_type_check_cond");

        let error_arg = self.ink_ctx.i64_type()
            .const_int(10, false);
        self.build_conditional_call(cond, self.exit_err_fn, &[EIntValue(error_arg)], "snek_error", "same_type_check", ctx);
    }

    fn compile_binary_op<'b>(&mut self, op: &BOper, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> EmitResult<IntValue<'ctx>> {
        const TRUE:  i64 = 3;
        const FALSE: i64 = 1;

        self.add_type_check(lhs, Primitive::Int, ctx);
        self.add_type_check(rhs, Primitive::Int, ctx);

        Ok(match op {
            BOper::Plus => self.build_checked_add(lhs, rhs, ctx),
            BOper::Minus => self.build_checked_sub(lhs, rhs, ctx),
            BOper::Times => {
                let two_lit = self.ink_ctx
                    .i64_type()
                    .const_int(2, false);
                let rhs_tmp = self.builder.build_int_signed_div(rhs, two_lit, "binop_mul_rhs_tmp");
                self.build_checked_mul(lhs, rhs_tmp, ctx)
            },
            BOper::Equal => {
                let cond = self.builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq_tmp");
                self.build_int_compare_const_conditional(cond, TRUE, FALSE, "binop_eq", ctx)
            },
            BOper::Greater => {
                let cond = self.builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "binop_gt_tmp");
                
                self.build_int_compare_const_conditional(cond, TRUE, FALSE, "binop_gt", ctx)
            },
            BOper::GreaterEqual => {
                let cond = self.builder
                    .build_int_compare(IntPredicate::SGE, lhs, rhs, "binop_ge");

                self.build_int_compare_const_conditional(cond, TRUE, FALSE, "binop_ge", ctx)
            },
            BOper::Less => {
                let cond = self.builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "binop_lt");

                self.build_int_compare_const_conditional(cond, TRUE, FALSE, "binop_lt", ctx)
            },
            BOper::LessEqual => {
                let cond = self.builder
                    .build_int_compare(IntPredicate::SLE, lhs, rhs, "binop_le");

                self.build_int_compare_const_conditional(cond, TRUE, FALSE, "binop_le", ctx)
            },
        })
    }

    pub fn compile_expr<'b>(&mut self, expr: &Expr, ctx: &Ctx<'b, 'ctx>) -> EmitResult<EmitVal<'ctx>> {
        match expr {
            Expr::Number(n) => {
                // should we use i64 and shifts to represent a Number
                // or should we have an llvm "value" struct which separates the tag and value as
                // fields
                let val = n << 1;
                Ok(self.ink_ctx.i64_type()
                    .const_int(val as u64, true)
                    .into())
            },

            Expr::Boolean(b) => {
                let val = match b {
                    true  => 3,
                    false => 1,
                };
                Ok(self.ink_ctx.i64_type()
                    .const_int(val, false)
                    .into())
            },
            Expr::Id(name) => {
                let var = ctx.vars.get(name)
                    .ok_or(format!("Unbound variable identifier {name}"))?;
                let i64_t = self.ink_ctx.i64_type();

                Ok(self.builder
                    .build_load(i64_t, *var, &name)
                    .into_int_value()
                    .into())
            },

            Expr::Let(vars, in_expr) => {
                let mut ctx = ctx.clone();
                for (ident, initializer) in vars {
                    let initializer = self.compile_expr(initializer, &ctx)?;
                    if initializer.has_terminator {
                        panic!("TODO: Handle terminator inside initializer");
                    }
                    let initializer = initializer.val;

                    let alloc = self.create_entry_block_alloca(ident, ctx.parent);
                    let _ = self.builder
                        .build_store(alloc, initializer);

                    ctx.vars = ctx.vars.update(ident.clone(), alloc)
                }
                self.compile_expr(in_expr, &ctx)
            },

            Expr::Set(name, expr) => {
                let Some(dest) = ctx.vars.get(name) else {
                    return Err(format!("Unbound variable identifier {name}"));
                };

                let val = self.compile_expr(expr, ctx)?;
                if val.has_terminator {
                    panic!("TODO: handle terminator inside initializer");
                }
                self.builder
                    .build_store(*dest, val.val);
                
                Ok(val)
            },

            Expr::UnOp(op, expr) => {
                let res = self.compile_expr(expr, ctx)?;
                if res.has_terminator {
                    panic!("TODO: handle terminator inside UnOp")
                }
                self.compile_unary_op(op, res.val, ctx)
                    .map(|v| v.into())
            },

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs, ctx)?;
                let rhs = self.compile_expr(rhs, ctx)?;
                if lhs.has_terminator || rhs.has_terminator {
                    panic!("TODO: handle terminator inside BinOp")
                }
                self.compile_binary_op(op, lhs.val, rhs.val, ctx)
                    .map(|v| v.into())
            },

            Expr::If(cond_expr, then_expr, else_expr) => {
                let false_ = self.ink_ctx.i64_type().const_int(1, false);
                let cond_res  = self.compile_expr(cond_expr, ctx)?;
                if  cond_res.has_terminator {
                    panic!("TODO: handle terminator inside if condition")
                }
                let cond = self.builder.build_int_compare(IntPredicate::NE, cond_res.val, false_, "if_cond");

                let then_block = self.ink_ctx.append_basic_block(ctx.parent, "if_then");
                let else_block = self.ink_ctx.append_basic_block(ctx.parent, "if_else");
                let end_block  = self.ink_ctx.append_basic_block(ctx.parent, "if_end");
                self.builder.build_conditional_branch(cond, then_block, else_block);

                self.builder.position_at_end(end_block);
                let phi = self.builder.build_phi(self.ink_ctx.i64_type(), "if_res");
                // phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);

                self.builder.position_at_end(then_block);
                let then_val  = self.compile_expr(then_expr, ctx)?;
                if !then_val.has_terminator {
                    self.builder.build_unconditional_branch(end_block);
                    let then_block = self.builder.get_insert_block()
                        .expect("Unable to get insert then block");
                    phi.add_incoming(&[(&then_val.val, then_block)])
                }

                self.builder.position_at_end(else_block);
                let else_val = self.compile_expr(else_expr, ctx)?;
                if !else_val.has_terminator {
                    self.builder.build_unconditional_branch(end_block);
                    let else_block = self.builder.get_insert_block()
                        .expect("Unable to get insert else block");
                    phi.add_incoming(&[(&else_val.val, else_block)]);
                }
                self.builder.position_at_end(end_block);
                
                Ok(phi.as_basic_value().into_int_value().into())
            }

            Expr::Loop(body) => {
                let loop_block = self.ink_ctx.append_basic_block(ctx.parent, "loop");
                let end_block  = self.ink_ctx.append_basic_block(ctx.parent, "loop_exit");

                // terminate current block by branching to loop
                self.builder
                    .build_unconditional_branch(loop_block);

                self.builder.position_at_end(loop_block);
                let phi_incoming = Vec::new();
                let body_ctx = ctx.with_loop_ctx(LoopCtx{ exit: &end_block, phi_incoming: RefCell::new(phi_incoming) });
                // discard result of body, since we loop yields break expression
                let _  = self.compile_expr(body, &body_ctx)?;
                self.builder.build_unconditional_branch(loop_block);

                let Ctx{loop_ctx: Some(LoopCtx { phi_incoming, .. }), ..} = body_ctx else {
                    panic!("this should never happen");
                };
                self.builder.position_at_end(end_block);
                let phi = self.builder.build_phi(self.ink_ctx.i64_type(), "loop_res");

                let phi_incoming: Vec<(IntValue<'_>, BasicBlock<'_>)> = phi_incoming.take();
                if phi_incoming.is_empty() {
                    return Err(format!("Loop never breaks"));
                }
                phi.add_incoming(&phi_incoming.iter()
                    .map(|(v, b)| (v as &dyn BasicValue<'_>, *b))
                    .collect::<Vec<_>>());

                Ok(phi.as_basic_value().into_int_value().into())
            },

            Expr::Break(expr) => {
                let Some(LoopCtx{ exit, phi_incoming }) = ctx.loop_ctx.as_ref() else {
                    return Err("Found break outside of loop".into());
                };

                let expr_val = self.compile_expr(expr, ctx)?;
                phi_incoming
                    .borrow_mut()
                    .push((expr_val.val, self.builder.get_insert_block().unwrap()));
                self.builder
                    .build_unconditional_branch(**exit);

                let val = self.ink_ctx
                    .i64_type()
                    .const_zero();
                // cheap hack: just yield int value zero
                Ok(EmitVal { val, has_terminator: true })
            },

            Expr::Block(stmts) => stmts.into_iter()
                    .map(|x| self.compile_expr(x, ctx))
                    .take_while(|r| r.is_ok())
                    .last()
                    .ok_or("Error: found block with no statements".to_owned())?,

            Expr::Call(name, args) => {
                let mangled_name = make_fn_name(name);
                let Some(fn_val) = self.module.get_function(&mangled_name) else {
                    return Err(format!("Function {name} is undefined"));
                };
                
                let args: Vec<_> = args.into_iter()
                    .map(|x| self.compile_expr(x, ctx)
                        .map(|y| EIntValue(y.val)))
                    .collect::<EmitResult<Vec<_>>>()?;
                
                Ok(self.builder
                    .build_call(fn_val, args.as_slice(), name.as_str())
                    .try_as_basic_value()
                    .expect_left("Unable to make return type from function call")
                    .into_int_value()
                    .into())
            },

            Expr::Input => {
                let var = ctx.vars.get("input")
                    .expect("Unable to find 'input' in allowed context. This should never happen.");
                let i64_t = self.ink_ctx.i64_type();

                Ok(self.builder
                    .build_load(i64_t, *var, "input")
                    .into_int_value()
                    .into())
            },
        }
    }

    // this function borrowed from inkwell kaleidoscope tutorial
    fn create_entry_block_alloca(&self, name: &str, target_fun: FunctionValue<'ctx>) -> PointerValue<'ctx> {
        let builder = self.ink_ctx.create_builder();

        let entry = target_fun.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.ink_ctx.i64_type(), name)
    }

    pub fn compile_proto(&mut self, fun: &FnDef, linkage:  Option<Linkage>) -> FunctionValue<'ctx> {
        use BasicMetadataTypeEnum::IntType;
        let param_list: Vec<_> = fun.args.iter()
            .map(|_| IntType(self.ink_ctx.i64_type()))
            .collect();
        let fn_type = self.ink_ctx.i64_type()
            .fn_type(param_list.as_slice(), false);

        let thing = self.module.add_function(&make_fn_name(&fun.name), fn_type, linkage);
        thing
    }

    pub fn compile_fn(&mut self, fun: FnDef) -> EmitResult<FunctionValue<'ctx>> {
        let fn_val = self.module.get_function(&make_fn_name(&fun.name))
            .expect("Attempted to compile fn without prototype -- this should never happen");

        let entry = self.ink_ctx.append_basic_block(fn_val, &format!("{}_entry", fun.name));
        self.builder.position_at_end(entry);

        let vars: im::HashMap<_, _> = fn_val
            .get_param_iter()
            .zip(fun.args)
            .map(|(arg, name)| {
                arg.set_name(&name);

                let alloc = self.create_entry_block_alloca(&name, fn_val);
                self.builder.build_store(alloc, arg);

                (name.clone(), alloc)
            })
            .collect();
        

        let fn_ctx = Ctx{
            vars,
            parent:   fn_val,
            loop_ctx: None,
            arg:      None,
        };
        let body_val = self.compile_expr(fun.body.as_ref(), &fn_ctx)?;

        // we can assume that a break hasn't terminated the current block
        // since we are necesarily not inside a loop
        self.builder.build_return(Some(&body_val.val));
        fn_val.print_to_stderr();

        match fn_val.verify(true) {
            true  => Ok(fn_val),
            false => Err("Failed to verify function".into()),
        }
    }
}

pub fn compile_program(prog: Program, output_path: &str, opt_level: OptimizationLevel, file_type: FileType) -> EmitResult<()> {
    if cfg!(target_arch = "aarch64") {
        Target::initialize_aarch64(&InitializationConfig::default());
    }
    else if cfg!(target_arch = "x86") {
        Target::initialize_x86(&InitializationConfig::default());
    }
    else {
        panic!("Unsupported architecture");
    }

    let reloc_mode = RelocMode::Default;
    let code_model = CodeModel::Default;

    println!("target triple is {}", TargetMachine::get_default_triple());
    println!("normalized target triple is {}", TargetMachine::normalize_triple(&TargetMachine::get_default_triple()));

    let mut target = Target::get_first().unwrap();
    println!("first target is {}: {}", target.get_name().to_str().unwrap(),
        target.get_description().to_str().unwrap());
    let mut i = 2;
    loop {
        let next = target.get_next();
        if next.is_none() {
            break;
        }

        target = next.unwrap();
        println!("target {i} is {}: {}", target.get_name().to_str().unwrap(),
            target.get_description().to_str().unwrap());
        i += 1;
    }


    let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
    // let target = Target::from_name("arm64").unwrap();
    let target_machine = target.create_target_machine(
        &TargetMachine::get_default_triple(),
        "generic", "", opt_level, reloc_mode, code_model
    ).unwrap();
    println!("picked target {}", target_machine.get_triple());

    let ink_ctx = IContext::create();
    let module = ink_ctx.create_module("snek_root");
    let builder = ink_ctx.create_builder();
    
    // compile functions
    let mut compiler = Compiler::new(&ink_ctx, &builder, &module);
    let Program { functions, main } = prog;

    // first compile prototypes
    let linkage = Linkage::DLLExport;
    for function in &functions {
        compiler.compile_proto(function, Some(linkage));
    }

    let _function_vals: Vec<FunctionValue> = functions.into_iter()
        .map(|f| compiler.compile_fn(f))
        .collect::<EmitResult<_>>()?;

    // TODO: fix cheap hack here
    let main_fn = FnDef{
        name: "our_code_starts_here".into(),
        // TODO: fix this cheap hack as well, input is just an argument to main
        args: vec!["input".into()],
        body: main,
    };

    let _main_proto = compiler.compile_proto(&main_fn, Some(linkage));
    let fun = compiler.compile_fn(main_fn)?;

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    // fpm.add_gvn_pass();
    // fpm.add_cfg_simplification_pass();
    // fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    // fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&TargetMachine::get_default_triple());

    // for function in function_vals {
    //     fpm.run_on(&function);
    // }
    // fpm.run_on(&main);

    let path = Path::new(output_path);
    target_machine
        .write_to_file(&module, file_type, path)
        .map_err(|err| format!("Failed to write to assembly file {output_path}: {}", err))
}
