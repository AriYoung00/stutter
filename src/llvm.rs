use std::path::Path;

use crate::ast::*;
use crate::util::*;

use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::targets::CodeModel;
use inkwell::targets::RelocMode;
use inkwell::values::PhiValue;
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
        BasicMetadataValueEnum,
    },
};

#[derive(Clone)]
struct LoopCtx<'a> {
    pub exit: &'a BasicBlock<'a>,
    pub phi:  &'a PhiValue<'a>,
}

#[derive(Clone)]
struct Ctx<'a, 'ctx> {
    pub vars: im::HashMap<String, PointerValue<'ctx>>,
    pub fns:  im::HashMap<String, Vec<String>>,

    pub parent: FunctionValue<'ctx>,
    pub loop_ctx: Option<LoopCtx<'a>>
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn with_loop_ctx(&self, loop_ctx: LoopCtx<'a>) -> Self {
        let Ctx{vars, fns, parent, ..} = self.clone();
        let loop_ctx = Some(loop_ctx);
        Ctx{ vars, fns, parent, loop_ctx }
    }
}

struct Compiler<'a, 'ctx> {
    pub ink_ctx: &'ctx IContext,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn compile_unary_op(&mut self, op: &UOper, operand: IntValue<'ctx>) -> EmitResult<IntValue<'ctx>> {
        let one = self.ink_ctx.i32_type().const_int(1, false);
        Ok(match op {
            UOper::Add1 => self.builder.build_int_add(operand, one, "add_tmp"),
            UOper::Sub1 => self.builder.build_int_sub(operand, one, "sub_tmp"),
            UOper::IsNum => {
                // efficient strategy: mask out all but the lower bit and invert
                // then shift left and add one to form boolean result
                let masked = self.builder
                    .build_and(operand, one.clone(), "isnum_and_tmp");
                // invert with xor because can't find not
                let inverted = self.builder
                    .build_xor(masked, one.clone(), "isnum_xor_tmp");
                let shifted = self.builder
                    .build_left_shift(inverted, one.clone(), "isnum_shift_tmp");
                self.builder
                    .build_int_add(shifted, one.clone(), "typecheck_result")
            },
            UOper::IsBool => {
                // efficient strategy: mask out all but the lower bit and shift
                // left
                let masked = self.builder
                    .build_and(operand, one.clone(), "isbool_and_tmp");
                let shifted = self.builder
                    .build_left_shift(masked, one.clone(), "isbool_shift_tmp");
                self.builder
                    .build_int_add(shifted, one.clone(), "isbool_add")
            },
            UOper::Print => todo!(),
        })
    }

    fn add_type_check(&mut self, operand: IntValue<'ctx>) -> EmitResult<IntValue<'ctx>> {
        let one = self.ink_ctx.i32_type().const_int(1, false);
        let masked = self.builder
            .build_and(operand, one, "type_check_and");
        // todo: figure our how to consume input operand for type check but also return same value
        // can we just clone these?
        todo!()
    }

    fn compile_binary_op(&mut self, op: &BOper, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> EmitResult<IntValue<'ctx>> {
        Ok(match op {
            BOper::Plus => self.builder.build_int_add(lhs, rhs, "bop_add_tmp"),
            BOper::Minus => self.builder.build_int_sub(lhs, rhs, "bop_sub_tmp"),
            BOper::Times => self.builder.build_int_mul(lhs, rhs, "bop_mul_tmp"),
            BOper::Equal => todo!(),
            BOper::Greater => todo!(),
            BOper::GreaterEqual => todo!(),
            BOper::Less => todo!(),
            BOper::LessEqual => todo!(),
        })
    }

    pub fn compile_expr<'b>(&mut self, expr: &Expr, ctx: &Ctx<'b, 'ctx>) -> EmitResult<IntValue<'ctx>> {
        match expr {
            Expr::Number(n) => {
                // should we use i64 and shifts to represent a Number
                // or should we have an llvm "value" struct which separates the tag and value as
                // fields
                let val = n << 1;
                Ok(self.ink_ctx.i64_type()
                    .const_int(val as u64, true))
            },

            Expr::Boolean(b) => {
                let val = match b {
                    true  => 3,
                    false => 1,
                };
                Ok(self.ink_ctx.i64_type()
                    .const_int(val, false))
            },

            Expr::Id(name) => {
                let var = ctx.vars.get(name)
                    .ok_or(format!("Unbound variable identifier {name}"))?;
                let i64_t = self.ink_ctx.i64_type();
                
                Ok(self.builder
                    .build_load(i64_t, *var, &name)
                    .into_int_value())
            },

            Expr::Let(vars, in_expr) => {
                let mut ctx = ctx.clone();
                for (ident, initializer) in vars {
                    let initializer = self.compile_expr(initializer, &ctx)?;

                    let alloc = self.builder
                        .build_alloca(self.ink_ctx.i64_type(), ident);
                    let _ = self.builder
                        .build_store(alloc, initializer);

                    ctx.vars = ctx.vars.update(ident.clone(), alloc)
                }
                self.compile_expr(in_expr, &ctx)
            },
            
            Expr::UnOp(op, expr) => {
                let res = self.compile_expr(expr, ctx)?;
                self.compile_unary_op(op, res)
            },

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs, ctx)?;
                let rhs = self.compile_expr(rhs, ctx)?;
                self.compile_binary_op(op, lhs, rhs)
            },

            Expr::If(cond_expr, then_expr, else_expr) => {
                let false_ = self.ink_ctx.i32_type().const_int(1, false);
                let cond_res  = self.compile_expr(cond_expr, ctx)?;
                let cond = self.builder.build_int_compare(IntPredicate::EQ, cond_res, false_, "if_cond");

                let then_block = self.ink_ctx.append_basic_block(ctx.parent, "if_then");
                let else_block = self.ink_ctx.append_basic_block(ctx.parent, "if_else");
                let end_block  = self.ink_ctx.append_basic_block(ctx.parent, "if_end");
                self.builder.build_conditional_branch(cond, then_block, else_block);
                
                self.builder.position_at_end(then_block);
                let then_val  = self.compile_expr(then_expr, ctx)?;
                let then_block = self.builder.get_insert_block()
                    .expect("Unable to get insert then block");

                self.builder.position_at_end(else_block);
                let else_val = self.compile_expr(else_expr, ctx)?;
                // TODO: remove this and see what happens?
                let else_block = self.builder.get_insert_block()
                    .expect("Unable to get insert else block");

                self.builder.position_at_end(end_block);

                let phi = self.builder.build_phi(self.ink_ctx.i32_type(), "if_res");
                phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);

                Ok(phi.as_basic_value().into_int_value())
            }
            
            Expr::Loop(body) => {
                let loop_block = self.ink_ctx.append_basic_block(ctx.parent, "loop");
                let end_block  = self.ink_ctx.append_basic_block(ctx.parent, "loop_exit");
                self.builder.build_unconditional_branch(loop_block);

                // first build phi so we can include it in context
                // this allows us to properly set incoming branches to phi from break expressions
                self.builder.position_at_end(end_block);
                let phi = self.builder.build_phi(self.ink_ctx.i32_type(), "loop_res");

                self.builder.position_at_end(loop_block);
                let body_ctx = ctx.with_loop_ctx(LoopCtx{ exit: &end_block, phi: &phi });
                // discard result of body, since we loop yields break expression
                let _  = self.compile_expr(body, &body_ctx)?;
                if phi.count_incoming() == 0 {
                    return Err(format!("Loop never breaks"));
                }
                
                Ok(phi.as_basic_value().into_int_value())
            },
            
            Expr::Break(expr) => {
                let Some(LoopCtx{ exit, phi }) = ctx.loop_ctx else {
                    return Err("Found break outside of loop".into());
                };

                let expr_val = self.compile_expr(expr, ctx)?;
                todo!()
            },
            
            Expr::Set(_, _) => todo!(),
            
            Expr::Block(_) => todo!(),
            
            Expr::Call(_, _) => todo!(),
            
            Expr::Input => todo!(),
        }
    }

    pub fn compile_fn(&mut self, fun: FnDef) -> EmitResult<FunctionValue<'ctx>> {
        todo!()
    }
}

    /*
     *  auto Filename = "output.o";
        std::error_code EC;
        raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

        if (EC) {
          errs() << "Could not open file: " << EC.message();
          return 1;
        }

        legacy::PassManager pass;
        auto FileType = CGFT_ObjectFile;

        if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
          errs() << "TargetMachine can't emit a file of this type";
          return 1;
        }

        pass.run(*TheModule);
        dest.flush();
     */
pub fn compile_program(_prog: Program, _file: &str, opt_level: OptimizationLevel) -> EmitResult<()> {
    if cfg!(target_arch = "aarch64") {
        Target::initialize_arm(&InitializationConfig::default());
    }
    else if cfg!(target_arch = "x86") {
        Target::initialize_x86(&InitializationConfig::default());
    }
    else {
        panic!("Unsupported architecture");
    }

    let path = Path::new(_file);
    let reloc_mode = RelocMode::Default;
    let code_model = CodeModel::Default;
    let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
    let target_machine = target.create_target_machine(
        &TargetMachine::get_default_triple(),
        "generic", "", opt_level, reloc_mode, code_model
    ).unwrap();

    let ink_ctx = IContext::create();
    let module = ink_ctx.create_module("snek_root");
    let builder = ink_ctx.create_builder();

    let compiler = Compiler {
        builder: &builder,
        ink_ctx: &ink_ctx,
        module: &module,
    };


    todo!("add pass manager and run on all functions after build")
}
