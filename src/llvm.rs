use std::cell::RefCell;
use std::path::Path;

use crate::ast::*;
use crate::util::*;

use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::targets::CodeModel;
use inkwell::targets::FileType;
use inkwell::targets::RelocMode;
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
        BasicMetadataValueEnum, BasicValue
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
    pub fns:  im::HashMap<String, Vec<String>>,

    // todo: rename this to something more explicit
    pub parent: FunctionValue<'ctx>,
    pub loop_ctx: Option<LoopCtx<'a, 'ctx>>
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn with_loop_ctx(&self, loop_ctx: LoopCtx<'a, 'ctx>) -> Self {
        let Ctx{vars, fns, parent, ..} = self.clone();
        let loop_ctx = Some(loop_ctx);
        Ctx{ vars, fns, parent, loop_ctx }
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

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(ink_ctx: &'ctx IContext, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> Self {
        use BasicMetadataTypeEnum::IntType;
        let i64_type = ink_ctx.i64_type();

        let one_arg_void_fn_type = ink_ctx.void_type().fn_type(&[IntType(i64_type)], false);

        let exit_err_fn = module.add_function("snek_error", one_arg_void_fn_type, Some(Linkage::External));
        let print_fn = module.add_function("snek_print", one_arg_void_fn_type, Some(Linkage::External));

        Self { ink_ctx, builder, module, exit_err_fn, print_fn }
    }

    fn compile_unary_op(&mut self, op: &UOper, operand: IntValue<'ctx>) -> EmitResult<IntValue<'ctx>> {
        let one = self.ink_ctx.i64_type().const_int(1, false);
        Ok(match op {
            UOper::Add1 => self.builder.build_int_add(operand, one, "add_tmp"),
            UOper::Sub1 => self.builder.build_int_sub(operand, one, "sub_tmp"),
            UOper::IsNum => {
                // efficient strategy: mask out all but the lower bit and invert
                // then shift left and add one to form boolean result
                let masked = self.builder
                    .build_and(operand, one, "isnum_and_tmp");
                // invert with xor because can't find not
                let inverted = self.builder
                    .build_xor(masked, one, "isnum_xor_tmp");
                let shifted = self.builder
                    .build_left_shift(inverted, one, "isnum_shift_tmp");
                self.builder
                    .build_int_add(shifted, one, "typecheck_result")
            },
            UOper::IsBool => {
                // efficient strategy: mask out all but the lower bit and shift
                // left
                let masked = self.builder
                    .build_and(operand, one, "isbool_and_tmp");
                let shifted = self.builder
                    .build_left_shift(masked, one, "isbool_shift_tmp");
                self.builder
                    .build_int_add(shifted, one, "isbool_add")
            },
            UOper::Print => todo!(),
        })
    }

    fn add_type_check(&mut self, operand: IntValue<'ctx>, the_type: Primitive) {
        let one = self.ink_ctx.i64_type().const_int(1, false);
        let masked = self.builder
            .build_and(operand, one, "type_check_and");

    }

    fn add_same_type_check(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> (IntValue<'ctx>, IntValue<'ctx>) {
        // todo: implement this
        (lhs, rhs)
    }

    fn compile_binary_op<'b>(&mut self, op: &BOper, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> EmitResult<IntValue<'ctx>> {
        Ok(match op {
            BOper::Plus => self.builder.build_int_add(lhs, rhs, "bop_add_tmp"),
            BOper::Minus => self.builder.build_int_sub(lhs, rhs, "bop_sub_tmp"),
            BOper::Times => self.builder.build_int_mul(lhs, rhs, "bop_mul_tmp"),
            BOper::Equal => {
                let (lhs, rhs) = self.add_same_type_check(lhs, rhs);

                let cond = self.builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq_tmp");

                let eq_block = self.ink_ctx.append_basic_block(ctx.parent, "eq_iseq_block");
                let neq_block  = self.ink_ctx.append_basic_block(ctx.parent, "eq_neq_block");
                self.builder.build_conditional_branch(cond, eq_block, neq_block);

                self.builder.position_at_end(eq_block);
                let true_val = self.ink_ctx.i64_type().const_int(3, false);
                // TODO: maybe comment this?
                let eq_block = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(neq_block);
                let false_val = self.ink_ctx.i64_type().const_int(1, false);
                // TODO: maybe comment this?
                let neq_block = self.builder.get_insert_block().unwrap();

                let phi = self.builder.build_phi(self.ink_ctx.i64_type(), "eq_res");
                phi.add_incoming(&[(&true_val, eq_block), (&false_val, neq_block)]);

                phi.as_basic_value().into_int_value()
            },
            BOper::Greater => {
                self.add_type_check(lhs, Primitive::Int);
                self.add_type_check(rhs, Primitive::Int);
                self.builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "sgt_tmp");
                todo!()
            },
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

                    let alloc = self.create_entry_block_alloca(ident, ctx.parent);
                    let _ = self.builder
                        .build_store(alloc, initializer);

                    ctx.vars = ctx.vars.update(ident.clone(), alloc)
                }
                self.compile_expr(in_expr, &ctx)
            },

            Expr::Set(_, _) => todo!(),

            Expr::UnOp(op, expr) => {
                let res = self.compile_expr(expr, ctx)?;
                self.compile_unary_op(op, res)
            },

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs, ctx)?;
                let rhs = self.compile_expr(rhs, ctx)?;
                self.compile_binary_op(op, lhs, rhs, ctx)
            },

            Expr::If(cond_expr, then_expr, else_expr) => {
                let false_ = self.ink_ctx.i64_type().const_int(1, false);
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

                let phi = self.builder.build_phi(self.ink_ctx.i64_type(), "if_res");
                phi.add_incoming(&[(&then_val, then_block), (&else_val, else_block)]);

                Ok(phi.as_basic_value().into_int_value())
            }

            Expr::Loop(body) => {
                let loop_block = self.ink_ctx.append_basic_block(ctx.parent, "loop");
                let end_block  = self.ink_ctx.append_basic_block(ctx.parent, "loop_exit");
                self.builder.build_unconditional_branch(loop_block);

                self.builder.position_at_end(loop_block);
                let phi_incoming = Vec::new();
                let body_ctx = ctx.with_loop_ctx(LoopCtx{ exit: &end_block, phi_incoming: RefCell::new(phi_incoming) });
                // discard result of body, since we loop yields break expression
                let _  = self.compile_expr(body, &body_ctx)?;

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

                Ok(phi.as_basic_value().into_int_value())
            },

            Expr::Break(expr) => {
                let Some(LoopCtx{ exit, phi_incoming }) = ctx.loop_ctx.as_ref() else {
                    return Err("Found break outside of loop".into());
                };

                let expr_val = self.compile_expr(expr, ctx)?;
                phi_incoming
                    .borrow_mut()
                    .push((expr_val, *exit.clone()));

                // cheap hack: just yield int value zero
                Ok(self.ink_ctx
                    .i64_type()
                    .const_zero())
            },

            Expr::Block(_) => todo!(),

            Expr::Call(_, _) => todo!(),

            Expr::Input => todo!(),
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

    pub fn compile_fn(&mut self, fun: FnDef, fns:  &im::HashMap<String, Vec<String>>,) -> 
      EmitResult<FunctionValue<'ctx>> {
        // strategy: create function value
        // compile function
        use BasicMetadataTypeEnum::IntType;
        let param_list: Vec<_> = fun.args.iter()
            .map(|_| IntType(self.ink_ctx.i64_type()))
            .collect();
        let fn_type = self.ink_ctx.i64_type()
            .fn_type(param_list.as_slice(), false);
        let fn_val = self.module.add_function(&fun.name, fn_type, None);

        let vars: im::HashMap<_, _> = fn_val
            .get_param_iter()
            .zip(fun.args)
            .map(|(arg, name)| {
                arg.set_name(&name);

                let alloc = self.create_entry_block_alloca(&name, fn_val);
                self.builder.build_store(alloc, arg);

                (name.clone(), self.create_entry_block_alloca(&name, fn_val))
            })
            .collect();
        

        let fn_ctx = Ctx{
            vars,
            fns: fns.clone(),
            parent: fn_val,
            loop_ctx: None,
        };
        let body_val = self.compile_expr(fun.body.as_ref(), &fn_ctx)?;
        self.builder.build_return(Some(&body_val));

        match fn_val.verify(true) {
            true  => Ok(fn_val),
            false => Err("Failed to verify function".into()),
        }
    }
}

pub fn compile_program(prog: Program, output_path: &str, opt_level: OptimizationLevel) -> EmitResult<()> {
    if cfg!(target_arch = "aarch64") {
        Target::initialize_arm(&InitializationConfig::default());
    }
    else if cfg!(target_arch = "x86") {
        Target::initialize_x86(&InitializationConfig::default());
    }
    else {
        panic!("Unsupported architecture");
    }

    let reloc_mode = RelocMode::Default;
    let code_model = CodeModel::Default;

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


    // let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
    let target = Target::from_name("arm").unwrap();
    let target_machine = target.create_target_machine(
        &TargetMachine::get_default_triple(),
        "generic", "", opt_level, reloc_mode, code_model
    ).unwrap();

    let ink_ctx = IContext::create();
    let module = ink_ctx.create_module("snek_root");
    let builder = ink_ctx.create_builder();
    
    // compile functions
    let mut compiler = Compiler::new(&ink_ctx, &builder, &module);
    let Program { functions, main } = prog;

    let function_map: im::HashMap<_, _> = functions.clone()
        .into_iter()
        .map(|FnDef{ name, args, .. }| (name, args))
        .collect();

    let function_vals: Vec<FunctionValue> = functions.into_iter()
        .map(|f| compiler.compile_fn(f, &function_map))
        .collect::<EmitResult<_>>()?;
    // TODO: fix cheap hack here
    let main_fn = FnDef{
        name: "our_code_starts_here".into(),
        args: Vec::new(),
        body: main,
    };
    let main = compiler.compile_fn(main_fn, &function_map)?;

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
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
        .write_to_file(&module, FileType::Assembly, path)
        .map_err(|err| format!("Failed to write to assembly file {output_path}: {}", err))
}
