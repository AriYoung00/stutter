use std::cell::RefCell;
use std::path::Path;

use crate::ast::*;
use crate::util::*;

use inkwell::AddressSpace;
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
use inkwell::{
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
        FunctionValue, PointerValue,
        BasicMetadataValueEnum::IntValue as EIntValue, BasicValue
    },
};

#[derive(Clone)]
struct LoopCtx<'a, 'ctx> {
    pub exit: &'a BasicBlock<'ctx>,
    // use RefCell here for convenience
    // tired of fighting the compiler
    pub phi_incoming: RefCell<Vec<(IntValue<'ctx>, BasicBlock<'ctx>)>>,
}

#[derive(Clone)]
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
    struct_eq_fn: FunctionValue<'ctx>,
    heap_top:    PointerValue<'ctx>,
}

#[allow(dead_code)]
enum Primitive {
    Bool,
    Int,
    Tuple,
    Nil,
}

impl ToString for Primitive {
    fn to_string(&self) -> String {
        match self {
            Primitive::Bool => format!("bool"),
            Primitive::Int  => format!("int"),
            Primitive::Tuple => format!("tuple"),
            Primitive::Nil  => format!("nil"),
        }
    }
}

fn make_fn_name(name: &str) -> String {
    format!("\x01{name}")
}

const fn make_snek_int(val: i64) -> u64 {
    (val << 1) as u64
}

const fn make_snek_bool(val: bool) -> u64 {
    ((val as u64) << 2) | 0b011
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(ink_ctx: &'ctx IContext, builder: &'a Builder<'ctx>, module: &'a Module<'ctx>) -> Self {
        use BasicMetadataTypeEnum::IntType;
        let i64_type = ink_ctx.i64_type();

        let one_arg_void_fn_type = ink_ctx.void_type().fn_type(&[IntType(i64_type)], false);
        let two_arg_val_fn_type = ink_ctx.i64_type().fn_type(&[IntType(i64_type), IntType(i64_type)], false);

        let exit_err_fn = module.add_function(&make_fn_name("snek_error"),
            one_arg_void_fn_type, Some(Linkage::External));
        let print_fn = module.add_function(&make_fn_name("snek_print"),
            one_arg_void_fn_type, Some(Linkage::External));
        let struct_eq_fn = module.add_function(&make_fn_name("snek_struct_eq"),
            two_arg_val_fn_type, Some(Linkage::External));

        let heap_top = module.add_global(ink_ctx.i64_type(), Some(AddressSpace::default()), "heap_top");
        heap_top.set_initializer(&ink_ctx.i64_type().const_zero());
        let heap_top = heap_top.as_pointer_value();

        Self { ink_ctx, builder, module, exit_err_fn, print_fn, struct_eq_fn, heap_top }
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
        let phi = self.builder.build_phi(self.ink_ctx.i64_type(), &format!("{label_tag}_res"));
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
        let two_lit = self.ink_ctx.i64_type().const_int(2, false);
        let three_lit = self.ink_ctx.i64_type().const_int(3, false);
        // this is the tagged Snek value one
        let one_snek = self.ink_ctx.i64_type().const_int(make_snek_int(1), false);

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
                    .build_left_shift(inverted, two_lit, "isnum_shift_tmp");
                self.builder
                    .build_int_add(shifted, three_lit, "typecheck_result")
            },
            UOper::IsBool => {
                // for some val
                // masked = val & 0b11
                // res = (((masked >> 1) & (masked)) << 2) + 3
                let masked = self.builder
                    .build_and(operand, three_lit, "isbool_and_tmp");
                let shifted_one = self.builder
                    .build_right_shift(masked, one_lit, false, "isbool_lshift_tmp");
                let anded = self.builder
                    .build_and(masked, shifted_one, "isbool_and_tmp");
                let de_shifted = self.builder
                    .build_left_shift(anded, two_lit, "isbool_rshift_tmp");

                self.builder
                    .build_int_add(de_shifted, three_lit, "isbool_add")
            },
            UOper::Print => {
                self.builder
                    .build_call(self.print_fn, &[EIntValue(operand)], "print_tmp");
                operand
            },
        })
    }

    fn add_type_check<'b>(&mut self, operand: IntValue<'ctx>, the_type: Primitive, ctx: &Ctx<'b, 'ctx>) {
        // to check if int, check if lowest bit is equal to zero
        // to check if bool, check if lowest two bits are equal to 3
        let three = self.ink_ctx.i64_type().const_int(0b11, false);
        let one = self.ink_ctx.i64_type().const_int(1, false);
        let zero = self.ink_ctx.i64_type().const_zero();

        let bool_masked = self.builder
            .build_and(operand, three, "type_check_bool_and");
        let int_masked = self.builder
            .build_and(operand, one, "type_check_int_and");

        // `cond` will be 0 if this is the type we think it is
        // i.e. if cond is 1, throw an error
        let cond = match the_type {
            // bool is tag=11
            Primitive::Bool => self.builder
                .build_int_compare(IntPredicate::NE, bool_masked, three, "type_check_cond"),
            // int is tag=x0
            Primitive::Int  => self.builder
                .build_int_compare(IntPredicate::NE, int_masked, zero, "type_check_cond"),
            // tuple is  tag=01, rest!=0
            Primitive::Tuple => {
                let is_not_one = self.builder
                    .build_int_compare(IntPredicate::NE, bool_masked, one, "type_check_cond");
                let is_only_one = self.builder
                    .build_int_compare(IntPredicate::EQ, operand, one, "type_check_cond");
                self.builder
                    .build_or(is_not_one, is_only_one, "type_check_cond")
            },
            // nil is tag=01, rest=0
            Primitive::Nil  => self.builder
                .build_int_compare(IntPredicate::NE, operand, one, "type_check_cond"),
        };

        // in the else block, we will branch to the respective error function
        let error_arg = match the_type {
            // 8 for "invalid argument - expected bool, not number",
            Primitive::Bool => self.ink_ctx.i64_type()
                .const_int(8, false),
            // 7 for "invalid argument - expected number, not bool",
            Primitive::Int  => self.ink_ctx.i64_type()
                .const_int(7, false),
            Primitive::Tuple => self.ink_ctx.i64_type()
                .const_int(11, false),
            Primitive::Nil => self.ink_ctx.i64_type()
                .const_int(12, false),
        };
        self.build_conditional_call(cond, self.exit_err_fn, &[EIntValue(error_arg)], "snek_error", "type_check", ctx);
    }

    #[allow(dead_code)]
    fn add_same_type_check<'b>(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) {
        // is_same_type = (lowest bits both zero) | (lowest 2 bits equal)
        let zero = self.ink_ctx.i64_type()
            .const_zero();
        let one = self.ink_ctx.i64_type()
            .const_int(1, false);
        let three = self.ink_ctx.i64_type()
            .const_int(3, false);

        let lhs_masked_one = self.builder
            .build_and(lhs, one, "same_type_check_lhs_masked");
        let rhs_masked_one = self.builder
            .build_and(rhs, one, "same_type_check_rhs_masked");

        let lhs_masked_three = self.builder
            .build_and(lhs, three, "same_type_check_lhs_masked");
        let rhs_masked_three = self.builder
            .build_and(rhs, three, "same_type_check_rhs_masked");

        let lhs_zero = self.builder
            .build_int_compare(IntPredicate::EQ, lhs_masked_one, zero, "same_type_check_lhs_lowest_zero");
        let rhs_zero = self.builder
            .build_int_compare(IntPredicate::EQ, rhs_masked_one, zero, "same_type_check_rhs_lowest_zero");
        let both_zero = self.builder
            .build_and(lhs_zero, rhs_zero, "same_type_check_lowest_both_zero");

        let lowest_two_eq = self.builder
            .build_int_compare(IntPredicate::EQ, lhs_masked_three, rhs_masked_three, "same_type_check_lowest_two_same");

        let cond = self.builder
            .build_or(both_zero, lowest_two_eq, "same_type_check_cond");
        // check hack to invert cond, since we want to fail if they're NOT the same type
        let cond = self.builder
            .build_not(cond, "same_type_check_cond_hack");

        let error_arg = self.ink_ctx.i64_type()
            .const_int(10, false);
        self.build_conditional_call(cond, self.exit_err_fn, &[EIntValue(error_arg)], "snek_error", "same_type_check", ctx);
    }

    fn get_vec_base_ptr<'b>(&mut self, vec_val: IntValue<'ctx>) -> PointerValue<'ctx> {
        let tup_tag = self.ink_ctx.i64_type()
            .const_int(1, false);
        let i64_ptr = self.ink_ctx.i64_type()
            .ptr_type(AddressSpace::default());

        // remove tag bit
        let tup_ptr_int = self.builder
            .build_xor(vec_val, tup_tag, "tup_ptr_de_tag");

        self.builder
            .build_int_to_ptr(tup_ptr_int, i64_ptr, "tup_size_ptr")
    }

    fn get_vec_elem_ptr<'b>(&mut self, idx_val: IntValue<'ctx>, vec_val: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> PointerValue<'ctx> {
        let i64_ptr = self.ink_ctx.i64_type()
            .ptr_type(AddressSpace::default());

        let zero = self.ink_ctx.i64_type().const_zero();
        let one = self.ink_ctx.i64_type()
            .const_int(1, false);
        let eight = self.ink_ctx.i64_type()
            .const_int(8, false);

        // remove index tag
        let tup_idx_int = self.builder
            .build_right_shift(idx_val, one, false, "tup_idx_de_tag");

        // read tuple size
        let tup_size_ptr = self.get_vec_base_ptr(vec_val);
        let tup_size = self.builder
            .build_load(self.ink_ctx.i64_type(), tup_size_ptr, "tup_size_val")
            .into_int_value();

        // error if tuple size is less than idx
        let cond = self.builder
            .build_int_compare(IntPredicate::UGT, tup_idx_int, tup_size, "tup_size_check_cond");
        let call_args = [EIntValue(self.ink_ctx.i64_type().const_int(12, false))];
        self.build_conditional_call(cond, self.exit_err_fn, &call_args, "snek_error", "tup_size_check_exit_err", ctx);

        let cond = self.builder
            .build_int_compare(IntPredicate::SLT, tup_idx_int, zero, "tup_bounds_check_cond");
        let call_args = [EIntValue(self.ink_ctx.i64_type().const_int(12, false))];
        self.build_conditional_call(cond, self.exit_err_fn, &call_args, "snek_error", "vec_bounds_check_exit_err", ctx);


        // offset index to account for length tag
        let tup_idx_int = self.builder
            .build_int_add(tup_idx_int, one, "vec_idx_length_offset");

        // multiply index by elem size
        let tup_idx_offset = self.builder
            .build_int_mul(tup_idx_int, eight, "tup_idx_compute_offset");

        // add offset to base pointer
        let tup_ptr_int = self.builder
            .build_ptr_to_int(tup_size_ptr, self.ink_ctx.i64_type(), "tup_ptr_to_int");
        let elem_ptr_int = self.builder
            .build_int_add(tup_ptr_int, tup_idx_offset, "tup_elem_ptr_int");

        // convert to pointer
        let elem_ptr = self.builder
            .build_int_to_ptr(elem_ptr_int, i64_ptr, "tup_elem_ptr");

        elem_ptr
    }

    fn compile_binary_op<'b>(&mut self, op: &BOper, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, ctx: &Ctx<'b, 'ctx>) -> EmitResult<IntValue<'ctx>> {
        const TRUE:  i64 = make_snek_bool(true) as i64;
        const FALSE: i64 = make_snek_bool(false) as i64;

        if let BOper::Equal = op {
            self.add_same_type_check(lhs, rhs, ctx)
            // for now do nothing here
        }
        else if let BOper::StructEqual = op {
            // do nothing
        }
        else {
            self.add_type_check(lhs, Primitive::Int, ctx);
            self.add_type_check(rhs, Primitive::Int, ctx);
        }

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
            BOper::StructEqual => {
                // call the external function for struct equality and return that
                let res = self.builder
                    .build_call(self.struct_eq_fn, &[EIntValue(lhs), EIntValue(rhs)], "snek_struct_eq")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                res
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
                Ok(self.ink_ctx.i64_type()
                    .const_int(make_snek_int(*n), true)
                    .into())
            },

            Expr::Boolean(b) => {
                Ok(self.ink_ctx.i64_type()
                    .const_int(make_snek_bool(*b), false)
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
                let false_ = self.ink_ctx.i64_type().const_int(make_snek_bool(false), false);
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
                    .collect::<EmitResult<Vec<_>>>()?
                    .into_iter()
                    .last()
                    .ok_or(format!("Error: Found empty block")),

            Expr::Call(name, args) => {
                let mangled_name = make_fn_name(name);
                let Some(fn_val) = self.module.get_function(&mangled_name) else {
                    return Err(format!("Function {name} is undefined"));
                };
                
                // TODO: we are just throwing away the has_terminator value from compilation
                // the correct behavior would have a BasicBlock for each statement where the basic
                // blocks unconditionally branch to the next if the sub-expr doesn't contain a
                // terminator
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

            Expr::Vec(vals) => {
                let vals: Vec<_> = vals.into_iter()
                    .map(|x| self.compile_expr(x, ctx)
                        //.map(|y| EIntValue(y.val)))
                        .map(|y| y.val))
                    .collect::<EmitResult<_>>()?;

                let old_heap_top_load = self.builder
                    .build_load(self.ink_ctx.i64_type(), self.heap_top, "tup_address_tmp");
                let old_heap_top_int = old_heap_top_load
                    .into_int_value();
                let old_heap_top_ptr = self.builder
                        .build_int_to_ptr(old_heap_top_int, self.ink_ctx.i64_type().ptr_type(AddressSpace::default()), "tup_address_ptr");


                // now allocate heap space
                // keep in mind that heap pointer's type is i64 so +1 will allocate 8 bytes
                // allocate for each value, plus one for length
                let incr_amt = self.ink_ctx.i64_type()
                    .const_int(8 * (vals.len() + 1) as u64, false);
                let new_heap_top = self.builder
                    .build_int_add(old_heap_top_int, incr_amt, "new_stack_top");
                self.builder.build_store(self.heap_top, new_heap_top);

                // store length
                self.builder
                    .build_store(old_heap_top_ptr, self.ink_ctx.i64_type()
                        .const_int(vals.len() as u64, false));
                // store elements
                for (idx, val) in vals.into_iter().enumerate() {
                    // make space for the length tag by adding 1
                    let idx = idx as u64 + 1;
                    let val_ptr_int = self.builder
                        .build_int_add(old_heap_top_int, self.ink_ctx.i64_type().const_int(8 * idx, false), "tup_assign_ptr_int");
                    let val_ptr = self.builder
                        .build_int_to_ptr(val_ptr_int, self.ink_ctx.i64_type().ptr_type(AddressSpace::default()), "tup_val_ptr");

                    self.builder
                        .build_store(val_ptr, val);
                }
              
                // construct tagged tuple ref
                let tag = self.ink_ctx.i64_type().const_int(0b01, false);
                Ok(self.builder
                    .build_or(old_heap_top_int, tag, "tuple_ref")
                    .into())
            },

            Expr::VecGet(idx_expr, tuple_expr) => {
                let EmitVal { val: idx_val, has_terminator: idx_has_term } = self.compile_expr(idx_expr, ctx)?;
                self.add_type_check(idx_val, Primitive::Int, ctx);
                let EmitVal { val: tup_val, has_terminator: tup_has_term } = self.compile_expr(tuple_expr, ctx)?;
                self.add_type_check(tup_val, Primitive::Tuple, ctx);

                let elem_ptr = self.get_vec_elem_ptr(idx_val, tup_val, ctx);
                // load value
                let elem_val = self.builder
                    .build_load(self.ink_ctx.i64_type(), elem_ptr, "elem_val")
                    .into_int_value();
                
                // Ok(EmitVal { val: elem_val, has_terminator: idx_has_term || tup_has_term })
                Ok(EmitVal { val: elem_val, has_terminator: false })
            },

            Expr::VecSet(idx_expr, vec_expr, val_expr) => {
                let EmitVal{ val: idx_val, .. }  = self.compile_expr(idx_expr, ctx)?;
                self.add_type_check(idx_val, Primitive::Int, ctx);
                let EmitVal{ val: vec_val, .. }  = self.compile_expr(vec_expr, ctx)?;
                self.add_type_check(vec_val, Primitive::Tuple, ctx);
                let EmitVal{ val: val_val, .. }  = self.compile_expr(val_expr, ctx)?;
                // no type check for val

                let elem_ptr = self.get_vec_elem_ptr(idx_val, vec_val, ctx);
                self.builder
                    .build_store(elem_ptr, val_val);
                
                Ok(EmitVal { val: val_val, has_terminator: false })
            },

            Expr::Nil => Ok(self.ink_ctx.i64_type()
                .const_int(1, false).into()),
            Expr::VecLen(vec_expr) => {
                let EmitVal{ val: vec_val, has_terminator } = self.compile_expr(vec_expr, ctx)?;
                let vec_size_ptr = self.get_vec_base_ptr(vec_val);
                let vec_size = self.builder
                    .build_load(self.ink_ctx.i64_type(), vec_size_ptr, "tup_size_val")
                    .into_int_value();

                let vec_size_tagged = self.builder
                    .build_int_mul(vec_size, self.ink_ctx.i64_type().const_int(2, false), "vec_size_add_tag");

                Ok(EmitVal { val: vec_size_tagged, has_terminator })
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

    fn _compile_fn(&mut self, fun: FnDef, is_main: bool) -> EmitResult<FunctionValue<'ctx>> {
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

        // TODO: fix hack
        if is_main {
            println!("we're in main, building heap top");
            let heap_top_arg_ptr = fn_ctx.vars.get("heap_top")
                .expect("Unable to find 'heap_top' as argument to main. This should never happen.");
            let heap_top_arg_val = self.builder.build_load(self.ink_ctx.i64_type(), *heap_top_arg_ptr, "heap_top_val");
            self.builder.build_store(self.heap_top, heap_top_arg_val);
        }

        // let body_val = self.compile_expr(fun.body.as_ref(), &fn_ctx)?;
        let body_val = self.compile_expr(fun.body.as_ref(), &fn_ctx)?;
        println!("compiled body of fn {}", fun.name);

        // we can assume that a break hasn't terminated the current block
        // since we are necesarily not inside a loop
        self.builder.build_return(Some(&body_val.val));
        println!("build return");

        match fn_val.verify(true) {
            true  => Ok(fn_val),
            false => Err("Failed to verify function".into()),
        }
    }

    pub fn compile_fn(&mut self, fun: FnDef) -> EmitResult<FunctionValue<'ctx>> {
        self._compile_fn(fun, false)
    }

    pub fn compile_main(&mut self, main: FnDef) -> EmitResult<FunctionValue<'ctx>> {
        let _main_proto = self.compile_proto(&main, Some(Linkage::DLLExport));
        self._compile_fn(main, true)
    }
}

pub fn compile_program(prog: Program, output_path: &str, opt_level: OptimizationLevel, file_type: FileType) -> EmitResult<()> {
    Target::initialize_x86(&InitializationConfig::default());
    Target::initialize_aarch64(&InitializationConfig::default());

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

    // TODO: fix cheap hack here
    let main_fn = FnDef{
        name: "our_code_starts_here".into(),
        // TODO: fix this cheap hack as well, input is just an argument to main
        args: vec!["input".into(), "heap_top".into()],
        body: main,
    };

    let main = compiler.compile_main(main_fn)?;

    let function_vals: Vec<FunctionValue> = functions.into_iter()
        .map(|f| compiler.compile_fn(f))
        .collect::<EmitResult<_>>()?;

    println!("compilation finished");
    module.print_to_stderr();

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

    for function in function_vals {
        fpm.run_on(&function);
    }
    fpm.run_on(&main);

    let path = Path::new(output_path);
    target_machine
        .write_to_file(&module, file_type, path)
        .map_err(|err| format!("Failed to write to assembly file {output_path}: {}", err))
}

#[cfg(test)]
mod tests {
    use crate::llvm::{make_snek_int, make_snek_bool};

    #[test]
    fn test_make_snek_int() {
        assert_eq!(make_snek_int(1), 2);
        assert_eq!(make_snek_int(-5), -10_i64 as u64);
        assert_eq!(make_snek_int(35), 70);
    }

    #[test]
    fn test_make_snek_bool() {
        assert_eq!(make_snek_bool(true), 0b111);
        assert_eq!(make_snek_bool(false), 0b011);

    }
}
