pub const EXIT_OPERAND_MISMATCH: &str = "exit_operand_mismatch";
pub const EXIT_EXPECTED_NUM: &str = "exit_expected_number";
pub const EXIT_EXPECTED_BOOL: &str = "exit_expected_bool";
pub const EXIT_OVERFLOW: &str = "exit_overflow";

pub const SNEK_PRINT: &str = "snek_print";

pub type ParseResult<T> = Result<T, String>;

#[cfg(test)]
/// this module contains convenience functions for tersely constructing an AST inside of tests
pub mod e {
    use crate::ast::{Expr, FnDef, UOper, BOper};

    pub type BE = Box<Expr>;

    pub fn ebreak(arg: BE) -> BE {
        Expr::from_break(arg)
    }

    pub fn eset(id: &str, arg: BE) -> BE {
        Expr::from_set(id.to_owned(), arg)
    }

    pub fn eloop(arg: BE) -> BE {
        Expr::from_loop(arg)
    }

    pub fn id(arg: &str) -> BE {
        Expr::from_id(arg.to_owned())
    }

    pub fn add1(arg: BE) -> BE {
        Expr::from_unary(UOper::Add1, arg)
    }

    pub fn sub1(arg: BE) -> BE {
        Expr::from_unary(UOper::Sub1, arg)
    }

    pub fn num(arg: i32) -> BE {
        Expr::from_num(arg as i64)
    }

    pub fn ebool(arg: bool) -> BE {
        Expr::from_bool(arg)
    }

    pub fn plus(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(BOper::Plus, lhs, rhs)
    }

    pub fn minus(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(BOper::Minus, lhs, rhs)
    }

    pub fn times(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(BOper::Times, lhs, rhs)
    }

    pub fn elet(ident: Vec<(&str, Expr)>, rhs: BE) -> BE {
        let ident = ident.into_iter().map(|(i, r)| (i.to_owned(), r)).collect();
        Expr::from_let(ident, rhs)
    }

    pub fn eif(cond: BE, if_body: BE, else_body: BE) -> BE {
        Expr::from_if(cond, if_body, else_body)
    }

    pub fn isbool(rhs: BE) -> BE {
        Expr::from_unary(crate::ast::UOper::IsBool, rhs)
    }

    pub fn geq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::GreaterEqual, lhs, rhs)
    }

    pub fn leq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::LessEqual, lhs, rhs)
    }

    pub fn eq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::Equal, lhs, rhs)
    }

    pub fn block(stmts: Vec<BE>) -> BE {
        Expr::from_block(stmts.into_iter().map(|x| *x).collect())
    }

    pub fn fun(name: &str, params: &[&str], body: BE) -> FnDef {
        FnDef {
            name: name.to_owned(),
            args: params.into_iter().map(|s| s.to_string()).collect(),
            body,
        }
    }

    pub fn call(name: &str, params: Vec<BE>) -> BE {
        let params = params.into_iter().map(|x| *x).collect();
        Box::new(Expr::Call(name.into(), params))
    }
}
