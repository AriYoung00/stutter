use std::fmt;


#[derive(Debug, PartialEq, Eq, Clone)]
/// This enum reprsents the types of valid unary operations
pub enum UOper {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Print,
}

impl UOper {
    pub fn is_uoper(s: &str) -> bool {
        match s {
            "add1" | "sub1" | "isnum" | "isbool" | "print" => true,
            _ => false
        }
    }
}

impl fmt::Display for UOper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UOper::Add1 => write!(f, "+1"),
            UOper::Sub1 => write!(f, "-1"),
            UOper::IsNum => write!(f, "isNum"),
            UOper::IsBool => write!(f, "isBool"),
            UOper::Print => write!(f, "print"),
        }
    }
}


#[derive(Debug, PartialEq, Eq, Clone)]
/// This enum represents the types of valid binary operations
pub enum BOper {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl BOper {
    pub fn is_boper(s: &str) -> bool {
        match s {
            "+" | "-" | "*" | "=" | ">" | ">="
                | "<" | "<=" => true,
            _ => false,
        }
    }
}


impl fmt::Display for BOper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BOper::Plus => write!(f, "+"),
            BOper::Minus => write!(f, "-"),
            BOper::Times => write!(f, "*"),
            BOper::Equal => write!(f, "=="),
            BOper::Greater => write!(f, ">"),
            BOper::GreaterEqual => write!(f, ">="),
            BOper::Less => write!(f, "<"),
            BOper::LessEqual => write!(f, "<="),
        }
    }
}


#[derive(Debug, PartialEq, Eq, Clone)]
/// This enum represents the types and shapes of valid expressions
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UOper, Box<Expr>),
    BinOp(BOper, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),

    Call(String, Vec<Expr>),
    Input,
}

/// This struct represents the type and shape of a valid function definition
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: String,
    pub args: Vec<String>,
    pub body: Box<Expr>,
}


/// This struct represents the type and shape of a valid top-level program
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub functions: Vec<FnDef>,
    pub main: Box<Expr>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "Number({})", n),
            Expr::Boolean(b) => write!(f, "Boolean({})", b),
            Expr::Id(id) => write!(f, "Id({})", id),
            Expr::Let(vars, expr) => {
                write!(f, "Let(")?;
                for (i, (id, e)) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} = {}", id, e)?;
                }
                write!(f, ") {}", expr)
            }
            Expr::UnOp(op, expr) => write!(f, "UnOp({}, {})", op, expr),
            Expr::BinOp(op, lhs, rhs) => write!(f, "BinOp({}, {}, {})", op, lhs, rhs),
            Expr::If(cond, then_expr, else_expr) => write!(f, "\nIf({}, {}, {})", cond, then_expr, else_expr),
            Expr::Loop(body) => write!(f, "\nLoop({})", body),
            Expr::Break(expr) => write!(f, "Break({})", expr),
            Expr::Set(id, expr) => write!(f, "Set({}, {})", id, expr),
            Expr::Block(exprs) => {
                write!(f, "{{")?;
                for e in exprs {
                    write!(f, "\n  {}", e)?;
                }
                write!(f, "\n}}")
            }
            Expr::Input => write!(f, "Input"),
            Expr::Call(name, args) => write!(f, "Call({name}, {args:?})"),
        }
    }
}
