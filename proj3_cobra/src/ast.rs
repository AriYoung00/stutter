#[derive(Debug, PartialEq, Eq)]
pub enum UOper {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Number(i32),
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
}
