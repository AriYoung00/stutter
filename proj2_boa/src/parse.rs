use std::collections::HashSet;
use std::str::FromStr;
use sexp::Atom::*;
use sexp::Sexp;


#[derive(Debug, PartialEq, Eq)]
pub enum UOper {
    Add1,
    Sub1,
}
impl FromStr for UOper {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "add1" => Ok(Self::Add1),
            "sub1" => Ok(Self::Sub1),
            _      => Err(format!("Invalid unary operation '{}'", s)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BOper {
    Plus,
    Minus,
    Times,
}
impl FromStr for BOper {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Times),
            _   => Err(format!("Invalid binary operation '{}'", s)),
        }
    }
}

type ParseResult<T> = Result<T, String>;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UOper, Box<Expr>),
    BinOp(BOper, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn from_num(i: i32) -> Box<Self> {
        Box::new(Self::Number(i))
    }
    pub fn from_id(s: String) -> Box<Self> {
        Box::new(Self::Id(s))
    }
    pub fn from_let(v: Vec<(String, Expr)>, e: Box<Expr>) -> Box<Self> {
        Box::new(Self::Let(v, e))
    }
    pub fn from_unary(op: UOper, rhs: Box<Expr>) -> Box<Self> {
        Box::new(Self::UnOp(op, rhs))
    }
    pub fn from_binary(op: BOper, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Self> {
        Box::new(Self::BinOp(op, lhs, rhs))
    }
}


fn parse_ident(ident: &Sexp) -> ParseResult<(String, Expr)> {
    let try_name = move |name: &str| -> Result<String, String> {
        if name == "add1" || name == "sub1" || name == "let" {
            Err(format!("Invalid identifier '{name}'"))
        }
        else {
            Ok(name.to_owned())
        }
    };

    match ident {
        Sexp::List(v) => match &v[..] {
            [Sexp::Atom(S(name)), rhs] => Ok((try_name(name)?, *parse_expr(rhs)?)),
            _ => Err(format!("Invalid binding encountered: {:?}", ident)),
        },
        _ => Err(format!("Invalid binding encountered (outer): {:?}", ident)),
    }
}

fn parse_let(idents: &[Sexp], rhs: &Sexp) -> ParseResult<Box<Expr>> {
    let maps: Vec<_> = idents.iter().map(parse_ident).collect::<Result<_, _>>()?;

    let mut seen = HashSet::new();
    for (name, _) in &maps {
        if seen.contains(&name) {
            return Err(format!("Duplicate binding '{name}'"));
        }
        else {
            seen.insert(name);
        }
    }


    Ok(Expr::from_let(maps, parse_expr(rhs)?))
}

fn parse_binary(op: &str, lhs: &Sexp, rhs: &Sexp) -> ParseResult<Box<Expr>> {
    op
        .parse()
        .and_then(|op| Ok(Expr::from_binary(op, parse_expr(lhs)?, parse_expr(rhs)?)))
}

fn parse_unary(operator: &str, operand: &Sexp) -> ParseResult<Box<Expr>> {
    operator
        .parse()
        .and_then(|op| Ok(Expr::from_unary(op, parse_expr(operand)?)))
}

fn parse_expr(sexp: &Sexp) -> ParseResult<Box<Expr>> {
    match sexp {
        Sexp::Atom(I(n)) => Ok(Expr::from_num(*n as i32)),
        Sexp::Atom(S(s)) => Ok(Expr::from_id(s.clone())),

        Sexp::List(v) => match &v[..] {
            [Sexp::Atom(S(op)), a] => parse_unary(&op, a),
            [Sexp::Atom(S(op)), Sexp::List(binds), rhs] if op == "let" => parse_let(binds, rhs),
            [Sexp::Atom(S(op)), lhs, rhs] => parse_binary(&op, lhs, rhs),
            _ => Err(format!("Invalid expression -- found {} operands (expected 1 or 2)", v.len())),
        },
        
        // [Sexp::Atom(S(op)), a] => parse_unary(op, a, ctx),
        // [Sexp::Atom(S(op)), lhs, rhs] => parse_binary(op, lhs, rhs, ctx),
        _ => Err(format!("Invalid expression {}", sexp)),
    }
}

impl FromStr for Box<Expr> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        sexp::parse(s)
            .map_err(|e| format!("Invalid S-expression syntax: {}", e))
            .and_then(|exp| parse_expr(&exp))
    }
}

#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use crate::parse::ParseResult;

    use super::Expr;

    fn id(arg: &str) -> Box<Expr> {
        Expr::from_id(arg.to_owned())
    }

    fn add1(arg: Box<Expr>) -> Box<Expr> {
        Expr::from_unary(super::UOper::Add1, arg)
    }

    fn sub1(arg: Box<Expr>) -> Box<Expr> {
        Expr::from_unary(super::UOper::Sub1, arg)
    }

    fn num(arg: i32) -> Box<Expr> {
        Expr::from_num(arg)
    }

    fn plus(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Expr::from_binary(super::BOper::Plus, lhs, rhs)
    }

    fn times(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        Expr::from_binary(super::BOper::Times, lhs, rhs)
    }

    fn elet(ident: Vec<(&str, Expr)>, rhs: Box<Expr>) -> Box<Expr> {
        let ident = ident.into_iter().map(|(i, r)| (i.to_owned(), r)).collect();
        Expr::from_let(ident, rhs)
    }

    #[test]
    fn test_parse_simple_valid() {
        let input = "5";
        let res: Box<Expr> = input.parse().unwrap();
        assert_eq!(res, Expr::from_num(5));

        let input = "hellothere";
        let res: Box<Expr> = input.parse().unwrap();
        assert_eq!(res, Expr::from_id("hellothere".to_owned()));

        let input = "(add1 (sub1 5))";
        let res: Box<Expr> = input.parse().unwrap();
        assert_eq!(res, add1(sub1(num(5))));

        let input = "(+ 1 2)";
        let res: Box<Expr> = input.parse().unwrap();
        assert_eq!(res, plus(num(1), num(2)));
    }

    #[test]
    fn test_parse_simple_invalid() {
        type PR = ParseResult<Box<Expr>>;

        let input = "(asdf 1)";
        let res: PR = input.parse();
        assert_matches!(res, Err(_));

        let input = "(+ 1 2 3)";
        let res: PR = input.parse();
        assert_matches!(res, Err(_));

        let input = "(add1 3 4)";
        let res: PR = input.parse();
        assert_matches!(res, Err(_));

        // this test removed since all bindings are guaranteed to be made up of 
        // alphanumeric characters in assignment spec
        // let input = "-";
        // let res: PR = input.parse();
        // assert_matches!(res, Err(_));

        let input = "1 2 3";
        let res: PR = input.parse();
        assert_matches!(res, Err(_));
    }

    #[test]
    fn test_parse_let() {
        type BE = Box<Expr>;
        let input = "(let ((a 1)) a)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, elet(vec![("a", *num(1))], id("a")));

        let input = "(let ((a 1) (b 2) (c 3)) (+ a (* b (add1 c))))";
        let res: BE = input.parse().unwrap();
        let expected = elet(
            vec![("a", *num(1)), ("b", *num(2)), ("c", *num(3))],
            plus(id("a"), times(id("b"), add1(id("c"))))
        );
        assert_eq!(res, expected);
    }
}
