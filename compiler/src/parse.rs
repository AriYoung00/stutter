use crate::ast::{UOper, BOper, Expr};
use crate::util::ParseResult;

use sexp::Sexp;
use sexp::Atom::*;

use std::collections::HashSet;
use std::str::FromStr;


impl FromStr for UOper {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "add1" => Ok(Self::Add1),
            "sub1" => Ok(Self::Sub1),
            "isnum" => Ok(Self::IsNum),
            "isbool" => Ok(Self::IsBool),
            _      => Err(format!("Invalid unary operation '{}'", s)),
        }
    }
}

impl FromStr for BOper {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+"  => Ok(Self::Plus),
            "-"  => Ok(Self::Minus),
            "*"  => Ok(Self::Times),
            "="  => Ok(Self::Equal),
            ">"  => Ok(Self::Greater),
            ">=" => Ok(Self::GreaterEqual),
            "<"  => Ok(Self::Less),
            "<=" => Ok(Self::LessEqual),
            _    => Err(format!("Invalid binary operation '{}'", s)),
        }
    }
}


impl Expr {
    pub fn from_num(i: i64) -> Box<Self> {
        Box::new(Self::Number(i))
    }
    pub fn from_bool(b: bool) -> Box<Self> {
        Box::new(Self::Boolean(b))
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
    pub fn from_if(cond: Box<Expr>, if_block: Box<Expr>, else_block: Box<Expr>) -> Box<Self> {
        Box::new(Self::If(cond, if_block, else_block))
    }
    pub fn from_loop(body: Box<Expr>) -> Box<Self> {
        Box::new(Self::Loop(body))
    }
    pub fn from_break(rhs: Box<Expr>) -> Box<Self> {
        Box::new(Self::Break(rhs))
    }
    pub fn from_set(name: String, rhs: Box<Expr>) -> Box<Self> {
        Box::new(Self::Set(name, rhs))
    }
    pub fn from_block(stmts: Vec<Expr>) -> Box<Self> {
        Box::new(Self::Block(stmts))
    }
}


fn parse_ident(ident: &Sexp) -> ParseResult<(String, Expr)> {
    let try_name = move |name: &str| -> Result<String, String> {
        if name.parse::<UOper>().is_ok() {
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

    if maps.is_empty() {
        return Err("Invalid binding (no identifiers)".to_owned());
    }

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


fn parse_list(op: &str, list: &[Sexp]) -> ParseResult<Box<Expr>> {
    match (op, list) {
        ("let", [Sexp::List(binds), rhs]) => parse_let(binds, rhs),
        ("set!", [Sexp::Atom(S(name)), rhs]) => Ok(Expr::from_set(name.clone(), 
                                                                  parse_expr(rhs)?)),
        ("if", [cond, if_b, else_b]) => Ok(Expr::from_if(parse_expr(cond)?, 
                parse_expr(if_b)?, parse_expr(else_b)?)),
        ("loop", [rhs])  => Ok(Expr::from_loop(parse_expr(rhs)?)),
        ("break", [rhs]) => Ok(Expr::from_break(parse_expr(rhs)?)),

        // most generic parse steps come last, to prioritize matching more specific
        (op, [a]) => parse_unary(&op, a),
        (op, [lhs, rhs]) => parse_binary(&op, lhs, rhs),
        _ => Err(format!("Invalid expression -- found {} operands (expected 1 or 2)", list.len())),
    }
}

fn parse_block(stmts: &[Sexp]) -> ParseResult<Box<Expr>> {
    let stmts = stmts.iter()
        .map(parse_expr) // parse statements
        .map(|r| r.and_then(|v| Ok(*v))) // unbox statements
        .collect::<ParseResult<Vec<_>>>()?; // collect and check error
    Ok(Expr::from_block(stmts))
}


fn parse_string(s: &str) -> ParseResult<Box<Expr>> {
    Ok(match s {
        "true" => Expr::from_bool(true),
        "false" => Expr::from_bool(false),
        "input" => Box::new(Expr::Input),
        _ => Expr::from_id(s.to_owned())
    })
}

fn parse_expr(sexp: &Sexp) -> ParseResult<Box<Expr>> {
    match sexp {
        Sexp::Atom(I(n)) => Ok(Expr::from_num(*n)),
        Sexp::Atom(S(s)) => parse_string(s),

        Sexp::List(v)    => match &v[..] {
            [Sexp::Atom(S(s)), the_rest @ ..] if s == "block" =>  parse_block(the_rest),
            [Sexp::Atom(S(s)), the_rest @ ..] => parse_list(s, the_rest),
            [first, _] => Err(format!("Invalid expression -- expected string atom first, got {:?}", first)),
            stuff @ _  => Err(format!("Invalid expression -- got ambiguous structure {:?}", stuff))
        },

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
    use crate::parse::ParseResult;
    use super::Expr;

    type BE = Box<Expr>;

    fn ebreak(arg: BE) -> BE {
        Expr::from_break(arg)
    }

    fn eset(id: &str, arg: BE) -> BE {
        Expr::from_set(id.to_owned(), arg)
    }

    fn eloop(arg: BE) -> BE {
        Expr::from_loop(arg)
    }

    fn id(arg: &str) -> BE {
        Expr::from_id(arg.to_owned())
    }

    fn add1(arg: BE) -> BE {
        Expr::from_unary(super::UOper::Add1, arg)
    }

    fn sub1(arg: BE) -> BE {
        Expr::from_unary(super::UOper::Sub1, arg)
    }

    fn num(arg: i32) -> BE {
        Expr::from_num(arg as i64)
    }

    fn plus(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(super::BOper::Plus, lhs, rhs)
    }
    
    fn minus(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(super::BOper::Minus, lhs, rhs)
    }

    fn times(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(super::BOper::Times, lhs, rhs)
    }

    fn elet(ident: Vec<(&str, Expr)>, rhs: BE) -> BE {
        let ident = ident.into_iter().map(|(i, r)| (i.to_owned(), r)).collect();
        Expr::from_let(ident, rhs)
    }

    fn eif(cond: BE, if_body: BE, else_body: BE) -> BE {
        Expr::from_if(cond, if_body, else_body)
    }

    fn isbool(rhs: BE) -> BE {
        Expr::from_unary(crate::ast::UOper::IsBool, rhs)
    }

    fn geq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::GreaterEqual, lhs, rhs)
    }

    fn leq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::LessEqual, lhs, rhs)
    }

    fn eq(lhs: BE, rhs: BE) -> BE {
        Expr::from_binary(crate::ast::BOper::Equal, lhs, rhs)
    }

    #[test]
    fn test_parse_simple_valid() {
        let input = "5";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, Expr::from_num(5));

        let input = "hellothere";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, Expr::from_id("hellothere".to_owned()));

        let input = "(add1 (sub1 5))";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, add1(sub1(num(5))));

        let input = "(+ 1 2)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, plus(num(1), num(2)));
    }

    #[test]
    fn test_parse_simple_invalid() {
        type PR = ParseResult<BE>;

        let input = "(asdf 1)";
        let res: PR = input.parse();
        assert!(matches!(res, Err(_)));

        let input = "(+ 1 2 3)";
        let res: PR = input.parse();
        assert!(matches!(res, Err(_)));

        let input = "(add1 3 4)";
        let res: PR = input.parse();
        assert!(matches!(res, Err(_)));

        let input = "1 2 3";
        let res: PR = input.parse();
        assert!(matches!(res, Err(_)));
    }

    #[test]
    fn test_parse_let() {
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

    #[test]
    fn test_parse_if() {
        let input = "(if true (+ 1 2) (- 2 1))";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eif(Expr::from_bool(true), plus(num(1), num(2)), minus(num(2), num(1))));

        let input = "(if (isbool 5) (<= 1 2) (>= 2 1))";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eif(isbool(num(5)), leq(num(1), num(2)), geq(num(2), num(1))));

        let input = "(if (isbool (= 1 1)) (add1 5) (sub1 7))";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eif(isbool(eq(num(1), num(1))), add1(num(5)), sub1(num(7))));
    }

    #[test]
    fn test_parse_break() {
        let input = "(break 5)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, ebreak(num(5)));
    }

    #[test]
    fn test_parse_loop() {
        let input = "(loop (if (= 1 2) (break 1) (break 2)))";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eloop(eif(eq(num(1), num(2)), ebreak(num(1)), ebreak(num(2)))));
    }

    #[test]
    fn test_parse_set() {
        let input = "(set! hello 5)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eset("hello", num(5)));
    }
}
