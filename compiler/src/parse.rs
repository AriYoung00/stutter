use crate::ast::{BOper, Expr, FnDef, Program, UOper};
use crate::util::ParseResult;

use sexp::Atom::*;
use sexp::Sexp;

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
            _ => Err(format!("Invalid unary operation '{}'", s)),
        }
    }
}

impl FromStr for BOper {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Times),
            "=" => Ok(Self::Equal),
            ">" => Ok(Self::Greater),
            ">=" => Ok(Self::GreaterEqual),
            "<" => Ok(Self::Less),
            "<=" => Ok(Self::LessEqual),
            _ => Err(format!("Invalid binary operation '{}'", s)),
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

/// This function takes a string reference, and attempts to determine if it is a valid name for a
/// `let` binding or `fun` declaration. It does so by attempting to parse it as a unary operation,
/// and then returning an error if it can be parsed as a unary operation, since all unary
/// operations are invalid names.
fn try_name(name: &str) -> ParseResult<String> {
    if name.parse::<UOper>().is_ok() {
        Err(format!("Invalid identifier '{name}'"))
    } 
    else {
        Ok(name.to_owned())
    }
}

/// This function takes an S-expression and attempts to parse it as a single `let` binding. It
/// returns a pair of (String, Box<Expr>) representing the name of the binding, and the expression
/// which should be evaluated to determine its value.
fn parse_ident(ident: &Sexp) -> ParseResult<(String, Expr)> {
    match ident {
        Sexp::List(v) => match &v[..] {
            [Sexp::Atom(S(name)), rhs] => Ok((try_name(name)?, *parse_expr(rhs)?)),
            _ => Err(format!("Invalid binding encountered: {:?}", ident)),
        },
        _ => Err(format!("Invalid binding encountered (outer): {:?}", ident)),
    }
}

/// This function takes a list of S-expressions, and attempts to parse all of them as individual
/// bindings within a `let` statement.
fn parse_let(idents: &[Sexp], rhs: &Sexp) -> ParseResult<Box<Expr>> {
    let maps: Vec<_> = idents.iter().map(parse_ident).collect::<Result<_, _>>()?;

    if maps.is_empty() {
        return Err("Invalid binding (no identifiers)".to_owned());
    }

    let mut seen = HashSet::new();
    for (name, _) in &maps {
        if seen.contains(&name) {
            return Err(format!("Duplicate binding '{name}'"));
        } else {
            seen.insert(name);
        }
    }

    Ok(Expr::from_let(maps, parse_expr(rhs)?))
}

/// This method takes an operation and two S-expressions, and tries to parse the combination of the
/// 3 as a binary operation. It will try to parse `op` as a [`BOper`], and then try to parse `lhs` and
/// `rhs` as [`Box<Expr>`].
fn parse_binary(op: &str, lhs: &Sexp, rhs: &Sexp) -> ParseResult<Box<Expr>> {
    op.parse()
        .and_then(|op| Ok(Expr::from_binary(op, parse_expr(lhs)?, parse_expr(rhs)?)))
}

/// This method takes an operation and an S-expression, and trues to parse them as a unary
/// operation. It will try to parse `op` as a [`UOper`], and then try to parse `operand` as a
/// [`Box<Expr>`]
fn parse_unary(operator: &str, operand: &Sexp) -> ParseResult<Box<Expr>> {
    operator
        .parse()
        .and_then(|op| Ok(Expr::from_unary(op, parse_expr(operand)?)))
}

/// This method takse an operation and a list of S-expressions, and tries to determine what to
/// parse the overall structure as. It checks for several keywords, attempting to dispatch to
/// parsing methods for each of the expressions the keywords represent. Finally, it will attempt to
/// parse the operation and operands as either a [`Expr::BinOp`] or [`Expr::UnOp`]
fn parse_list(op: &str, list: &[Sexp]) -> ParseResult<Box<Expr>> {
    match (op, list) {
        ("let", [Sexp::List(binds), rhs]) => parse_let(binds, rhs),
        ("set!", [Sexp::Atom(S(name)), rhs]) => Ok(Expr::from_set(name.clone(), parse_expr(rhs)?)),
        ("if", [cond, if_b, else_b]) => Ok(Expr::from_if(
            parse_expr(cond)?,
            parse_expr(if_b)?,
            parse_expr(else_b)?,
        )),
        ("loop", [rhs]) => Ok(Expr::from_loop(parse_expr(rhs)?)),
        ("break", [rhs]) => Ok(Expr::from_break(parse_expr(rhs)?)),
        ("fun", _) => Err("Found 'fun' in an invalid context (not top-level)".into()),

        (op, [a])
            if UOper::is_uoper(op) => parse_unary(op, a),
        (op, [lhs, rhs])
            if BOper::is_boper(op) => parse_binary(&op, lhs, rhs),

        // we explicitly cover this case because it shouldn't happen
        (op, []) => panic!("parse_list called with empty list, op={op}"),

        (fn_name, args) => parse_fn_call(fn_name, args),
    }
}

/// This method will attempt to parse the given list of statements as a `block!` expression by
/// iteratively mapping them into parsed expressions.
fn parse_block(stmts: &[Sexp]) -> ParseResult<Box<Expr>> {
    let stmts = stmts
        .iter()
        .map(parse_expr) // parse statements
        .map(|r| r.and_then(|v| Ok(*v))) // unbox statements
        .collect::<ParseResult<Vec<_>>>()?; // collect and check error
    Ok(Expr::from_block(stmts))
}

/// This memthod will attempt to parse a [`Sexp::Atom(S(_))`] which has been found on its own
/// inside its parse tree level. This method does not consider attempting to parse `s` as a keyword
/// or id -- just as a literal (`true`, `false`, `input`, `print`)
fn parse_string(s: &str) -> ParseResult<Box<Expr>> {
    Ok(match s {
        "true" => Expr::from_bool(true),
        "false" => Expr::from_bool(false),
        "input" => Box::new(Expr::Input),
        _ => Expr::from_id(s.to_owned()),
    })
}

/// This method will attempt to parse the given list of statements as a [`FnDef`]. It does not
/// return an expression, as [`FnDef`] is not a member of expression, and is not valid in all of
/// the contexts where an [`Expr`] is within the AST / parse tree
fn parse_fun(stmts: &[Sexp]) -> ParseResult<FnDef> {
    let make_name = |exp: &Sexp| match exp {
        Sexp::Atom(S(name)) => try_name(name),
        _ => return Err("Found something besides Atom(str) in fun name / param position".into()),
    };

    let [Sexp::List(name_and_params), body_exp] = stmts else {
        return Err(format!("Expected (name param+) (body) in fn def, instead found {stmts:?}"));
    };

    let [name_exp, params_exps @ ..] = &name_and_params[..] else {
        return Err(format!("unable to bind name and params from structure: {name_and_params:?}"));
    };

    let name: String = make_name(name_exp)?;
    let args: Vec<_> = params_exps
        .into_iter()
        .map(make_name)
        .collect::<ParseResult<_>>()?;
    let body = parse_expr(body_exp)?;

    Ok(FnDef { name, args, body })
}

/// This method is the top-level dispatch method for attempting to parse an S-expression. It
/// examines several attributes of the structure, and either attempts to directly parse it as an
/// Atom (in the case of [`Sexp::Atom`], or dispatches parsing to some other, more specialized
/// parsing method (in the case of [`Sexp::List`])
fn parse_expr(sexp: &Sexp) -> ParseResult<Box<Expr>> {
    match sexp {
        Sexp::Atom(I(n)) => Ok(Expr::from_num(*n)),
        Sexp::Atom(S(s)) => parse_string(s),

        Sexp::List(v) => match &v[..] {
            [Sexp::Atom(S(s)), the_rest @ ..] if s == "block" => parse_block(the_rest),
            [Sexp::Atom(S(s)), the_rest @ ..] => parse_list(s, the_rest),
            [first, _] => Err(format!(
                "Invalid expression -- expected string atom first, got {:?}",
                first
            )),
            stuff @ _ => Err(format!(
                "Invalid expression -- got ambiguous structure {:?}",
                stuff
            )),
        },

        _ => Err(format!("Invalid expression {}", sexp)),
    }
}

/// Parse a function call with `name` assumed to be the name of the function being called, and
/// `args` the expressions which should be evaluated as the arguments to fn call
fn parse_fn_call(name: &str, args: &[Sexp]) -> ParseResult<Box<Expr>> {
    let args = args
        .iter()
        .map(parse_expr) // parse statements
        .map(|r| r.and_then(|v| Ok(*v))) // unbox statements
        .collect::<ParseResult<Vec<_>>>()?; // collect and check error

    Ok(Box::new(Expr::Call(name.into(), args)))
}

impl FromStr for Box<Expr> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        sexp::parse(s)
            .map_err(|e| format!("Invalid S-expression syntax: {}", e))
            .and_then(|exp| parse_expr(&exp))
    }
}

/// This enum is a convenience type which is used to generically parse top-level statements into
/// either function definitions, or expressions. It is internal to `parse.rs`, and should not be
/// public / used outside this file.
enum InterParseRes {
    Fn(FnDef),
    Exp(Box<Expr>),
}

/// This function takes a list of S-Expressions which should be the top level expressions in the
/// file which we are parsing. This means that it should be a list of expressions, each of which
/// represent either a function definition, or a top-level expression to evaluate.
///
/// This method will parse either of those things, and return an InterParseResult, which can be
/// either of those things. The caller should handle partitioning the vector of InterParseRes into
/// the expected structure of the program.
fn parse_top_level(stuff: &[Sexp]) -> ParseResult<Vec<InterParseRes>> {
    let parser = |exp: &Sexp| {
        Ok(match exp {
            // if it's a list
            Sexp::List(l) => match &l[..] {
                // check if the first element is the 'fun keyword'
                // if it is, parse as a function definition
                [Sexp::Atom(S(s)), the_rest @ ..] 
                    if s == "fun" => InterParseRes::Fn(parse_fun(the_rest)?),
                // otherwise, parse as a normal expression
                _ => InterParseRes::Exp(parse_expr(&exp)?),
            },
            // otherwise, parse as a normal expression
            _ => InterParseRes::Exp(parse_expr(&exp)?),
        })
    };

    // do the parsing and transform output into correct shape
    stuff.iter().map(parser).collect::<ParseResult<_>>()
}

/// This trait should be used to parse the top-level file contents (as a string) into a Program
impl FromStr for Program {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // CHEAP CHEAP HACK OH GOD
        let s = format!("({s})");
        // this might not be necessary? not sure...

        let exp = sexp::parse(&s).map_err(|e| format!("Invalid S-expression syntax: {}", e))?;

        let parts = match &exp {
            Sexp::Atom(_) => vec![InterParseRes::Exp(parse_expr(&exp)?)],
            Sexp::List(l) => parse_top_level(l)?,
        };

        let (defs, top_lvl): (Vec<_>, Vec<_>) = parts
            .into_iter()
            .partition(|e| matches!(e, InterParseRes::Fn(_)));

        if top_lvl.len() != 1 {
            return Err(format!(
                "Expected exactly one top level expression, got {}",
                top_lvl.len()
            ));
        }
        let top_lvl = top_lvl.into_iter().next().unwrap();

        let mapped: Vec<_> = defs
            .into_iter()
            .map(|f| {
                if let InterParseRes::Fn(func) = f {
                    Ok(func)
                } else {
                    Err("Found expr in defs, this should never happen".to_owned())
                }
            })
            .collect::<ParseResult<_>>()?;

        if let InterParseRes::Exp(e) = top_lvl {
            Ok(Program {
                defs: mapped,
                main: e,
            })
        } else {
            Err("Found FnDef in top level expression, this should never happen".to_owned())
        }
    }
}

#[cfg(test)]
mod test {
    use super::Expr;
    use crate::{
        ast::{FnDef, Program},
        parse::ParseResult,
    };

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

    fn ebool(arg: bool) -> BE {
        Expr::from_bool(arg)
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

    fn block(stmts: Vec<BE>) -> BE {
        Expr::from_block(stmts.into_iter().map(|x| *x).collect())
    }

    fn fun(name: &str, params: &[&str], body: BE) -> FnDef {
        FnDef {
            name: name.to_owned(),
            args: params.into_iter().map(|s| s.to_string()).collect(),
            body,
        }
    }

    fn call(name: &str, params: Vec<BE>) -> BE {
        let params = params.into_iter().map(|x| *x).collect();
        Box::new(Expr::Call(name.into(), params))
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
    fn test_parse_let() {
        let input = "(let ((a 1)) a)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, elet(vec![("a", *num(1))], id("a")));

        let input = "(let ((a 1) (b 2) (c 3)) (+ a (* b (add1 c))))";
        let res: BE = input.parse().unwrap();
        let expected = elet(
            vec![("a", *num(1)), ("b", *num(2)), ("c", *num(3))],
            plus(id("a"), times(id("b"), add1(id("c")))),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parse_if() {
        let input = "(if true (+ 1 2) (- 2 1))";
        let res: BE = input.parse().unwrap();
        assert_eq!(
            res,
            eif(
                Expr::from_bool(true),
                plus(num(1), num(2)),
                minus(num(2), num(1))
            )
        );

        let input = "(if (isbool 5) (<= 1 2) (>= 2 1))";
        let res: BE = input.parse().unwrap();
        assert_eq!(
            res,
            eif(isbool(num(5)), leq(num(1), num(2)), geq(num(2), num(1)))
        );

        let input = "(if (isbool (= 1 1)) (add1 5) (sub1 7))";
        let res: BE = input.parse().unwrap();
        assert_eq!(
            res,
            eif(isbool(eq(num(1), num(1))), add1(num(5)), sub1(num(7)))
        );
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
        assert_eq!(
            res,
            eloop(eif(eq(num(1), num(2)), ebreak(num(1)), ebreak(num(2))))
        );
    }

    #[test]
    fn test_parse_set() {
        let input = "(set! hello 5)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, eset("hello", num(5)));
    }

    #[test]
    fn test_parse_one_fun_program() {
        // second line of input is so I can parse to program correctly
        // no other good way to parse fun, sadly
        let input = r#"
        (fun (testing param1 param2 param3) (+ param1 (+ param2 param3)))
        (testing 1 2 3)
        "#;
        let res: Program = input.parse().unwrap();
        let expected_defs = vec![fun(
            "testing",
            &["param1", "param2", "param3"],
            plus(id("param1"), plus(id("param2"), id("param3"))),
        )];
        assert_eq!(res.defs, expected_defs);
    }

    #[test]
    fn test_parse_two_fun_program() {
        // second line of input is so I can parse to program correctly
        // no other good way to parse fun, sadly
        let input = r#"
        (fun (testing param1 param2 param3) (+ param1 (+ param2 param3)))
        (fun (testing2 bleh1 bleh2 bleh3) (+ bleh1 (+ bleh2 bleh3)))
        (testing 1 2 3)
        "#;
        let res: Program = input.parse().unwrap();
        let expected_defs = vec![
            fun(
                "testing",
                &["param1", "param2", "param3"],
                plus(id("param1"), plus(id("param2"), id("param3"))),
            ),
            fun(
                "testing2",
                &["bleh1", "bleh2", "bleh3"],
                plus(id("bleh1"), plus(id("bleh2"), id("bleh3"))),
            ),
        ];
        assert_eq!(res.defs, expected_defs);
    }

    #[test]
    fn test_parse_one_arg_call() {
        let input = "(fib 10)";
        let res: BE = input.parse().unwrap();
        assert_eq!(res, call("fib", vec![num(10)]));
    }

    #[test]
    fn test_parse_multi_arg_call() {
        let input = "(bleh false 10 35 true)";
        let res: BE = input.parse().unwrap();
        assert_eq!(
            res,
            call("bleh", vec![ebool(false), num(10), num(35), ebool(true)])
        );
    }

    #[test]
    fn test_parse_multi_function_call() {
        let input = r#"
            (block
             (fib 10)
             (test true 2 3)
             (also true false false true false true true)
            )
        "#;
        let res: BE = input.parse().unwrap();
        assert_eq!(
            res,
            block(vec![
                call("fib", vec![num(10)]),
                call("test", vec![ebool(true), num(2), num(3)]),
                call("also", vec![ebool(true), ebool(false),ebool(false), ebool(true), ebool(false), 
                                  ebool(true), ebool(true)]),
            ])
        );
    }
}
