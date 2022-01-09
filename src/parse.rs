//! Lexical analysis.
use crate::{
    glue::{expect, fold_many_till, Error as Error2, IResult, Span, State},
    syntax::{AssignDef, ClassBody, ClassDef, DefStmt, Expr, Ident, KeywordType, Literal, Module},
};
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1, line_ending, multispace0, space0},
    combinator::{all_consuming, eof, map, opt},
    error::context,
    sequence::{preceded, terminated, tuple},
    InputLength,
};
use std::cell::RefCell;

/// Parser entry-point.
pub fn parse(source: &str) -> (Module, Vec<Error2>) {
    // Store our error stack external to our `nom` parser here. It
    // is wrapped in a `RefCell` so parser functions down the line
    // can remotely push errors onto it as they run.
    let errors = RefCell::new(Vec::new());

    // Mutable list of errors is smuggled into the parsers via
    // the input span.
    let input = Span::new_extra(source, State(&errors));

    // We expect the parser to consume the whole source input, and
    // produce some kind of syntax tree regardless of errors.
    //
    // If a panic occurs, it's considered a bug in the parser.
    let (rest, module) = expect(all_consuming(parse_module), "source not consumed")(input)
        .expect("parser must succeed");

    println!("{:#?}, {}", rest, rest.input_len());
    println!("{:#?}", errors);

    (module.unwrap(), errors.into_inner())

    // nom::combinator::map(
    //     nom::multi::many_till(parse_def_stmt, nom::combinator::eof),
    //     |(stmts, _rest)| Module { stmts },
    // )(input)
    // .map(|(_input, output)| output)
}

fn parse_module(input: Span) -> IResult<Module> {
    let parsers = map(
        expect(parse_def_stmt, "failed to parse definition statement"),
        |maybe_stmt| maybe_stmt.unwrap_or(DefStmt::Error),
    );

    map(
        // Keep consuming statements until end-of-file
        fold_many_till(parsers, eof, Vec::new, |mut acc: Vec<_>, stmt| {
            // Skip empty lines.
            if !matches!(stmt, DefStmt::Empty) {
                acc.push(stmt);
            }

            acc
        }),
        // many_till(
        //     map(
        //         expect(parse_def_stmt, "failed to parse definition statement"),
        //         |maybe_stmt| maybe_stmt.unwrap_or(DefStmt::Error),
        //     ),
        //     eof,
        // ),
        // |(stmts, _rest)| Module { stmts },
        |stmts| Module { stmts },
    )(input)
    // .map(|(_input, output)| output)
}

/// Definition statements.
fn parse_def_stmt(input: Span) -> IResult<DefStmt> {
    preceded(
        multispace0,
        nom::branch::alt((
            map(parse_empty_stmt, |_| DefStmt::Empty),
            map(parse_assign_def, |x| DefStmt::AssignDef(x)),
            map(parse_class_def, |x| DefStmt::Class(x)),
            // TODO: Simple Statement
        )),
    )(input)
}

fn parse_class_def(input: Span) -> IResult<ClassDef> {
    map(
        tuple((
            tag("class"),
            parse_ident,
            opt(parse_inherit),
            parse_class_body,
        )),
        |(_kw, name, parent, body)| ClassDef { name, parent, body },
    )(input)
}

fn parse_inherit(input: Span) -> IResult<Ident> {
    map(
        tuple((preceded(space0, tag("is")), parse_ident)),
        |(_kw, name)| name,
    )(input)
}

/// Parse the body of a class, including its parentheses.
fn parse_class_body(input: Span) -> IResult<ClassBody> {
    let body = tuple((
        preceded(space0, tag("{")),
        multispace0,
        preceded(space0, tag("}")),
    ));

    context(
        "class_body",
        map(body, |(_, _, _)| ClassBody {
                /* TODO: Parse methods */
    }),
    )(input)
}

fn parse_assign_def(input: Span) -> IResult<AssignDef> {
    nom::error::context(
        "assign_def",
        nom::combinator::map(
            nom::sequence::preceded(
                space0,
                nom::sequence::tuple((
                    parse_keyword(KeywordType::Var),
                    parse_ident,
                    parse_eq,
                    map(
                        expect(parse_number, "failed to parse right-hand-side"),
                        |lit| lit.map(Expr::Lit).unwrap_or(Expr::Error),
                    ),
                    parse_eos,
                )),
            ),
            |(keyword, name, _op, expr, _eos)| AssignDef {
                keyword,
                name,
                expr,
            },
        ),
    )(input)
}

/// Empty line.
fn parse_empty_stmt(input: Span) -> IResult<Span> {
    parse_eos(input)
}

/// Create a parser that will consume the given keyword
/// from the source input, and output an identifier if
/// it matches.
fn parse_keyword(keyword: KeywordType) -> impl Fn(Span) -> IResult<Ident> {
    move |input| {
        map(tag(keyword.as_str()), |name: Span| Ident {
            name: name.fragment().into(),
        })(input)
    }
}

fn parse_number(input: Span) -> IResult<Literal> {
    map(preceded(space0, digit1), |s: Span| {
        Literal::Number(s.fragment().into())
    })(input)
}

fn parse_ident(input: Span) -> IResult<Ident> {
    map(preceded(space0, alpha1), |name: Span| Ident {
        name: name.fragment().into(),
    })(input)
}

fn parse_eq(input: Span) -> IResult<Span> {
    nom::sequence::preceded(space0, tag("="))(input)
}

/// End-of-statement
///
/// When a line contains a single statement, that
/// statement must be terminated with a newline
/// character or end-of-file.
///
/// No semicolons are allowed.
fn parse_eos(input: Span) -> IResult<Span> {
    terminated(space0, nom::branch::alt((line_ending, eof)))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_assign() {
        println!("{:?}", parse("var a = 5"));
        println!("{:?}", parse("  var   b  =   7  "));
        println!(
            "{:#?}",
            parse(
                r#"
                var x = 1
                var y = !2
                "#
            )
        );
    }

    #[test]
    fn test_class_def() {
        println!(
            "{:#?}",
            parse(
                r#"
        class Bar {}

        class Foo is Bar {

        }
        "#
            )
        );
    }
}
