//! Glue code for working with `nom`.
//!
//! See: https://eyalkalderon.com/blog/nom-error-recovery/
use nom::{error::ParseError, InputLength, Parser};
use std::{cell::RefCell, ops::Range};

/// This is used in place of `&str` or `&[u8]` in our `nom` parsers.
pub type Span<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

/// Convenient type alias for `nom::IResult<I, O>` reduced to `IResult<O>`.
pub type IResult<'a, T> = nom::IResult<Span<'a>, T, nom::error::Error<Span<'a>>>;

/// Our own trait for extracintg a range from a value.
pub trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

/// Get the Span's start and end offsets.
impl<'a> ToRange for Span<'a> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

/// Error containing a text span and an error message to display.
#[derive(Debug)]
pub struct Error(Range<usize>, String);

/// Contextual error state carried around in the `LocatedSpan::extra`
/// field in between `nom` parsers.
#[derive(Clone, Debug)]
pub struct State<'a>(pub &'a RefCell<Vec<Error>>);

impl<'a> State<'a> {
    /// Pushes an error onto the errors stack from within a `nom`
    /// parser combinator while still allowing parsing to continue.
    pub fn report_error(&self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

/// Evaluate `parser` and wrap the result in a `Some(_)`. Otherwise,
/// emit the  provided `error_msg` and return a `None` while allowing
/// parsing to continue.
pub fn expect<'a, F, M, T>(
    mut parser: F,
    error_msg: M,
) -> impl FnMut(Span<'a>) -> IResult<Option<T>>
where
    // F: Fn(LocatedSpan<'a>) -> IResult<T>,
    F: Parser<Span<'a>, T, nom::error::Error<Span<'a>>>,
    M: ToString,
{
    move |input| match parser.parse(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
            let nom::error::Error { input, .. } = err;
            let err = Error(input.to_range(), error_msg.to_string());
            input.extra.report_error(err);
            Ok((input, None))
        }
        Err(err) => Err(err),
    }
}

/// Applies the parser `f` until the parser `s` produces a result,
/// and accumulates the results of `f` using a given function `g`.
///
/// This is a combination of `fold_many0` and `many_till`, because
/// we need to fold until end-of-file, but also terminate statements
/// on end-of-file.
///
/// # Arguments
///
/// - `f` The parser to apply.
/// - `s` The parser that will stop the fold when it returns success.
/// - `init` A function returning the initial value.
/// - `g` The function that combines a result of `f` with the current accumulator.
pub fn fold_many_till<I, O, P, E, F, G, S, H, R>(
    mut f: F,
    mut s: S,
    mut init: H,
    mut g: G,
) -> impl FnMut(I) -> nom::IResult<I, R, E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    S: Parser<I, P, E>,
    G: FnMut(R, O) -> R,
    H: FnMut() -> R,
    E: ParseError<I>,
{
    move |i: I| {
        let mut res = init();
        let mut input = i;

        loop {
            let i_ = input.clone();
            let len = input.input_len();
            match s.parse(i_.clone()) {
                Ok((i, _)) => return Ok((i, res)),
                Err(nom::Err::Error(_)) => {
                    match f.parse(i_) {
                        Ok((i, o)) => {
                            if i.input_len() == len {
                                // infinite loop check: the parser must always consume
                                return Err(nom::Err::Error(E::from_error_kind(
                                    input,
                                    nom::error::ErrorKind::Many0,
                                )));
                            }

                            res = g(res, o);
                            input = i;
                        }
                        Err(nom::Err::Error(_)) => {
                            return Ok((input, res));
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fold_many_till() {
        use nom::{bytes::complete::tag, IResult};

        fn parser(s: &str) -> IResult<&str, Vec<&str>> {
            fold_many_till(tag("abc"), tag("end"), Vec::new, |mut acc: Vec<_>, item| {
                acc.push(item);
                acc
            })(s)
        }

        assert_eq!(parser("abcabcend"), Ok(("", vec!["abc", "abc"])));
        assert_eq!(parser("abc123end"), Ok(("123end", vec!["abc"])));
        assert_eq!(parser("123123end"), Ok(("123123end", vec![])));
        assert_eq!(parser("end"), Ok(("", vec![])));
        assert_eq!(parser(""), Ok(("", vec![])));
    }
}
