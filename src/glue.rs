//! Glue code for working with `nom`.
//!
//! See: https://eyalkalderon.com/blog/nom-error-recovery/
use nom::Parser;
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
