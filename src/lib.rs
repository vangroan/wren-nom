mod glue;
mod parse;
mod syntax;

pub use self::parse::parse;

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;
