use nom::{error::VerboseError, Parser};

pub type Span<'i> = nom_locate::LocatedSpan<&'i str>;
pub type PErr<'i> = VerboseError<Span<'i>>;
pub type Res<'i, O = ()> = nom::IResult<Span<'i>, O, PErr<'i>>;

pub trait MyParser<'i, O>: Parser<Span<'i>, O, PErr<'i>> {}
impl<'i, O, P> MyParser<'i, O> for P where P: Parser<Span<'i>, O, PErr<'i>> {}
