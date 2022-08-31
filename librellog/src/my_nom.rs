use nom::error::VerboseError;

pub type Span<'i> = nom_locate::LocatedSpan<&'i str>;
pub type PErr<'i> = VerboseError<Span<'i>>;
pub type Res<'i, O = ()> = nom::IResult<Span<'i>, O, PErr<'i>>;
