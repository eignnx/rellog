pub type Span<'i> = nom_locate::LocatedSpan<&'i str>;
pub type Res<'i, O = ()> = nom::IResult<Span<'i>, O>;
