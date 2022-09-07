//! Goal right now:
//! Parse very simple versions of Haskell's `do` expressions.
//!
//! ```ignore
//! expr --> 'ident'>
//! expr --> 'do'> |stmts|>
//!
//! stmts --> |expr|=
//! stmts --> stmts= |stmt|=
//!
//! stmt --> 'ident'> '='> expr=
//! ```
//!

use nom::{
    error::{VerboseError, VerboseErrorKind},
    IResult,
};
use nom_indentation::{I9nParser, WithI9n};
use nom_locate::LocatedSpan;
