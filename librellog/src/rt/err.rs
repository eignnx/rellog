use std::fmt;

use crate::ast::{RcTm, Sig};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
    InstantiationError(RcTm),
    NoSuchRelation(Sig),
    ArgumentTypeError {
        rel: String,
        /// The attribute key that the incorrect value was passed to.
        /// Example: `[pred "adsf"][Succ]` -> key = "pred".
        key: String,
        expected_ty: String,
        recieved_tm: String,
    },
    GenericError {
        msg: String,
    },
    UnexpectedPartialList {
        rel: String,
        key: String,
        partial: RcTm,
    },
    IoError(String),
    MaxRecursionDepthExceeded {
        depth: usize,
        query: RcTm,
    },
}

impl fmt::Display for Err {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Err::AttemptToQueryNonCallable(tm) => {
                write!(f, "The term `{tm}` is not callable.")
            }
            Err::InstantiationError(tm) => {
                write!(f, "The term `{tm}` is not sufficiently instantiated.")
            }
            Err::NoSuchRelation(sig) => {
                write!(f, "No relation exists with signature `{sig}`.")
            }
            Err::ArgumentTypeError {
                rel,
                key,
                expected_ty,
                recieved_tm,
            } => {
                write!(
                    f,
                    "The `{key}` key of the relation `{rel}` expected a \
                     `{expected_ty}` argument, but received: {recieved_tm}."
                )
            }
            Err::GenericError { msg } => {
                write!(f, "Generic error: {msg}")
            }
            Err::UnexpectedPartialList { rel, key, partial } => {
                write!(f, "The relation `{rel}` received a partial list for it's `{key}` argument: {partial}")
            }
            Err::IoError(err) => f.write_str(err),
            Err::MaxRecursionDepthExceeded { depth, query } => {
                write!(
                    f,
                    "Max recursion depth ({depth}) exceeded for query `{query}`.",
                )
            }
        }
    }
}

impl From<std::io::Error> for Err {
    fn from(err: std::io::Error) -> Self {
        Err::IoError(err.to_string())
    }
}
