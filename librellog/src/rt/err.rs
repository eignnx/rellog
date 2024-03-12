use std::fmt;

use crate::{
    ast::{RcTm, Sig, Tm},
    lex, parse, tm,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
    InstantiationError {
        rel: String,
        tm: RcTm,
    },
    NoSuchRelation(Sig),
    NoSuchMacro(Sig),
    ArgumentTypeError {
        rel: String,
        /// The attribute key that the incorrect value was passed to.
        /// Example: `[pred "adsf"][Succ]` -> key = "pred".
        key: String,
        expected_ty: String,
        recieved_tm: String,
    },
    GenericError {
        rel: String,
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
    ParseError(String),
    LexError(String),
    MacroExpansionError {
        macro_name: String,
        err: Box<Err>,
    },
    MacroImplemenationError {
        macro_name: String,
        msg: String,
    },
}

impl fmt::Display for Err {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Err::AttemptToQueryNonCallable(tm) => {
                write!(f, "The term `{tm}` is not callable.")
            }
            Err::InstantiationError { rel, tm } => {
                write!(
                    f,
                    "In `{rel}`: The term `{tm}` is not sufficiently instantiated."
                )
            }
            Err::NoSuchRelation(sig) => {
                write!(f, "No relation exists with signature `{sig}`.")
            }
            Err::NoSuchMacro(sig) => {
                write!(f, "No macro could be found with signature `[{sig}]`.")
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
                     `{expected_ty}` argument, but received the term `{recieved_tm}`."
                )
            }
            Err::GenericError { rel, msg } => {
                write!(f, "Generic error from `{rel}`: {msg}")
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
            Err::ParseError(msg) => write!(f, "{msg}"),
            Err::LexError(msg) => write!(f, "{msg}"),
            Err::MacroExpansionError { macro_name, err } => {
                write!(
                    f,
                    "Error during macro expansion of directive `[{macro_name}]`:\n\t- {err}"
                )
            }
            Err::MacroImplemenationError { macro_name, msg } => {
                write!(
                    f,
                    "Error in implementation of macro `{macro_name}`:\n\t- {msg}"
                )
            }
        }
    }
}

impl From<Err> for Tm {
    fn from(err: Err) -> Self {
        match err {
            Err::AttemptToQueryNonCallable(tm) => tm!([attempt_to_call_non_callable tm]),
            Err::InstantiationError { rel, tm } => {
                tm!([instantiation_error tm!([rel RcTm::sym(rel)][tm tm]).into()])
            }
            Err::NoSuchRelation(sig) => tm!([no_such_relation sig.into()]),
            Err::NoSuchMacro(sig) => tm!([no_such_macro sig.into()]),
            Err::ArgumentTypeError {
                rel,
                key,
                expected_ty,
                recieved_tm,
            } => {
                tm!([argument_type_error tm!(
                    [rel
                        RcTm::sym(rel)
                    ][key
                        RcTm::sym(key)
                    ][expected_ty
                        RcTm::sym(expected_ty)
                    ][recieved_tm
                        RcTm::sym(recieved_tm)
                    ]
                ).into()])
            }
            Err::GenericError { rel, msg } => tm!([generic_error
                tm!([rel RcTm::sym(rel)][msg RcTm::sym(msg)]).into()
            ]),
            Err::UnexpectedPartialList { rel, key, partial } => tm!([unexpected_partial_list tm!(
                [rel
                    RcTm::sym(rel)
                ][key
                    RcTm::sym(key)
                ][partial
                    partial
                ]
            ).into()]),
            Err::IoError(msg) => tm!([io_error RcTm::sym(msg)]),
            Err::MaxRecursionDepthExceeded { depth, query } => {
                tm!([max_recursion_depth_exceeded tm!(
                    [depth
                        Tm::Int(depth.into()).into()
                    ][query query]
                ).into()])
            }
            Err::ParseError(e) => tm!([parse_error RcTm::sym(e)]),
            Err::LexError(e) => tm!([lex_error RcTm::sym(e)]),
            Err::MacroExpansionError { macro_name, err } => tm!([macro_expansion_error tm!(
                [macro_name
                    RcTm::sym(macro_name)
                ][err
                    Tm::from(err.as_ref().clone()).into()
                ]
            ).into()]),
            Err::MacroImplemenationError { macro_name, msg } => {
                tm!([macro_implementation_error tm!(
                [macro_name
                    RcTm::sym(macro_name)
                ][msg
                    RcTm::sym(msg)
                ]
            ).into()])
            }
        }
    }
}

impl From<std::io::Error> for Err {
    fn from(err: std::io::Error) -> Self {
        Err::IoError(err.to_string())
    }
}

impl<'a> From<parse::Error<'a>> for Err {
    fn from(e: parse::Error) -> Self {
        Err::ParseError(e.to_string())
    }
}

impl From<lex::LexError> for Err {
    fn from(e: lex::LexError) -> Self {
        Err::LexError(e.to_string())
    }
}
