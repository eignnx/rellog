use std::{fmt, path::PathBuf};

use librellog::{lex, parse};

pub type AppRes<'ts, T> = Result<T, AppErr<'ts>>;

#[derive(Debug)]
pub enum AppErr<'ts> {
    FileOpen(String, std::io::Error),
    FileRead(String, std::io::Error),
    Lex {
        err: lex::LexError,
    },
    Parse {
        fname: Option<PathBuf>,
        err: parse::Error<'ts>,
    },
    #[allow(unused)]
    IoError(Box<dyn std::error::Error>),
}

impl<'ts> From<lex::LexError> for AppErr<'ts> {
    fn from(le: lex::LexError) -> Self {
        Self::Lex { err: le }
    }
}

impl<'ts> From<(Option<PathBuf>, parse::Error<'ts>)> for AppErr<'ts> {
    fn from((fname, pe): (Option<PathBuf>, parse::Error<'ts>)) -> Self {
        Self::Parse { fname, err: pe }
    }
}

impl<'ts> fmt::Display for AppErr<'ts> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppErr::FileOpen(msg, io_err) => {
                write!(
                    f,
                    "Could not open file `{msg}`.\n(io error: {:?})",
                    io_err.kind()
                )
            }
            AppErr::FileRead(msg, io_err) => write!(
                f,
                "Could not read file `{msg}`.\n(io error: {:?})",
                io_err.kind()
            ),
            AppErr::Lex { err } => write!(f, "Unable to tokenize:\n{}", err),
            AppErr::Parse { fname, err } => {
                if let Some(fname) = fname {
                    err.fname.borrow_mut().replace(fname.clone());
                }
                write!(f, "Unable to parse:\n{}", err)
            }
            AppErr::IoError(e) => write!(f, "An IO operation failed:\n{e}"),
        }
    }
}
