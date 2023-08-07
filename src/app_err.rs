use std::fmt;

use librellog::{lex, parse};

pub type AppRes<'ts, T> = Result<T, AppErr<'ts>>;

#[derive(Debug)]
pub enum AppErr<'ts> {
    FileOpen(String, std::io::Error),
    FileRead(String, std::io::Error),
    Lex(lex::LexError),
    Parse(parse::Error<'ts>),
}

impl<'ts> From<lex::LexError> for AppErr<'ts> {
    fn from(le: lex::LexError) -> Self {
        Self::Lex(le)
    }
}

impl<'ts> From<parse::Error<'ts>> for AppErr<'ts> {
    fn from(pe: parse::Error<'ts>) -> Self {
        Self::Parse(pe)
    }
}

impl<'ts> fmt::Display for AppErr<'ts> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppErr::FileOpen(msg, io_err) => {
                write!(
                    f,
                    "Could not open file `{msg}`.\n\t(io error: {:?})",
                    io_err.kind()
                )
            }
            AppErr::FileRead(msg, io_err) => write!(
                f,
                "Could not read file `{msg}`.\n\t(io error: {:?})",
                io_err.kind()
            ),
            AppErr::Lex(le) => write!(f, "Unable to tokenize: {le}"),
            AppErr::Parse(pe) => write!(f, "Unable to parse: {pe}"),
        }
    }
}
