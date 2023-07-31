use std::fmt;

use librellog::parse;

pub type AppRes<'ts, T> = Result<T, AppErr<'ts>>;

#[derive(Debug)]
pub enum AppErr<'ts> {
    FileOpen(String, std::io::Error),
    FileRead(String, std::io::Error),
    Parse(parse::Error<'ts>),
}

impl<'ts> fmt::Display for AppErr<'ts> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppErr::FileOpen(msg, io_err) => {
                write!(
                    f,
                    "Could not open file {msg}. (io error {:?})",
                    io_err.kind()
                )
            }
            AppErr::FileRead(msg, io_err) => write!(
                f,
                "Could not read file {msg}. (io error {:?})",
                io_err.kind()
            ),
            AppErr::Parse(pe) => write!(f, "{pe}"),
        }
    }
}
