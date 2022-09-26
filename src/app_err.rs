use std::fmt;

use librellog::parse;

pub type AppRes<'ts, T> = Result<T, AppErr<'ts>>;

#[derive(Debug)]
pub enum AppErr<'ts> {
    FileOpenErr(String, std::io::Error),
    FileReadErr(String, std::io::Error),
    ParseErr(parse::Error<'ts>),
}

impl<'ts> fmt::Display for AppErr<'ts> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self) // Punt this off till later...
    }
}
