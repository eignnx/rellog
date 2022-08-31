use std::fmt;

use librellog::parse;

pub type AppRes<T> = Result<T, AppErr>;

#[derive(Debug)]
pub enum AppErr {
    FileOpenErr(String, std::io::Error),
    FileReadErr(String, std::io::Error),
    ParseErr(parse::AllocError),
}

impl fmt::Display for AppErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self) // Punt this off till later...
    }
}
