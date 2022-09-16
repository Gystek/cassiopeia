use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub struct Error {
    pub file: String,
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub msg: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (startl, startc) = self.start;
        let (endl, endc) = self.end;

        write!(f, "\x1b[1;31merror\x1b[0m: {}:", self.file)?;

        if self.start == self.end {
            write!(f, "{}.{}: ", startl, startc)
        } else {
            write!(f, "{}.{}-{}.{}: ", startl, startc, endl, endc)
        }?;

        write!(f, "{}", self.msg)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! errorm {
    ($file:expr, $line:expr, $col:expr, $($arg:tt)*) => {{
        let pos = ($line, $col);
        error!($file, pos, pos, $($arg)*)
    }}
}

#[macro_export]
macro_rules! error {
    ($file:expr, $start:expr, $end:expr, $($arg:tt)*) => {{
        std::result::Result::Err($crate::errors::Error {
            file: $file,
            start: $start,
            end: $end,
            msg: format_args!($($arg)*).to_string(),
        })
    }}
}
