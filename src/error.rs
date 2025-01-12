use std::error::Error as StdError;
use std::fmt;
use std::result::Result as StdResult;

pub type Result<T = ()> = StdResult<T, Box<Error>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind {
    MissingRootElement,
    UnclosedRootElement,
    UnexpectedCloseElement,
    TooManyNodes,
    TooManyAttributes,
    TooManyNamespaces,
    TooManyEntityReferences,
    ExpectedLiteral(&'static str),
    ExpectedSpace,
    ExpectedQuote,
    ExpectedEnd,
    InvalidSyntax,
    InvalidName,
    InvalidReference(String),
    UnknownNamespace(String),
    UnknownEntity(String),
}

impl<T> From<ErrorKind> for Result<T> {
    #[cold]
    #[inline(never)]
    fn from(kind: ErrorKind) -> Self {
        Err(Box::new(Error { kind, pos: None }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    pub kind: ErrorKind,
    pub pos: Option<(usize, usize)>,
}

impl StdError for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ErrorKind::MissingRootElement => fmt.write_str("missing root element")?,
            ErrorKind::UnclosedRootElement => fmt.write_str("unclosed root element")?,
            ErrorKind::UnexpectedCloseElement => fmt.write_str("unexpected close element")?,
            ErrorKind::TooManyNodes => fmt.write_str("too many nodes")?,
            ErrorKind::TooManyAttributes => fmt.write_str("too many attributes")?,
            ErrorKind::TooManyNamespaces => fmt.write_str("too many namespaces")?,
            ErrorKind::TooManyEntityReferences => fmt.write_str("too many entity references")?,
            ErrorKind::ExpectedLiteral(literal) => write!(fmt, "expected literal `{literal}`")?,
            ErrorKind::ExpectedSpace => fmt.write_str("expected whitespace")?,
            ErrorKind::ExpectedQuote => fmt.write_str("expected single or double quote")?,
            ErrorKind::ExpectedEnd => fmt.write_str("expected end")?,
            ErrorKind::InvalidSyntax => fmt.write_str("invalid syntax")?,
            ErrorKind::InvalidName => fmt.write_str("invalid name")?,
            ErrorKind::InvalidReference(value) => write!(fmt, "invalid reference `{value}`")?,
            ErrorKind::UnknownNamespace(prefix) => write!(fmt, "unknown namespace `{prefix}`")?,
            ErrorKind::UnknownEntity(name) => write!(fmt, "unknown entity `{name}`")?,
        }

        if let Some((line, pos)) = self.pos {
            write!(fmt, " at {line}:{pos}")?;
        }

        Ok(())
    }
}
