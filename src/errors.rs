use crate::T;

#[derive(Clone, Debug)]
pub enum E {
    TargetFile(String),
    FileNotFound(String),
    Todo(String),
    IllegalChar(String),
    UnexpectedToken(T),
    ExpectedToken(T, T),
}
impl std::fmt::Display for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TargetFile(v) => write!(f, "ERROR: target file '{v}' not found"),
            Self::FileNotFound(v) => write!(f, "ERROR: file '{v}' not found"),
            Self::Todo(v) => write!(f, "ERROR: todo - {v}"),
            Self::IllegalChar(c) => write!(f, "ERROR: illegal character {c:?}"),
            Self::UnexpectedToken(t) => write!(f, "ERROR: unexpected {}", t.name()),
            Self::ExpectedToken(t1, t2) => write!(f, "ERROR: expected {}, got {}", t1.name(), t2.name()),
        }
    }
}