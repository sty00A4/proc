use crate::*;

#[derive(Clone, Debug)]
pub enum E {
    TargetFile(String),
    FileNotFound(String),
    Todo(String),
    IllegalChar(String),
    UnexpectedToken(T),
    ExpectedToken(T, T),
    ExpectedType(Type, Type),
    ExpectedTypeArg(String, Type, Type),
    ExpectedNode(N, N),
    Binary(T, V, V),
    InvalidBinaryOp(T),
    Unary(T, V),
    InvalidUnaryOp(T),
    CannotAssign(N),
    NotDefined(String),
    Cast(Type, V),
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
            Self::ExpectedType(t1, t2) => write!(f, "ERROR: expected {t1}, got {t2}"),
            Self::ExpectedTypeArg(arg, t1, t2) => write!(f, "ERROR: expected {t1} for #{arg} argument, got {t2}"),
            Self::ExpectedNode(n1, n2) => write!(f, "ERROR: expected {}, got {}", n1.name(), n2.name()),
            Self::Binary(op, left, right) => write!(f, "ERROR: cannot perform {} on {} and {}", op.name(), left.typ(), right.typ()),
            Self::InvalidBinaryOp(op) => write!(f, "ERROR: invalid binary operator {}", op.name()),
            Self::Unary(op, v) => write!(f, "ERROR: cannot perform {} on {}", op.name(), v.typ()),
            Self::InvalidUnaryOp(op) => write!(f, "ERROR: invalid unary operator {}", op.name()),
            Self::CannotAssign(id) => write!(f, "ERROR: cannot assign value to {}", id.name()),
            Self::NotDefined(id) => write!(f, "ERROR: {id} is not defined"),
            Self::Cast(typ, v) => write!(f, "ERROR: cannot cast {v:?} to {typ}"),
        }
    }
}