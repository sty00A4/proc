use std::fs::read_to_string;
use crate::*;

#[derive(Clone, Debug)]
pub enum E {
    Todo(String),
    TargetFile(String), FileNotFound(String),
    IllegalChar(String),
    UnexpectedToken(T), ExpectedToken(T, T),
    ExpectedType(Type, Type), ExpectedTypeArg(String, Type, Type),
    ExpectedNode(N, N),
    Binary(T, V, V), InvalidBinaryOp(T),
    Unary(T, V), InvalidUnaryOp(T),
    CannotAssign(N), NotDefined(String), AlreadyDefined(String),
    Cast(Type, V),
    InvalidIterator(Type),
    Rule(V, String), RuleCast(V, String),
    InvalidField(Type, Type), InvalidHead(Type), FieldNotFound(String),
    IndexRange(usize, i64),
    Assertion, Test
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
            Self::AlreadyDefined(id) => write!(f, "ERROR: {id} is already defined"),
            Self::Cast(typ, v) => write!(f, "ERROR: cannot cast {v:?} to {typ}"),
            Self::InvalidIterator(typ) => write!(f, "ERROR: cannot iterate over {typ}"),
            Self::Rule(v, name) => write!(f, "ERROR: rule {name} does not apply on {v:?}"),
            Self::RuleCast(v, name) => write!(f, "ERROR: cannot cast {v:?} to {name}"),
            Self::InvalidField(head, field) => write!(f, "ERROR: cannot index {head} by {field}"),
            Self::InvalidHead(head) => write!(f, "ERROR: cannot index {head}"),
            Self::FieldNotFound(field) => write!(f, "ERROR: field {field} not found"),
            Self::IndexRange(max, index) => write!(f, "ERROR: index {index} out of range of {max}"),
            Self::Assertion => write!(f, "ERROR: assertion failed"),
            Self::Test => write!(f, "ERROR: test proc not found"),
        }
    }
}

pub fn get_line(pos: &Position, path: &String) -> String {
    match read_to_string(path) {
        Ok(text) => match text.split("\n").collect::<Vec<&str>>().get(pos.0.start..pos.0.end) {
            Some(line) => line.join("\n"),
            None => match text.split("\n").collect::<Vec<&str>>().last() {
                Some(line) => line.to_string(),
                None => "".into()
            }
        }
        Err(e) => "FILE NOT FOUND".into()
    }
}

pub fn display_trace(trace: Trace) -> String {
    let mut s = String::new();
    for (pos, path) in trace.iter() {
        s.push_str("in ");
        s.push_str(path.as_str());
        s.push_str(":");
        s.push_str(format!("{}", pos.0.start + 1).as_str());
        s.push_str(":");
        s.push_str(format!("{}", pos.1.start + 1).as_str());
        s.push_str("\n");
        s.push_str(get_line(pos, path).as_str());
        s.push_str("\n");
    }
    s
}