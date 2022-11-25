use std::collections::HashMap;
use crate::position::*;
use crate::errors::*;
use crate::context::*;
use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum N {
    Wildcard, Null, Int(i64), Float(f64), Bool(bool), String(String), Vector(Vec<Node>),
    Object(HashMap<Node, Node>), ID(String),
    Binary { op: Token, left: Box<Node>, right: Box<Node> },
    Unary { op: Token, node: Box<Node> }, Multi { op: Token, nodes: Vec<Node> },
    Assign { global: bool, id: Box<Node>, expr: Box<Node> },
    OpAssign { op: Token, id: Box<Node>, expr: Box<Node> },
    Inc(Box<Node>), Dec(Box<Node>),
}
impl std::fmt::Display for N {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Vector(v) => write!(f, "[{}]", v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}")).collect::<Vec<String>>().join(", ")),
            Self::ID(v) => write!(f, "{v}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op:?} {right}"),
            Self::Unary { op, node } => write!(f, "{op:?} {node}"),
            Self::Multi { op, nodes } => write!(f, "{op:?} {}", nodes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")),
            Self::Assign { global, id, expr } => if *global { write!(f, "global {id} = {expr}") } else { write!(f, "{id} = {expr}") }
            Self::OpAssign { op, id, expr } => write!(f, "{id} {op:?} {expr}"),
            Self::Inc(id) => write!(f, "{id}++"),
            Self::Dec(id) => write!(f, "{id}--"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Node(N, Position);
impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.0)
    }
}

pub struct Parser {
    tokens: Vec<Vec<Token>>,
    path: String,
    col: usize,
    ln: usize
}
impl Parser {
    pub fn new(path: &String, tokens: Vec<Vec<Token>>) -> Self {
        Self { tokens, path: path.clone(), col: 0, ln: 0 }
    }
    pub fn parse(&mut self, context: &mut Context) -> Result<Node, E> {
        Ok(Node(N::Null, Position::new(0..1, 0..1)))
    }
}

pub fn parse(path: &String, tokens: Vec<Vec<Token>>, context: &mut Context) -> Result<Node, E> {
    Parser::new(path, tokens).parse(context)
}