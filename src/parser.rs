use std::collections::HashMap;
use crate::position::*;
use crate::errors::*;
use crate::context::*;
use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum N {
    Body(Vec<Node>),
    Wildcard, Null, Int(i64), Float(f64), Bool(bool), String(String), Vector(Vec<Node>),
    Object(HashMap<Node, Node>), ID(String),
    Binary { op: T, left: Box<Node>, right: Box<Node> },
    Unary { op: T, node: Box<Node> }, Multi { op: T, nodes: Vec<Node> },
    Assign { global: bool, id: Box<Node>, expr: Box<Node> },
    OpAssign { op: T, id: Box<Node>, expr: Box<Node> },
    Inc(Box<Node>), Dec(Box<Node>),
    Call { id: Box<Node>, args: Vec<Node> }
}
impl std::fmt::Display for N {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Body(nodes) => write!(f, "\n{}\n", nodes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("\n")),
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Vector(v) => write!(f, "[{}]", v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}")).collect::<Vec<String>>().join(", ")),
            Self::ID(v) => write!(f, "{v}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op} {right}"),
            Self::Unary { op, node } => write!(f, "{op} {node}"),
            Self::Multi { op, nodes } => write!(f, "{op} {}", nodes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")),
            Self::Assign { global, id, expr } => if *global { write!(f, "global {id} = {expr}") } else { write!(f, "var {id} = {expr}") }
            Self::OpAssign { op, id, expr } => write!(f, "{id} {op} {expr}"),
            Self::Inc(id) => write!(f, "{id}++"),
            Self::Dec(id) => write!(f, "{id}--"),
            Self::Call { id, args } => write!(f, "{id}! {}", args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Layer {
    Binary(Vec<T>), UnaryLeft(Vec<T>), UnaryRight(Vec<T>),
    Atom
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
    pub fn token(&self) -> &T {
        match self.tokens.get(self.ln) {
            Some(line) => match line.get(self.col) {
                Some(token) => &token.0,
                None => &line.last().unwrap().0
            }
            None => &self.tokens.last().unwrap().last().unwrap().0
        }
    }
    pub fn pos(&self) -> &Position {
        match self.tokens.get(self.ln) {
            Some(line) => match line.get(self.col) {
                Some(token) => &token.1,
                None => &line.last().unwrap().1
            }
            None => &self.tokens.last().unwrap().last().unwrap().1
        }
    }
    pub fn advance(&mut self) { self.col += 1; }
    pub fn advance_ln(&mut self) { self.ln += 1; self.col = 0; }
    pub fn advance_expect(&mut self, token: T, context: &mut Context) -> Result<(), E> {
        if self.token() != &token {
            context.trace(self.pos().clone(), &self.path);
            return Err(E::ExpectedToken(token, self.token().clone()))
        }
        self.advance();
        Ok(())
    }
    pub fn ops(&self, layer: usize) -> Layer {
        let layers = [
            Layer::Binary(vec![T::And, T::Or, T::Xor]),
            Layer::Binary(vec![T::EQ, T::NE, T::LT, T::LE, T::GT, T::GE]),
            Layer::Binary(vec![T::Add, T::Sub]),
            Layer::Binary(vec![T::Mul, T::Div, T::Mod]),
            Layer::UnaryLeft(vec![T::Add, T::Sub]),
            Layer::UnaryLeft(vec![T::Len]),
        ];
        layers.get(layer).or_else(|| Some(&Layer::Atom)).unwrap().clone()
    }
    pub fn operation(&mut self, layer_type: Layer, layer: usize, context: &mut Context) -> Result<Node, E> {
        match layer_type {
            Layer::Binary(ops) => {
                let start = self.col;
                let mut left = self.operation(self.ops(layer + 1), layer + 1, context)?;
                while ops.contains(&self.token()) {
                    let op = self.token().clone();
                    self.advance();
                    let right = self.operation(self.ops(layer + 1), layer + 1, context)?;
                    left = Node(N::Binary{
                        op, left: Box::new(left.clone()), right: Box::new(right)
                    }, Position::new(self.ln..self.ln+1, start..self.col))
                }
                Ok(left)
            }
            Layer::UnaryLeft(ops) => {
                let start = self.col;
                if ops.contains(&self.token()) {
                    let op = self.token().clone();
                    self.advance();
                    let node = self.operation(self.ops(layer), layer, context)?;
                    return Ok(Node(N::Unary{
                        op, node: Box::new(node)
                    }, Position::new(self.ln..self.ln+1, start..self.col)))
                }
                self.operation(self.ops(layer + 1), layer + 1, context)
            }
            Layer::UnaryRight(ops) => {
                let start = self.col;
                let mut node = self.operation(self.ops(layer), layer, context)?;
                while ops.contains(&self.token()) {
                    let op = self.token().clone();
                    self.advance();
                    node = Node(N::Unary{
                        op, node: Box::new(node.clone())
                    }, Position::new(self.ln..self.ln+1, start..self.col));
                }
                Ok(node)
            }
            Layer::Atom => self.atom(context)
        }
    }
    pub fn parse(&mut self, context: &mut Context) -> Result<Node, E> {
        let mut nodes: Vec<Node> = vec![];
        while self.token() != &T::EOF {
            let node = self.stat(context)?;
            nodes.push(node);
        }
        Ok(Node(N::Body(nodes), Position::new(0..self.ln, 0..self.col)))
    }
    pub fn stat(&mut self, context: &mut Context) -> Result<Node, E> {
        let start = self.col;
        match self.token() {
            T::Var | T::Global => {
                let prefix = self.token().clone();
                self.advance();
                let id = self.atom(context)?; // field
                self.advance_expect(T::Assign, context)?;
                let expr = self.expr(context)?;
                self.advance_ln();
                Ok(Node(N::Assign {
                    global: prefix == T::Global, id: Box::new(id), expr: Box::new(expr)
                }, Position::new(self.ln..self.ln+1, start..self.col)))
            }
            T::ID(_) => {
                let id = self.atom(context)?; // field
                self.advance_expect(T::Call, context)?;
                let mut args: Vec<Node> = vec![];
                while self.token() != &T::EOL {
                    let value = self.expr(context)?;
                    args.push(value);
                }
                self.advance_ln();
                Ok(Node(N::Call {
                    id: Box::new(id), args
                }, Position::new(self.ln..self.ln+1, start..self.col)))
            }
            _ => {
                context.trace(self.pos().clone(), &self.path);
                Err(E::UnexpectedToken(self.token().clone()))
            }
        }
    }
    pub fn expr(&mut self, context: &mut Context) -> Result<Node, E> {
        self.operation(self.ops(0), 0, context)
    }
    pub fn atom(&mut self, context: &mut Context) -> Result<Node, E> {
        let ret: Result<Node, E> = match self.token() {
            T::Wildcard => Ok(Node(N::Wildcard, self.pos().clone())),
            T::Null => Ok(Node(N::Null, self.pos().clone())),
            T::Int(v) => Ok(Node(N::Int(v.clone()), self.pos().clone())),
            T::Float(v) => Ok(Node(N::Float(v.clone()), self.pos().clone())),
            T::Bool(v) => Ok(Node(N::Bool(v.clone()), self.pos().clone())),
            T::String(v) => Ok(Node(N::String(v.clone()), self.pos().clone())),
            T::ID(id) => Ok(Node(N::ID(id.clone()), self.pos().clone())),
            _ => {
                context.trace(self.pos().clone(), &self.path);
                Err(E::UnexpectedToken(self.token().clone()))
            }
        };
        self.advance();
        ret
    }
}

pub fn parse(path: &String, tokens: Vec<Vec<Token>>, context: &mut Context) -> Result<Node, E> {
    Parser::new(path, tokens).parse(context)
}