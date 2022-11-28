use std::collections::HashMap;
use crate::position::*;
use crate::errors::*;
use crate::context::*;
use crate::value::*;
use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum N {
    Body(Vec<Node>),
    Wildcard, Null, Int(i64), Float(f64), Bool(bool), String(String),
    Vector(Vec<Node>), Object(Vec<(Node, Node)>), ID(String), Type(Type),
    Binary { op: T, left: Box<Node>, right: Box<Node> },
    Unary { op: T, node: Box<Node> }, Multi { op: T, nodes: Vec<Node> },
    Assign { global: bool, id: Box<Node>, expr: Box<Node> }, OpAssign { op: T, id: Box<Node>, expr: Box<Node> },
    Inc(Box<Node>), Dec(Box<Node>),
    Return(Box<Node>), Break, Continue,
    Call { id: Box<Node>, args: Vec<Node> },
    If { cond: Box<Node>, body: Box<Node>, else_body: Option<Box<Node>> }, While { cond: Box<Node>, body: Box<Node> },
    Proc { name: Box<Node>, params: Vec<(Node, Option<Node>, Option<Node>)>, body: Box<Node> }
}
impl std::fmt::Display for N {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Body(nodes) => write!(f, "{}", nodes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("; ")),
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Vector(v) => write!(f, "[{}]", v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}")).collect::<Vec<String>>().join(", ")),
            Self::ID(v) => write!(f, "{v}"),
            Self::Type(v) => write!(f, "{v}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op} {right}"),
            Self::Unary { op, node } => write!(f, "{op} {node}"),
            Self::Multi { op, nodes } => write!(f, "{op} {}", nodes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")),
            Self::Assign { global, id, expr } => if *global { write!(f, "global {id} = {expr}") } else { write!(f, "var {id} = {expr}") }
            Self::OpAssign { op, id, expr } => write!(f, "{id} {op} {expr}"),
            Self::Inc(id) => write!(f, "{id}++"),
            Self::Dec(id) => write!(f, "{id}--"),
            Self::Return(node) => write!(f, "return {node}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Dec(id) => write!(f, "{id}--"),
            Self::Call { id, args } => write!(f, "{id}! {}", args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" ")),
            Self::If { cond, body, else_body } => match else_body {
                Some(else_body) => write!(f, "if {cond} {body} else {else_body}"),
                None => write!(f, "if {cond} {body}")
            },
            Self::While { cond, body } => write!(f, "while {cond} {body}"),
            Self::Proc { name, params, body } => write!(f, "proc {name} <- {} {body}",
            params.iter().map(|(id, typ, default)|
                match typ {
                    Some(typv) => match default {
                        Some(defaultv) => format!("{id} : {typv} = {defaultv}"),
                        None => format!("{id} : {typv}")
                    }
                    None => match default {
                        Some(defaultv) => format!("{id} = {defaultv}"),
                        None => format!("{id}")
                    }
                }
            ).collect::<Vec<String>>().join(" ")),
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
impl Node {
    pub fn display(&self, indent: usize) -> String {
        let s = String::from("    ").repeat(indent);
        match &self.0 {
            N::Body(nodes) => format!("{}", nodes.iter().map(|x| format!("{}", x.display(indent))).collect::<Vec<String>>().join("\n")),
            N::Wildcard => format!("_"),
            N::Null => format!("null"),
            N::Int(v) => format!("{v:?}"),
            N::Float(v) => format!("{v:?}"),
            N::Bool(v) => format!("{v:?}"),
            N::String(v) => format!("{v:?}"),
            N::Vector(v) => format!("[{}]", v.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(", ")),
            N::Object(v) => format!("{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}")).collect::<Vec<String>>().join(", ")),
            N::ID(v) => format!("{v}"),
            N::Type(v) => format!("{v}"),
            N::Binary { op, left, right } => format!("{} {op} {}", left.display(indent), right.display(indent)),
            N::Unary { op, node } => format!("{op} {}", node.display(indent)),
            N::Multi { op, nodes } => format!("{op} {}", nodes.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(" ")),
            N::Assign { global, id, expr } => if *global { format!("{s}global {} = {}", id.display(indent), expr.display(indent)) } else { format!("{s}var {} = {}", id.display(indent), expr.display(indent)) }
            N::OpAssign { op, id, expr } => format!("{s}{} {op} {}", id.display(indent), expr.display(indent)),
            N::Inc(id) => format!("{s}{}++", id.display(indent)),
            N::Dec(id) => format!("{s}{}--", id.display(indent)),
            N::Return(node) => format!("{s}return {}", node.display(indent)),
            N::Break => format!("{s}break"),
            N::Continue => format!("{s}continue"),
            N::Call { id, args } => format!("{s}{}! {}", id.display(indent), args.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(" ")),
            N::If { cond, body, else_body } => match else_body {
                Some(else_body) => format!("{s}if {} \n{}\n{s}else\n{}", cond.display(indent), body.display(indent + 1), else_body.display(indent + 1)),
                None => format!("if {} \n{}\n", cond.display(indent), body.display(indent + 1))
            },
            N::While { cond, body } => format!("while {} \n{}\n", cond.display(indent), body.display(indent + 1)),
            N::Proc { name, params, body } => format!("proc {} <- {} \n{}\n", name.display(indent),
            params.iter().map(|(id, typ, default)|
                match typ {
                    Some(typv) => match default {
                        Some(defaultv) => format!("{} : {} = {}", id.display(indent), typv.display(indent), defaultv.display(indent)),
                        None => format!("{} : {}", id.display(indent), typv.display(indent))
                    }
                    None => match default {
                        Some(defaultv) => format!("{} = {}", id.display(indent), defaultv.display(indent)),
                        None => format!("{}", id.display(indent))
                    }
                }
            ).collect::<Vec<String>>().join(" "), body.display(indent + 1)),
        }
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
    ln: usize,
    layers: Vec<Layer>
}
impl Parser {
    pub fn new(path: &String, tokens: Vec<Vec<Token>>) -> Self {
        Self {
            tokens, path: path.clone(), col: 0, ln: 0,
            layers: vec![
                Layer::Binary(vec![T::And, T::Or, T::Xor]),
                Layer::Binary(vec![T::EQ, T::NE, T::LT, T::LE, T::GT, T::GE]),
                Layer::Binary(vec![T::Add, T::Sub]),
                Layer::Binary(vec![T::Mul, T::Div, T::Mod]),
                Layer::UnaryLeft(vec![T::Add, T::Sub]),
                Layer::UnaryLeft(vec![T::Len]),
                Layer::UnaryRight(vec![T::Safe]),
                Layer::Binary(vec![T::Option]),
                Layer::Binary(vec![T::Field]),
            ]
        }
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
            context.trace(self.pos().to_owned(), &self.path);
            return Err(E::ExpectedToken(token, self.token().to_owned()))
        }
        self.advance();
        Ok(())
    }
    pub fn expect(&mut self, token: T, context: &mut Context) -> Result<(), E> {
        if self.token() != &token {
            context.trace(self.pos().to_owned(), &self.path);
            return Err(E::ExpectedToken(token, self.token().to_owned()))
        }
        Ok(())
    }
    pub fn ops(&self, layer: usize) -> Layer {
        self.layers.get(layer).or_else(|| Some(&Layer::Atom)).unwrap().clone()
    }
    pub fn operation(&mut self, layer_type: Layer, layer: usize, context: &mut Context) -> Result<Node, E> {
        match layer_type {
            Layer::Binary(ops) => {
                let start = self.col;
                let mut left = self.operation(self.ops(layer + 1), layer + 1, context)?;
                while ops.contains(&self.token()) {
                    let op = self.token().to_owned();
                    self.advance();
                    let right = self.operation(self.ops(layer + 1), layer + 1, context)?;
                    let n = match &left.0 {
                        N::Binary { op: op_, left: left_, right: right_ } if &op == op_ => 
                        N::Multi { op, nodes: vec![left_.as_ref().clone(), right_.as_ref().clone(), right] },

                        N::Multi { op: op_, nodes: nodes_ } if &op == op_ => {
                            let mut new_nodes = nodes_.clone();
                            new_nodes.push(right);
                            N::Multi { op, nodes: new_nodes }
                        }
                        _ => N::Binary {
                            op, left: Box::new(left.clone()), right: Box::new(right)
                        }
                    };
                    left = Node(n, Position::new(self.ln..self.ln+1, start..self.col))
                }
                Ok(left)
            }
            Layer::UnaryLeft(ops) => {
                let start = self.col;
                if ops.contains(&self.token()) {
                    let op = self.token().to_owned();
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
                let mut node = self.operation(self.ops(layer + 1), layer + 1, context)?;
                while ops.contains(&self.token()) {
                    let op = self.token().to_owned();
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
            if self.token() == &T::EOL { self.advance_ln(); continue }
            let node = self.stat(0, context)?;
            nodes.push(node);
        }
        Ok(Node(N::Body(nodes), Position::new(0..self.ln, 0..self.col)))
    }
    pub fn stat(&mut self, start_indent: u16, context: &mut Context) -> Result<Node, E> {
        let start = self.col;
        let mut indent: u16 = start_indent;
        if let T::Indent(i) = self.token() { indent += *i; self.advance(); }
        match self.token() {
            T::Var | T::Global => {
                let prefix = self.token().to_owned();
                self.advance();
                let id = self.operation(self.layers.last().unwrap().clone(), self.layers.len()-1, context)?;
                self.advance_expect(T::Assign, context)?;
                let expr = self.expr(context)?;
                self.advance_ln();
                Ok(Node(N::Assign {
                    global: prefix == T::Global, id: Box::new(id), expr: Box::new(expr)
                }, Position::new(self.ln..self.ln+1, start..self.col)))
            }
            T::ID(_) => {
                let id = self.operation(self.layers.last().unwrap().clone(), self.layers.len()-1, context)?;
                if [T::AddAssign, T::SubAssign, T::MulAsssign, T::DivAssign, T::ModAssign].contains(&self.token()) {
                    let op = self.token().to_owned();
                    self.advance();
                    let expr = self.expr(context)?;
                    self.advance_ln();
                    return Ok(Node(N::OpAssign {
                        op, id: Box::new(id), expr: Box::new(expr)
                    }, Position::new(self.ln..self.ln+1, start..self.col)))
                }
                if self.token() == &T::Inc {
                    self.advance_ln();
                    return Ok(Node(N::Inc(Box::new(id)), Position::new(self.ln..self.ln+1, start..self.col)))
                }
                if self.token() == &T::Dec {
                    self.advance_ln();
                    return Ok(Node(N::Dec(Box::new(id)), Position::new(self.ln..self.ln+1, start..self.col)))
                }
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
            T::If => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance();
                let cond = self.expr(context)?;
                self.expect(T::EOL, context)?;
                self.advance_ln();
                let mut nodes: Vec<Node> = vec![];
                let (body_start_ln, body_start_col) = (self.ln, self.col);
                while let T::Indent(i) = self.token() {
                    if *i <= indent { break }
                    let node = self.stat(0, context)?;
                    nodes.push(node);
                }
                let body = Node(N::Body(nodes), Position::new(body_start_ln..self.ln, body_start_col..self.col));
                let mut else_indent = indent;
                if let T::Indent(i) = self.token() { else_indent = *i; self.advance(); }
                let mut else_body: Option<Box<Node>> = None;
                if self.token() == &T::Else {
                    self.advance();
                    if self.token() == &T::If {
                        else_body = Some(Box::new(self.stat(else_indent, context)?));
                    } else {
                        self.advance_ln();
                        let mut else_nodes: Vec<Node> = vec![];
                        let (else_start_ln, else_start_col) = (self.ln, self.col);
                        while let T::Indent(i) = self.token() {
                            if *i <= indent { break }
                            let node = self.stat(0, context)?;
                            else_nodes.push(node);
                        }
                        else_body = Some(Box::new(Node(N::Body(else_nodes), Position::new(else_start_ln..self.ln, else_start_col..self.col))));
                    }
                }
                Ok(Node(N::If {
                    cond: Box::new(cond), body: Box::new(body), else_body: else_body
                }, Position::new(start_ln..self.ln, start_col..self.col)))
            }
            T::While => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance();
                let cond = self.expr(context)?;
                self.expect(T::EOL, context)?;
                self.advance_ln();
                let mut nodes: Vec<Node> = vec![];
                let (body_start_ln, body_start_col) = (self.ln, self.col);
                while let T::Indent(i) = self.token() {
                    if *i <= indent { break }
                    let node = self.stat(0, context)?;
                    nodes.push(node);
                }
                let body = Node(N::Body(nodes), Position::new(body_start_ln..self.ln, body_start_col..self.col));
                Ok(Node(N::While {
                    cond: Box::new(cond), body: Box::new(body)
                }, Position::new(start_ln..self.ln, start_col..self.col)))
            }
            T::Return => {
                let start = self.col;
                self.advance();
                let node = self.expr(context)?;
                self.expect(T::EOL, context)?;
                self.advance_ln();
                Ok(Node(N::Return(Box::new(node)), Position::new(self.ln..self.ln+1, start..self.col)))
            }
            T::Break => {
                let node = Node(N::Break, self.pos().to_owned());
                self.advance();
                self.expect(T::EOL, context)?;
                self.advance_ln();
                Ok(node)
            }
            T::Continue => {
                let node = Node(N::Continue, self.pos().to_owned());
                self.advance();
                self.expect(T::EOL, context)?;
                self.advance_ln();
                Ok(node)
            }
            T::Proc => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance();
                let name = self.atom(context)?;
                let mut params: Vec<(Node, Option<Node>, Option<Node>)> = vec![];
                if self.token() == &T::In {
                    self.advance();
                    while self.token() != &T::EOL {
                        let mut typ: Option<Node> = None;
                        let mut default: Option<Node> = None;
                        let id = self.atom(context)?;
                        if self.token() == &T::Rep {
                            self.advance();
                            typ = Some(self.operation(self.ops(0), 0, context)?);
                        }
                        if self.token() == &T::Assign {
                            self.advance();
                            default = Some(self.expr(context)?);
                        }
                        params.push((id, typ, default));
                    }
                }
                self.expect(T::EOL, context)?;
                self.advance_ln();
                let mut nodes: Vec<Node> = vec![];
                let (body_start_ln, body_start_col) = (self.ln, self.col);
                while let T::Indent(i) = self.token() {
                    if *i <= indent { break }
                    let node = self.stat(0, context)?;
                    nodes.push(node);
                }
                let body = Node(N::Body(nodes), Position::new(body_start_ln..self.ln, body_start_col..self.col));
                
                Ok(Node(N::Proc {
                    name: Box::new(name), params, body: Box::new(body)
                }, Position::new(start_ln..self.ln, start_col..self.col)))
            }
            _ => {
                context.trace(self.pos().to_owned(), &self.path);
                Err(E::UnexpectedToken(self.token().to_owned()))
            }
        }
    }
    pub fn expr(&mut self, context: &mut Context) -> Result<Node, E> {
        self.operation(self.ops(0), 0, context)
    }
    pub fn atom(&mut self, context: &mut Context) -> Result<Node, E> {
        let ret: Result<Node, E> = match self.token() {
            T::Wildcard => Ok(Node(N::Wildcard, self.pos().to_owned())),
            T::Null => Ok(Node(N::Null, self.pos().to_owned())),
            T::Int(v) => Ok(Node(N::Int(*v), self.pos().to_owned())),
            T::Float(v) => Ok(Node(N::Float(*v), self.pos().to_owned())),
            T::Bool(v) => Ok(Node(N::Bool(*v), self.pos().to_owned())),
            T::String(v) => Ok(Node(N::String(v.to_owned()), self.pos().to_owned())),
            T::Type(v) => Ok(Node(N::Type(v.to_owned()), self.pos().to_owned())),
            T::ID(id) => Ok(Node(N::ID(id.to_owned()), self.pos().to_owned())),
            _ => {
                context.trace(self.pos().to_owned(), &self.path);
                Err(E::UnexpectedToken(self.token().to_owned()))
            }
        };
        self.advance();
        ret
    }
}

pub fn parse(path: &String, tokens: Vec<Vec<Token>>, context: &mut Context) -> Result<Node, E> {
    Parser::new(path, tokens).parse(context)
}