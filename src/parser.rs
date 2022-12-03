use std::collections::HashMap;
use crate::*;

pub type ProcParams = Vec<(Node, Option<Node>, bool)>;

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
    Call { id: Box<Node>, args: Vec<Node> }, CallExpr { id: Box<Node>, args: Vec<Node> },
    If { cond: Box<Node>, body: Box<Node>, else_body: Option<Box<Node>> }, While { cond: Box<Node>, body: Box<Node> },
    IfExpr { cond: Box<Node>, node: Box<Node>, else_node: Box<Node> },
    Proc { name: Box<Node>, params: ProcParams, body: Box<Node> },
    Rule { name: Box<Node>, id: Box<Node>, rules: Rules },
    Field { head: Box<Node>, field: Box<Node> },
}
impl N {
    pub fn name(&self) -> &str {
        match self {
            Self::Body(_) => "body",
            Self::Wildcard => "wildcard",
            Self::Null => "null",
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Vector(_) => "vec",
            Self::Object(_) => "obj",
            Self::ID(_) => "identifier",
            Self::Type(_) => "type",
            Self::Binary { op:_, left:_, right:_ } => "binary operation",
            Self::Unary { op:_, node:_ } => "unary operation",
            Self::Multi { op:_, nodes:_ } => "multi operation",
            Self::Assign { global:_, id:_, expr:_ } => "assignment",
            Self::OpAssign { op:_, id:_, expr:_ } => "operator assignment",
            Self::Inc(_) => "incrementation",
            Self::Dec(_) => "decrementation",
            Self::Return(_) => "return",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Call { id:_, args:_ } => "call",
            Self::CallExpr { id:_, args:_ } => "call expression",
            Self::If { cond:_, body:_, else_body:_ } => "if statement",
            Self::While { cond:_, body:_ } => "while statement",
            Self::IfExpr { cond:_, node:_, else_node:_ } => "if expression",
            Self::Proc { name:_, params:_, body:_ } => "procedure definition",
            Self::Rule { name:_, id:_, rules:_ } => "rule definition",
            Self::Field { head:_, field:_ } => "field",
        }
    }
}
impl std::fmt::Display for N {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Body(nodes) => write!(f, "{}", nodes.iter().map(|x| x.to_string())
            .collect::<Vec<String>>().join("; ")),
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Vector(v) => write!(f, "[{}]", v.iter().map(|x| x.to_string())
            .collect::<Vec<String>>().join(", ")),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}"))
            .collect::<Vec<String>>().join(", ")),
            Self::ID(v) => write!(f, "{v}"),
            Self::Type(v) => write!(f, "{v}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op} {right}"),
            Self::Unary { op, node } => write!(f, "{op} {node}"),
            Self::Multi { op, nodes } => write!(f, "{op} {}", nodes.iter().map(|x| x.to_string())
            .collect::<Vec<String>>().join(" ")),
            Self::Assign { global, id, expr } => if *global {
                write!(f, "global {id} = {expr}")
            } else {
                write!(f, "var {id} = {expr}")
            }
            Self::OpAssign { op, id, expr } => write!(f, "{id} {op} {expr}"),
            Self::Inc(id) => write!(f, "{id}++"),
            Self::Dec(id) => write!(f, "{id}--"),
            Self::Return(node) => write!(f, "return {node}"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Dec(id) => write!(f, "{id}--"),
            Self::Call { id, args } => write!(f, "{id}! {}", args.iter().map(|x| x.to_string())
            .collect::<Vec<String>>().join(", ")),
            Self::CallExpr { id, args } => write!(f, "{id}({})", args.iter().map(|x| x.to_string())
            .collect::<Vec<String>>().join(", ")),
            Self::If { cond, body, else_body } => match else_body {
                Some(else_body) => write!(f, "if {cond} {body} else {else_body}"),
                None => write!(f, "if {cond} {body}")
            },
            Self::IfExpr { cond, node, else_node } => write!(f, "{node} if {cond} else {else_node}"),
            Self::While { cond, body } => write!(f, "while {cond} {body}"),
            Self::Proc { name, params, body } => write!(f, "proc {name} <- {} {body}",
            params.iter().map(|(id, typ, apply)|
                match typ {
                    Some(typv) => format!("{id} : {typv}{}", if *apply { "!" } else { "" }),
                    None => format!("{id}")
                }
            ).collect::<Vec<String>>().join(", ")),
            Self::Rule { name, id, rules } => write!(f, "rule {name} <- {id}; {}",
            rules.iter().map(|(rule, new)| match new {
                Some(new) => format!("{rule} : {new}"),
                None => format!("{rule}")
            })
            .collect::<Vec<String>>().join("; ")),
            Self::Field { head, field } => write!(f, "{head}.{field}"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Node(pub N, pub Position);
impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.0)
    }
}
impl Node {
    pub fn display(&self, indent: usize) -> String {
        let s = String::from("    ").repeat(indent);
        match &self.0 {
            N::Body(nodes) => format!("{}", nodes.iter().map(|x| format!("{}", x.display(indent)))
            .collect::<Vec<String>>().join("\n")),
            N::Wildcard => format!("_"),
            N::Null => format!("null"),
            N::Int(v) => format!("{v:?}"),
            N::Float(v) => format!("{v:?}"),
            N::Bool(v) => format!("{v:?}"),
            N::String(v) => format!("{v:?}"),
            N::Vector(v) => format!("[{}]", v.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(", ")),
            N::Object(v) => format!("{{ {} }}",
            v.iter().map(|(k, v)| format!("{} = {}", k.display(indent), v.display(indent)))
            .collect::<Vec<String>>().join(", ")),
            N::ID(v) => format!("{v}"),
            N::Type(v) => format!("{v}"),
            N::Binary { op, left, right } => format!("{} {op} {}", left.display(indent), right.display(indent)),
            N::Unary { op, node } => format!("{op} {}", node.display(indent)),
            N::Multi { op, nodes } => format!("{op} {}",
            nodes.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(" ")),
            N::Assign { global, id, expr } => if *global {
                format!("{s}global {} = {}", id.display(indent), expr.display(indent))
            } else {
                format!("{s}var {} = {}", id.display(indent), expr.display(indent))
            }
            N::OpAssign { op, id, expr } => format!("{s}{} {op} {}", id.display(indent), expr.display(indent)),
            N::Inc(id) => format!("{s}{}++", id.display(indent)),
            N::Dec(id) => format!("{s}{}--", id.display(indent)),
            N::Return(node) => format!("{s}return {}", node.display(indent)),
            N::Break => format!("{s}break"),
            N::Continue => format!("{s}continue"),
            N::Call { id, args } => format!("{s}{}! {}", id.display(indent),
            args.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(", ")),
            N::CallExpr { id, args } => format!("{}({})", id.display(indent),
            args.iter().map(|x| x.display(indent)).collect::<Vec<String>>().join(", ")),
            N::If { cond, body, else_body } => match else_body {
                Some(else_body) => format!("{s}if {} \n{}\n{s}else\n{}", cond.display(indent),
                body.display(indent + 1), else_body.display(indent + 1)),
                None => format!("if {} \n{}\n", cond.display(indent), body.display(indent + 1))
            },
            N::IfExpr { cond, node, else_node } => format!("{} if {} else {}", node.display(indent),
            cond.display(indent), else_node.display(indent)),
            N::While { cond, body } => format!("{s}while {} \n{}", cond.display(indent), body.display(indent + 1)),
            N::Proc { name, params, body } => format!("{s}proc {} <- {} \n{}", name.display(indent),
            params.iter().map(|(id, typ, apply)|
                match typ {
                    Some(typv) => format!("{} : {}{}", id.display(indent), typv.display(indent), if *apply { "!" } else { "" }),
                    None => format!("{}", id.display(indent))
                }
            ).collect::<Vec<String>>().join(" "), body.display(indent + 1)),
            N::Rule { name, id, rules } => format!("{s}rule {} <- {}\n{}",
            name.display(indent), id.display(indent),
            rules.iter().map(|(rule, new)| match new {
                Some(new) => format!("{} : {}", rule.display(indent), new.display(indent)),
                None => format!("{}", rule.display(indent))
            })
            .collect::<Vec<String>>().join("\n")),
            N::Field { head, field } => format!("{}.{}", head.display(indent), field.display(indent)),
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
    layers: Vec<Layer>,
}
impl Parser {
    pub fn new(path: &String, tokens: Vec<Vec<Token>>) -> Self {
        Self {
            tokens, path: path.clone(), col: 0, ln: 0,
            layers: vec![
                Layer::Binary(vec![T::And, T::Or, T::Xor]),
                Layer::UnaryLeft(vec![T::Not]),
                Layer::Binary(vec![T::EQ, T::NE, T::LT, T::LE, T::GT, T::GE, T::Is, T::Contains]),
                Layer::Binary(vec![T::Add, T::Sub]),
                Layer::Binary(vec![T::Mul, T::Div, T::Mod]),
                Layer::UnaryLeft(vec![T::Add, T::Sub]),
                Layer::UnaryLeft(vec![T::Len]),
                Layer::UnaryRight(vec![T::Safe]),
                Layer::Binary(vec![T::Option]),
                Layer::Binary(vec![T::Field]),
            ],
        }
    }
    pub fn token(&self) -> &T {
        match self.tokens.get(self.ln) {
            Some(line) => match line.get(self.col) {
                Some(token) => &token.0,
                None => match &line.last() {
                    Some(token) => &token.0,
                    None => &T::EOL
                }
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
        self.expect(token, context)?;
        self.advance();
        Ok(())
    }
    pub fn advance_if(&mut self, token: T, context: &mut Context) {
        if self.token() == &token { self.advance(); }
    }
    pub fn advance_line_break(&mut self) {
        self.advance();
        self.advance_if_line_break();
    }
    pub fn advance_if_line_break(&mut self) {
        if self.token() == &T::EOL {
            self.advance_ln();
            if let T::Indent(_) = self.token() {
                self.advance();
            }
        }
    }
    pub fn expect(&mut self, token: T, context: &mut Context) -> Result<(), E> {
        if self.token() != &token {
            context.trace(self.pos().to_owned());
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
            Layer::Atom => self.call(context)
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
                let mut params: ProcParams = vec![];
                if self.token() == &T::In {
                    self.advance();
                    while self.token() != &T::EOL {
                        let mut typ: Option<Node> = None;
                        let mut apply = false;
                        let id = self.atom(context)?;
                        if self.token() == &T::Rep {
                            self.advance();
                            typ = Some(self.operation(self.ops(0), 0, context)?);
                            if self.token() == &T::Call {
                                apply = true;
                                self.advance();
                            }
                        }
                        self.advance_if(T::Sep, context);
                        params.push((id, typ, apply));
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
            T::Rule => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance();
                let name = self.atom(context)?;
                self.advance_expect(T::In, context)?;
                let id = self.atom(context)?;
                self.expect(T::EOL, context)?;
                self.advance_ln();
                let mut rules: Rules = vec![];
                while let T::Indent(i) = self.token() {
                    if *i <= indent { break }
                    self.advance();
                    let node = self.expr(context)?;
                    let mut new: Option<Node> = None;
                    if self.token() == &T::Rep {
                        self.advance();
                        new = Some(self.expr(context)?);
                    }
                    self.expect(T::EOL, context)?;
                    rules.push((node, new));
                    self.advance_ln();
                }
                Ok(Node(N::Rule {
                    name: Box::new(name), id: Box::new(id), rules
                }, Position::new(start_ln..self.ln, start_col..self.col)))
            }
            _ => {
                let node = self.operation(self.layers.last().unwrap().clone(), self.layers.len()-1, context)?;
                if [T::Assign, T::AddAssign, T::SubAssign, T::MulAssign, T::DivAssign, T::ModAssign].contains(&self.token()) {
                    let op = self.token().to_owned();
                    self.advance();
                    let expr = self.expr(context)?;
                    self.advance_ln();
                    return Ok(Node(N::OpAssign {
                        op, id: Box::new(node), expr: Box::new(expr)
                    }, Position::new(self.ln..self.ln+1, start..self.col)))
                }
                if self.token() == &T::Inc {
                    self.advance_ln();
                    return Ok(Node(N::Inc(Box::new(node)), Position::new(self.ln..self.ln+1, start..self.col)))
                }
                if self.token() == &T::Dec {
                    self.advance_ln();
                    return Ok(Node(N::Dec(Box::new(node)), Position::new(self.ln..self.ln+1, start..self.col)))
                }
                self.advance_expect(T::Call, context)?;
                let mut args: Vec<Node> = vec![];
                while self.token() != &T::EOL {
                    let value = self.expr(context)?;
                    self.advance_if(T::Sep, context);
                    args.push(value);
                }
                self.advance_ln();
                Ok(Node(N::Call {
                    id: Box::new(node), args
                }, Position::new(self.ln..self.ln+1, start..self.col)))
            }
        }
    }
    pub fn expr(&mut self, context: &mut Context) -> Result<Node, E> {
        let start = self.col;
        let node = self.operation(self.ops(0), 0, context)?;
        if self.token() == &T::If {
            self.advance();
            let cond = self.operation(self.ops(0), 0, context)?;
            self.advance_expect(T::Else, context)?;
            let else_node = self.operation(self.ops(0), 0, context)?;
            return Ok(Node(N::IfExpr {
                cond: Box::new(cond), node: Box::new(node), else_node: Box::new(else_node)
            }, Position::new(self.ln..self.ln+1, start..self.col)))
        }
        Ok(node)
    }
    pub fn call(&mut self, context: &mut Context) -> Result<Node, E> {
        let node = self.field(context)?;
        if self.token() == &T::EvalIn {
            let (start_ln, start_col) = (self.ln, self.col);
            self.advance_line_break();
            let mut args: Vec<Node> = vec![];
            while self.token() != &T::EvalOut {
                let arg = self.expr(context)?;
                self.advance_if(T::Sep, context);
                self.advance_if_line_break();
                args.push(arg);
            }
            self.advance();
            return Ok(Node(N::CallExpr {
                id: Box::new(node), args
            }, Position::new(start_ln..self.ln+1, start_col..self.col)))
        }
        if self.token() == &T::ObjectIn {
            let arg = self.atom(context)?;
            return Ok(Node(N::CallExpr {
                id: Box::new(node.clone()), args: vec![arg.clone()]
            }, Position::new((node.1).0.start..(arg.1).0.end, (node.1).1.start..(arg.1).1.end)))
        }
        if let T::String(_) = self.token() {
            let arg = self.atom(context)?;
            return Ok(Node(N::CallExpr {
                id: Box::new(node.clone()), args: vec![arg.clone()]
            }, Position::new((node.1).0.start..(arg.1).0.end, (node.1).1.start..(arg.1).1.end)))
        }
        Ok(node)
    }
    pub fn field(&mut self, context: &mut Context) -> Result<Node, E> {
        let start = self.col;
        let mut head = self.atom(context)?;
        while self.token() == &T::Field {
            self.advance();
            let field = self.atom(context)?;
            head = Node(N::Field {
                head: Box::new(head.clone()), field: Box::new(field.clone()) }
            , Position::new(self.ln..self.ln+1, start..(field.1).1.end))
        }
        Ok(head)
    }
    pub fn atom(&mut self, context: &mut Context) -> Result<Node, E> {
        let node = match self.token() {
            T::Wildcard => Ok(Node(N::Wildcard, self.pos().to_owned())),
            T::Null => Ok(Node(N::Null, self.pos().to_owned())),
            T::Int(v) => Ok(Node(N::Int(*v), self.pos().to_owned())),
            T::Float(v) => Ok(Node(N::Float(*v), self.pos().to_owned())),
            T::Bool(v) => Ok(Node(N::Bool(*v), self.pos().to_owned())),
            T::String(v) => Ok(Node(N::String(v.to_owned()), self.pos().to_owned())),
            T::Type(v) => Ok(Node(N::Type(v.to_owned()), self.pos().to_owned())),
            T::ID(id) => Ok(Node(N::ID(id.to_owned()), self.pos().to_owned())),
            T::EvalIn => {
                self.advance();
                let node = self.expr(context)?;
                Ok(node)
            }
            T::VectorIn => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance_line_break();
                let mut nodes: Vec<Node> = vec![];
                while self.token() != &T::VectorOut {
                    let node = self.expr(context)?;
                    self.advance_if(T::Sep, context);
                    self.advance_if_line_break();
                    nodes.push(node);
                }
                // some weird shit is going on here with the operation function
                Ok(Node(N::Vector(nodes), Position::new(start_ln..self.ln+1, start_col..self.col)))
            }
            T::ObjectIn => {
                let (start_ln, start_col) = (self.ln, self.col);
                self.advance_line_break();
                let mut nodes: Vec<(Node, Node)> = vec![];
                while self.token() != &T::ObjectOut {
                    let key = self.atom(context)?;
                    self.advance_expect(T::Assign, context)?;
                    let expr = self.expr(context)?;
                    self.advance_if(T::Sep, context);
                    self.advance_if_line_break();
                    nodes.push((key, expr));
                }
                // some weird shit is going on here with the operation function
                Ok(Node(N::Object(nodes), Position::new(start_ln..self.ln+1, start_col..self.col)))
            }
            _ => {
                context.trace(self.pos().to_owned());
                Err(E::UnexpectedToken(self.token().to_owned()))
            }
        }?;
        self.advance();
        Ok(node)
    }
}

pub fn parse(path: &String, tokens: Vec<Vec<Token>>, context: &mut Context) -> Result<Node, E> {
    Parser::new(path, tokens).parse(context)
}