use std::{collections::HashMap, io::Write, fs};
use crate::*;

pub type Trace = Vec<(Position, String)>;

#[derive(Debug, Clone)]
pub struct Scope {
    pub vars: HashMap<String, V>
}
impl Scope {
    pub fn new() -> Self { Self { vars: HashMap::new() } }
    pub fn from(scope: &Self) -> Self { Self { vars: scope.vars.clone() } }
    
    pub fn get(&self, id: &String) -> Option<&V> {
        self.vars.get(id)
    }
    pub fn get_mut(&mut self, id: &String) -> Option<&mut V> {
        self.vars.get_mut(id)
    }
    pub fn set(&mut self, id: &String, v: &V) -> Option<V> {
        self.vars.insert(id.to_owned(), v.to_owned())
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pub stack: Vec<Scope>,
    pub global: Scope,
    pub trace: Trace,
    pub path: String,
}
impl Context {
    pub fn new(path: &String) -> Self {
        Self { stack: vec![Scope::new()], global: Scope::new(), trace: vec![], path: path.clone() }
    }
    pub fn from(context: &Context) -> Self {
        Self {
            stack: context.stack.clone(), global: context.global.clone(),
            trace: context.trace.clone(), path: context.path.clone()
        }
    }
    pub fn proc(context: &Context) -> Self {
        Self {
            stack: vec![Scope::new()], global: context.global.clone(),
            trace: context.trace.clone(), path: context.path.clone()
        }
    }
    pub fn container(context: &Context) -> Self {
        Self {
            stack: vec![Scope::new()], global: context.global.clone(),
            trace: vec![], path: context.path.clone()
        }
    }
    
    pub fn push(&mut self) {
        self.stack.push(Scope::new());
    }
    pub fn pop(&mut self) -> Option<Scope> {
        self.stack.pop()
    }
    
    pub fn get(&self, id: &String) -> Option<&V> {
        for scope in self.stack.iter().rev() {
            if let Some(v) = scope.get(id) { return Some(v) }
        }
        self.global.get(id)
    }
    pub fn get_mut(&mut self, id: &String) -> Option<&mut V> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(v) = scope.get_mut(id) { return Some(v) }
        }
        self.global.get_mut(id)
    }
    pub fn set(&mut self, id: &String, v: &V) -> Option<V> {
        if let Some(_) = self.global.get(id) {
            return self.global.set(id, v)
        }
        for scope in self.stack.iter_mut().rev() {
            if let Some(_) = scope.get(id) {
                return scope.set(id, v)
            }
        }
        self.stack.last_mut().unwrap().set(id, v)
    }
    pub fn def(&mut self, id: &String, v: &V) -> Option<V> {
        self.global.set(id, v)
    }
    
    pub fn trace(&mut self, pos: Position) {
        self.trace.push((pos, self.path.clone()));
    }
    pub fn pop_trace(&mut self) -> Option<(Position, String)> {
        self.trace.pop()
    }
}

pub fn _print(context: &mut Context, pos: &Position) -> Result<V, E> {
    let x = context.get(&String::from("x"));
    if let Some(x) = x { println!("{x}"); }
    Ok(V::Null)
}
pub fn _input(context: &mut Context, pos: &Position) -> Result<V, E> {
    let x = context.get(&String::from("x"));
    if let Some(x) = x {
        print!("{x}");
        std::io::stdout().flush();
    }
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => {
            while input.ends_with("\n") || input.ends_with("\r") { input.remove(input.len() - 1); }
            Ok(V::String(input))
        }
        Err(e) => Err(E::Error(e.to_string()))
    }
}
pub fn _assert(context: &mut Context, pos: &Position) -> Result<V, E> {
    let x = context.get(&String::from("x"));
    if let Some(x) = x {
        if V::bool(x) == V::Bool(false) {
            context.trace(pos.clone());
            return Err(E::Assertion)
        }
    } else {
        context.trace(pos.clone());
        return Err(E::Assertion)
    }
    Ok(V::Null)
}
pub fn fs_read(context: &mut Context, pos: &Position) -> Result<V, E> {
    let path = context.get(&String::from("path"));
    if let Some(V::String(path)) = path {
        match fs::read_to_string(path) {
            Ok(text) => Ok(V::String(text)),
            Err(_) => Err(E::FieldNotFound(path.clone()))
        }
    } else {
        Err(E::FieldNotFound("<NO_FILE>".into()))
    }
}
pub fn std_context(context: &mut Context) {
    fn type_node(typ: Type) -> Node {
        Node(N::Type(typ), Position::new(0..0, 0..0))
    }
    context.def(&String::from("print"), &V::ForeignProc(vec![
        ("x".into(), None, false)
    ], _print));
    context.def(&String::from("input"), &V::ForeignProc(vec![
        ("x".into(), Some(type_node(Type::String)), false)
    ], _input));
    context.def(&String::from("assert"), &V::ForeignProc(vec![
        ("x".into(), None, false)
    ], _assert));
    let mut fs_context = Context::new(&String::from("<FS>"));
    fs_context.def(&String::from("read"), &V::ForeignProc(vec![
        ("path".into(), Some(type_node(Type::String)), false)
    ], fs_read));
    context.def(&String::from("fs"), &&V::Container(fs_context));
    // todo more std functions: io, fs, language primitivesss
}