#![allow(dead_code)]#![allow(unused)]

mod position;
mod errors;
mod value;
mod context;
mod lexer;
mod parser;
use position::*;
use errors::*;
use value::*;
use context::*;
use lexer::*;
use parser::*;

fn runfile_context(path: &String, text: &String) -> Result<V, E> {
    let mut context = Context::new();
    let tokens = lex(path, text, &mut context)?;
    for (ln, line) in tokens.iter().enumerate() {
        print!("{ln} ");
        for token in line.iter() { print!("{token:?}\t"); }
        println!();
    }
    let ast = parse(path, tokens, &mut context)?;
    println!("{ast}");
    println!("{}", ast.display(0));
    Ok(V::Null)
}

fn main() {
    let mut args_ = std::env::args().collect::<Vec<String>>();
    let mut args = args_.iter_mut();
    args.next();
    match args.next() {
        Some(path) => match std::fs::read_to_string(path.as_str()) {
            Ok(text) => match runfile_context(path, &text) {
                Ok(v) => {}
                Err(e) => println!("{e}"),
            }
            Err(e) => println!("ERROR: {e}"),
        }
        None => return,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::position::*;
    use crate::errors::*;
    use crate::value::*;
    use crate::context::*;
    use crate::lexer::*;

    #[test]
    fn type_checking() {
        assert!(Type::Any == Type::Any);
        assert!(Type::Undefiend == Type::Undefiend);
        assert!(Type::Union(vec![Type::Int, Type::Float]) == Type::Union(vec![Type::Int, Type::Float]));
        assert!(Type::Union(vec![Type::Int, Type::Float]) != Type::Union(vec![Type::Int, Type::Bool]));
        assert!(Type::Union(vec![Type::Int, Type::Float, Type::Bool]) != Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Scission(vec![Type::Int, Type::Float]) == Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Int == Type::Scission(vec![Type::String]));
        assert!(Type::Int != Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Scission(vec![Type::Int, Type::Float]) != Type::Int);
    }
}