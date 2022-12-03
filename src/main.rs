#![allow(dead_code)]#![allow(unused)]

use std::thread;
const STACK_SIZE: usize = 0x800000;

mod position;
mod errors;
mod value;
mod context;
mod lexer;
mod parser;
mod inter;
use position::*;
use errors::*;
use value::*;
use context::*;
use lexer::*;
use parser::*;
use inter::*;

fn run(path: &String, text: &String, context: &mut Context) -> Result<Option<V>, E> {
    let tokens = lex(path, text, context)?;
    // for (ln, line) in tokens.iter().enumerate() {
    //     print!("{ln} ");
    //     for token in line.iter() { print!("{token:?}\t"); }
    //     println!();
    // }
    let ast = parse(path, tokens, context)?;
    // println!("{ast}");
    // println!("{}", ast.display(0));
    let (value, ret) = interpret(&ast, context)?;
    if ret == R::Return {
        return Ok(Some(value))
    }
    Ok(None)
}
fn run_context(path: &String, text: &String) -> Result<Option<V>, (E, Trace)> {
    let mut context = Context::new(path);
    std_context(&mut context);
    let res = run(path, text, &mut context);
    match res {
        Ok(v) => Ok(v),
        Err(e) => Err((e, context.trace))
    }
}
fn runfile_context(path: &String) -> Result<Option<V>, (E, Trace)> {
    match std::fs::read_to_string(path.as_str()) {
        Ok(text) => run_context(path, &text),
        Err(e) => Err((E::TargetFile(path.clone()), vec![])),
    }
}

fn _main() {
    let mut args_ = std::env::args().collect::<Vec<String>>();
    let mut args = args_.iter_mut();
    args.next();
    match args.next() {
        Some(path) => match runfile_context(path) {
            Ok(v) => match v {
                Some(v) => println!("{v}"),
                None => {}
            }
            Err((e, trace)) => println!("{e}\n{}", display_trace(trace)),
        }
        None => return,
    }
}
fn main() {
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(_main)
        .unwrap();
    child.join().unwrap();
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use crate::*;

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
    #[test]
    fn samples_numbers() -> Result<(), (E, Trace)> {
        runfile_context(&String::from("samples/numbers.pr"))?;
        Ok(())
    }
}