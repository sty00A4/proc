#![allow(dead_code)]#![allow(unused)]

use std::thread;

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
fn run_file_context(path: &String) -> Result<Option<V>, (E, Trace)> {
    match std::fs::read_to_string(path.as_str()) {
        Ok(text) => run_context(path, &text),
        Err(e) => Err((E::TargetFile(path.clone()), vec![])),
    }
}
fn run_file(path: &String, context: &mut Context) -> Result<Option<V>, E> {
    match std::fs::read_to_string(path.as_str()) {
        Ok(text) => run(path, &text, context),
        Err(e) => Err(E::TargetFile(path.clone())),
    }
}

fn _main() {
    let mut args_ = std::env::args().collect::<Vec<String>>();
    let mut args = args_.iter_mut();
    args.next();
    match args.next() {
        Some(path) => match run_file_context(path) {
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
    match thread::Builder::new()
        .name("_main".to_string())
        .stack_size(0x800000)
        .spawn(_main)
    {
        Ok(child) => match child.join() {
            Ok(_) => {},
            Err(e) => eprintln!("{e:?}")
        }
        Err(e) => eprintln!("{e}")
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use crate::*;

    fn test_file(path: &'static str) -> Result<(), E> {
        let mut context = Context::new(&path.to_string());
        std_context(&mut context);
        run_file(&path.to_string(), &mut context)?;
        match context.get(&"test".to_string()) {
            Some(proc) => if let V::Proc(_, body) = proc {
                let mut call_context = Context::proc(&context);
                interpret(body, &mut call_context)?;
                Ok(())
            } else {
                Err(E::Test)
            }
            None => Err(E::Test)
        }
    }
    fn do_file(path: &'static str) -> Result<(), E> {
        match run_file_context(&path.to_string()) {
            Ok(_) => Ok(()),
            Err((e, trace)) => Err(e)
        }
    }
    #[test]
    fn type_checking() {
        assert!(Type::Any == Type::Any);
        assert!(Type::Undefined == Type::Undefined);
        assert!(Type::Union(vec![Type::Int, Type::Float]) == Type::Union(vec![Type::Int, Type::Float]));
        assert!(Type::Union(vec![Type::Int, Type::Float]) != Type::Union(vec![Type::Int, Type::Bool]));
        assert!(Type::Union(vec![Type::Int, Type::Float, Type::Bool]) != Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Scission(vec![Type::Int, Type::Float]) == Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Int == Type::Scission(vec![Type::String]));
        assert!(Type::Int != Type::Scission(vec![Type::Int, Type::Float]));
        assert!(Type::Scission(vec![Type::Int, Type::Float]) != Type::Int);
        assert!(Type::Vector(vec![Type::Int]) == Type::Vector(vec![Type::Int]));
        assert!(Type::Vector(vec![Type::Int, Type::Float]) == Type::Vector(vec![Type::Float, Type::Int]));
    }
    #[test]
    fn samples_numbers() -> Result<(), E> {
        test_file("samples/numbers.pr")
    }
    #[test]
    fn samples_field() -> Result<(), E> {
        test_file("samples/field.pr")
    }
    #[test]
    fn samples_ops() -> Result<(), E> {
        test_file("samples/ops.pr")
    }
    
    #[test]
    fn samples_person() -> Result<(), E> {
        do_file("samples/person.pr")
    }
}