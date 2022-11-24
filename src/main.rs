#![allow(dead_code)]#![allow(unused)]

mod position;
mod errors;
mod value;
mod context;
mod lexer;
use position::*;
use errors::*;
use value::*;
use context::*;
use lexer::*;

fn runfile_context(path: &String, text: &String) -> Result<V, E> {
    let mut context = Context::new();
    let tokens = lex(path, text, &mut context)?;
    for (ln, line) in tokens.iter().enumerate() {
        print!("{ln} ");
        for token in line.iter() {
            print!("{token:?}\t");
        }
        println!();
    }
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