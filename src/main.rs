#![allow(dead_code)]#![allow(unused)]

mod position;
mod errors;
mod value;
mod context;
use position::*;
use errors::*;
use value::*;
use context::*;

fn runfile_context(path: &String, text: &String) -> Result<V, E> {
    Ok(V::Int(0))
}

fn main() {
    let mut args_ = std::env::args().collect::<Vec<String>>();
    let mut args = args_.iter_mut();
    args.next();
    match args.next() {
        Some(path) => match std::fs::read_to_string(path.as_str()) {
            Ok(text) => match runfile_context(path, &text) {
                Ok(v) => println!("{v}"),
                Err(e) => println!("{e}"),
            }
            Err(e) => println!("ERROR: {e}"),
        }
        None => return,
    }
}