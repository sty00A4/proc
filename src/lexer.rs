use std::cmp::min;
use crate::position::*;
use crate::errors::*;
use crate::context::*;

static DIGITS: [&str; 10] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
static LETTERS: [&str; 53] = [
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",
    "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
    "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_"
];

#[derive(Debug, Clone)]
pub enum T {
    EOF, EOL,
    Rule, Container, Proc, If, Else, While,
    Var, Global, Assign, Rep,
    Call,

    Int(i64), Float(f64), Bool(bool), String(String),
    ID(String)
}
#[derive(Clone)]
pub struct Token(T, Position);
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

pub fn lex(path: &String, text: &String, context: &mut Context) -> Result<Vec<Vec<Token>>, E> {
    let mut tokens: Vec<Vec<Token>> = vec![];
    for (ln, line) in text.split("\n").enumerate() {
        tokens.push(vec![]);
        let mut col: usize = 0;
        while col < line.len() {
            match &line[col..col+1] {
                // white space
                " "|"\t"|"\r" => col += 1,
                "\"" => {
                    let start = col;
                    col += 1;
                    let mut s = String::new();
                    while col < line.len() {
                        if &line[col..col+1] == "\"" { break }
                        s.push_str(&line[col..col+1]);
                        col += 1;
                    }
                    col += 1;
                    tokens[ln].push(Token(T::String(s), Position::new(ln..ln+1, start..col+1)));
                }
                "!" => {
                    tokens[ln].push(Token(T::Call, Position::new(ln..ln+1, col..col+1)));
                    col += 1;
                }
                _ => {
                    // id
                    if LETTERS.contains(&&line[col..col+1]) {
                        let start = col;
                        let mut id = String::new();
                        while col < line.len() {
                            if !LETTERS.contains(&&line[col..col+1]) { break }
                            id.push_str(&line[col..col+1]);
                            col += 1;
                        }
                        tokens[ln].push(Token((|| match id.as_str() {
                            "rule" => T::Rule,
                            "container" => T::Container,
                            "proc" => T::Proc,
                            "if" => T::If,
                            "else" => T::Else,
                            "while" => T::While,
                            "var" => T::Var,
                            "global" => T::Global,
                            _ => T::ID(id)
                        })(), Position::new(ln..ln+1, start..col)));
                        continue
                    }
                    // error
                    context.trace(Position::new(ln..ln+1, col..col+1), path);
                    return Err(E::IllegalChar(line[col..col+1].to_string()))
                }
            }
        }
        tokens[ln].push(Token(T::EOL, Position::new(ln..ln+1, col..col))); // end of line
    }
    tokens.push(vec![Token(T::EOF, Position::new(tokens.len()-1..tokens.len(), 0..0))]); // end of file
    Ok(tokens)
}