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
    EOF, EOL, Indent(u16),
    Rule, Container, Proc, If, Else, While,
    Var, Global,
//  !     =       :    <-  ->   #    ?     |       .      ..     ,
    Call, Assign, Rep, In, Out, Len, Safe, Option, Field, Range, Sep,
//  (       )        [         ]          {         }
    EvalIn, EvalOut, VectorIn, VectorOut, ObjectIn, ObjectOut,
//  +    -    *    /    %    ==  !=  <   >   <=  >=
    Add, Sub, Mul, Div, Mod, EQ, NE, LT, GT, LE, GE,
//  +=         -=         *=          /=         %=
    AddAssign, SubAssign, MulAsssign, DivAssign, ModAssign,
//  ++   --
    Inc, Dec,
//      in
    Is, Contains, And, Or, Xor,
//  _
    Wildcard, Null,
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
        if [" ", "\t"].contains(&&line[col..col+1]) {
            let start = col;
            let mut indent: u16 = 0;
            while col < line.len() && [" ", "\t"].contains(&&line[col..col+1]) {
                indent += if &line[col..col+1] == "\t" { 4 } else { 1 };
                col += 1;
            }
            if col < line.len() {
                tokens[ln].push(Token(T::Indent(indent), Position::new(ln..ln+1, start..col+1)));
            }
        }
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
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::NE, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Call, Position::new(ln..ln+1, start..col)));
                }
                "=" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::EQ, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Assign, Position::new(ln..ln+1, start..col)));
                }
                ":" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::Rep, Position::new(ln..ln+1, start..col)));
                }
                "<" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "-" {
                        col += 1;
                        tokens[ln].push(Token(T::In, Position::new(ln..ln+1, start..col)));
                    }
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::LE, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::LT, Position::new(ln..ln+1, start..col)));
                }
                ">" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::GE, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::GT, Position::new(ln..ln+1, start..col)));
                }
                "#" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::Len, Position::new(ln..ln+1, start..col)));
                }
                "?" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::Safe, Position::new(ln..ln+1, start..col)));
                }
                "|" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::Option, Position::new(ln..ln+1, start..col)));
                }
                "." => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "." {
                        col += 1;
                        tokens[ln].push(Token(T::Range, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Field, Position::new(ln..ln+1, start..col)));
                }
                "," => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::Sep, Position::new(ln..ln+1, start..col)));
                }
                "(" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::EvalIn, Position::new(ln..ln+1, start..col)));
                }
                ")" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::EvalOut, Position::new(ln..ln+1, start..col)));
                }
                "[" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::VectorIn, Position::new(ln..ln+1, start..col)));
                }
                "]" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::VectorOut, Position::new(ln..ln+1, start..col)));
                }
                "{" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::ObjectIn, Position::new(ln..ln+1, start..col)));
                }
                "}" => {
                    let start = col;
                    col += 1;
                    tokens[ln].push(Token(T::ObjectOut, Position::new(ln..ln+1, start..col)));
                }
                "+" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "+" {
                        col += 1;
                        tokens[ln].push(Token(T::Inc, Position::new(ln..ln+1, start..col)));
                    }
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::AddAssign, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Add, Position::new(ln..ln+1, start..col)));
                }
                "-" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "-" {
                        col += 1;
                        tokens[ln].push(Token(T::Dec, Position::new(ln..ln+1, start..col)));
                    }
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::SubAssign, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Sub, Position::new(ln..ln+1, start..col)));
                }
                "*" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::MulAsssign, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Mul, Position::new(ln..ln+1, start..col)));
                }
                "/" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "/" {
                        col += 1;
                        while col < line.len() { col += 1 }
                        continue
                    }
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::DivAssign, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Div, Position::new(ln..ln+1, start..col)));
                }
                "%" => {
                    let start = col;
                    col += 1;
                    if &line[col..col+1] == "=" {
                        col += 1;
                        tokens[ln].push(Token(T::ModAssign, Position::new(ln..ln+1, start..col)));
                    }
                    tokens[ln].push(Token(T::Mod, Position::new(ln..ln+1, start..col)));
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
                            "_" => T::Wildcard,
                            "null" => T::Null,
                            "true" => T::Bool(true),
                            "false" => T::Bool(false),
                            "rule" => T::Rule,
                            "container" => T::Container,
                            "proc" => T::Proc,
                            "if" => T::If,
                            "else" => T::Else,
                            "while" => T::While,
                            "var" => T::Var,
                            "global" => T::Global,
                            "is" => T::Is,
                            "in" => T::Contains,
                            "or" => T::Or,
                            "and" => T::And,
                            "xor" => T::Xor,
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