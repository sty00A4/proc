use std::collections::HashMap;
use crate::position::*;
use crate::errors::*;
use crate::context::*;
use crate::value::*;
use crate::lexer::*;
use crate::parser::*;

pub enum R { None, Return, Break, Continue }

pub fn interpret(input_node: &Node, context: &mut Context) -> Result<(V, R), E> {
    match input_node {
        Node(N::Wildcard, _) => Ok((V::Wildcard, R::None)),
        Node(N::Null, _) => Ok((V::Null, R::None)),
        Node(N::Int(v), _) => Ok((V::Int(*v), R::None)),
        Node(N::Float(v), _) => Ok((V::Float(*v), R::None)),
        Node(N::Bool(v), _) => Ok((V::Bool(*v), R::None)),
        Node(N::String(v), _) => Ok((V::String(v.to_owned()), R::None)),
        Node(N::Type(v), _) => Ok((V::Type(v.to_owned()), R::None)),
        Node(N::Binary { op, left, right }, pos) => {
            let (left, _) = interpret(left, context)?;
            let (right, _) = interpret(right, context)?;
            match op {
                _ => {
                    context.trace(pos.to_owned());
                    return Err(E::InvalidBinaryOp(op.to_owned()))
                }
            };
            context.trace(pos.to_owned());
            Err(E::Binary(op.clone(), left.clone(), right.clone()))
        }
        _ => Err(E::Todo(input_node.to_string()))
    }
}