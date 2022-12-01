use std::collections::HashMap;
use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub enum R { None, Return, Break, Continue }

pub fn binary(op: &T, left: &V, right: &V, pos: &Position, context: &mut Context) -> Result<V, E> {
    match op {
        T::Add => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Int(*v1 + *v2)),
                V::Float(v2) => return Ok(V::Float(*v1 as f64 + v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 + *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 + *v2)),
                _ => {}
            }
            V::String(v1) => match right {
                V::String(v2) => return Ok(V::String(v1.clone() + v2.as_str())),
                _ => {}
            }
            _ => {}
        }
        T::Sub => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Int(*v1 - *v2)),
                V::Float(v2) => return Ok(V::Float(*v1 as f64 - *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 - *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 - *v2)),
                _ => {}
            }
            _ => {}
        }
        T::Mul => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Int(*v1 * *v2)),
                V::Float(v2) => return Ok(V::Float(*v1 as f64 * *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 * *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 * *v2)),
                _ => {}
            }
            V::String(v1) => match right {
                V::Int(v2) => return Ok(V::String(v1.repeat(*v2 as usize))),
                _ => {}
            }
            _ => {}
        }
        T::Div => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 as f64 / *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 as f64 / *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 / *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 / *v2)),
                _ => {}
            }
            _ => {}
        }
        T::Mod => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Int(*v1 % *v2)),
                V::Float(v2) => return Ok(V::Float(*v1 as f64 % *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Float(*v1 % *v2 as f64)),
                V::Float(v2) => return Ok(V::Float(*v1 % *v2)),
                _ => {}
            }
            _ => {}
        }
        _ => {
            context.trace(pos.to_owned());
            return Err(E::InvalidBinaryOp(op.to_owned()))
        }
    };
    context.trace(pos.to_owned());
    Err(E::Binary(op.clone(), left.clone(), right.clone()))
}
pub fn unary(op: &T, value: &V, pos: &Position, context: &mut Context) -> Result<V, E> {
    match op {
        T::Sub => match value {
            V::Int(v) => return Ok(V::Int(-v)),
            V::Float(v) => return Ok(V::Float(-v)),
            _ => {}
        }
        T::Len => match value {
            V::String(v) => return Ok(V::Int(v.len() as i64)),
            V::Vector(v, _) => return Ok(V::Int(v.len() as i64)),
            _ => {}
        }
        T::Not => match value {
            V::Bool(v) => return Ok(V::Bool(!v)),
            _ => {}
        }
        _ => {
            context.trace(pos.clone());
            return Err(E::InvalidUnaryOp(op.clone()))
        }
    };
    context.trace(pos.clone());
    Err(E::Unary(op.clone(), value.clone()))
}

pub fn interpret(input_node: &Node, context: &mut Context) -> Result<(V, R), E> {
    match input_node {
        // atom
        Node(N::Wildcard, _) => Ok((V::Wildcard, R::None)),
        Node(N::Null, _) => Ok((V::Null, R::None)),
        Node(N::Int(v), _) => Ok((V::Int(*v), R::None)),
        Node(N::Float(v), _) => Ok((V::Float(*v), R::None)),
        Node(N::Bool(v), _) => Ok((V::Bool(*v), R::None)),
        Node(N::String(v), _) => Ok((V::String(v.to_owned()), R::None)),
        Node(N::ID(id), pos) => {
            let value = context.get(id);
            match value {
                Some(value) => Ok((value.clone(), R::None)),
                None => {
                    context.trace(pos.clone());
                    Err(E::NotDefined(id.clone()))
                }
            }
        }
        Node(N::Vector(nodes), _) => {
            let mut values: Vec<V> = vec![];
            let mut types: Vec<Type> = vec![];
            for n in nodes.iter() {
                let (v, _) = interpret(n, context)?;
                types.push(v.typ());
                values.push(v);
            }
            Ok((V::Vector(values, Type::create_union(types)), R::None))
        }
        Node(N::Object(nodes), _) => {
            let mut values: HashMap<String, V> = HashMap::new();
            for (key_node, node) in nodes.iter() {
                if let Node(N::ID(key_str), _) = key_node {
                    let (v, _) = interpret(node, context)?;
                    values.insert(key_str.clone(), v);
                } else {
                    context.trace(key_node.1.clone());
                    return Err(E::ExpectedNode(N::ID("".into()), key_node.0.clone()))
                }
            }
            Ok((V::Object(values), R::None))
        }
        Node(N::Type(v), _) => Ok((V::Type(v.to_owned()), R::None)),

        // operations
        Node(N::Binary { op, left, right }, pos) => {
            let (left, _) = interpret(left, context)?;
            let (right, _) = interpret(right, context)?;
            let res = binary(op, &left, &right, pos, context)?;
            Ok((res, R::None))
        }
        Node(N::Unary { op, node }, pos) => {
            let (value, _) = interpret(node, context)?;
            let res = unary(op, &value, pos, context)?;
            Ok((res, R::None))
        }
        Node(N::Multi { op, nodes }, pos) => {
            match nodes.len() {
                0 => Ok((V::Null, R::None)),
                1 => {
                    let (value, _) = interpret(&nodes[0], context)?;
                    let res = unary(op, &value, pos, context)?;
                    Ok((res, R::None))
                }
                2 => {
                    let (left, _) = interpret(&nodes[0], context)?;
                    let (right, _) = interpret(&nodes[1], context)?;
                    let res = binary(op, &left, &right, pos, context)?;
                    Ok((res, R::None))
                }
                _ => {
                    match op {
                        // todo: comp
                        _ => {
                            let (mut value, _) = interpret(&nodes[0], context)?;
                            for i in 1..nodes.len() {
                                let (v, _) = interpret(&nodes[i], context)?;
                                value = binary(op, &value, &v, &nodes[i].1, context)?;
                            }
                            Ok((value, R::None))
                        }
                    }
                }
            }
        }

        // structure
        Node(N::Return(node), _) => {
            let (value, _) = interpret(node, context)?;
            Ok((value, R::Return))
        }
        Node(N::Body(nodes), _) => {
            context.push();
            for n in nodes.iter() {
                let (value, ret) = interpret(n, context)?;
                if ret != R::None {
                    context.pop();
                    return Ok((value, ret))
                }
            }
            context.pop();
            Ok((V::Null, R::None))
        }
        Node(N::Assign { global, id: id_node, expr }, pos) => {
            let (value, _) = interpret(expr, context)?;
            match id_node.as_ref() {
                Node(N::ID(id), id_pos) => {
                    if *global {
                        context.set(id, &value);
                    } else {
                        context.def(id, &value);
                    }
                    Ok((V::Null, R::None))
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }
        }
        Node(N::OpAssign { op, id: id_node, expr }, pos) => {
            let (value, _) = interpret(expr, context)?;
            let new_value = match id_node.as_ref() {
                //                                this sucks, but it works
                Node(N::ID(id), id_pos) => match (&mut context.clone()).get(id) {
                    Some(old_value) => match op {
                        T::Assign => Ok(value),
                        T::AddAssign => binary(&T::Add, old_value, &value, pos, context),
                        T::SubAssign => binary(&T::Sub, old_value, &value, pos, context),
                        T::MulAssign => binary(&T::Mul, old_value, &value, pos, context),
                        T::DivAssign => binary(&T::Div, old_value, &value, pos, context),
                        T::ModAssign => binary(&T::Mod, old_value, &value, pos, context),
                        _ => Ok(old_value.clone())
                    }
                    None => {
                        context.trace(id_pos.clone());
                        Err(E::NotDefined(id.clone()))
                    }
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }?;
            match id_node.as_ref() {
                Node(N::ID(id), id_pos) => {
                    context.set(id, &new_value);
                    Ok((V::Null, R::None))
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }
        }
        Node(N::Inc(id_node), pos) => {
            let new_value = match id_node.as_ref() {
                //                                this sucks, but it works
                Node(N::ID(id), id_pos) => match (&mut context.clone()).get(id) {
                    Some(old_value) => binary(&T::Add, old_value, &V::Int(1), pos, context),
                    None => {
                        context.trace(id_pos.clone());
                        Err(E::NotDefined(id.clone()))
                    }
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }?;
            match id_node.as_ref() {
                Node(N::ID(id), id_pos) => {
                    context.set(id, &new_value);
                    Ok((V::Null, R::None))
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }
        }
        Node(N::Dec(id_node), pos) => {
            let new_value = match id_node.as_ref() {
                //                                this sucks, but it works
                Node(N::ID(id), id_pos) => match (&mut context.clone()).get(id) {
                    Some(old_value) => binary(&T::Sub, old_value, &V::Int(1), pos, context),
                    None => {
                        context.trace(id_pos.clone());
                        Err(E::NotDefined(id.clone()))
                    }
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }?;
            match id_node.as_ref() {
                Node(N::ID(id), id_pos) => {
                    context.set(id, &new_value);
                    Ok((V::Null, R::None))
                }
                _ => {
                    context.trace(pos.clone());
                    Err(E::CannotAssign(id_node.0.clone()))
                }
            }
        }
        _ => Err(E::Todo(input_node.to_string()))
    }
}