use std::collections::HashMap;
use std::cmp::min;
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
        T::EQ => return Ok(V::Bool(left == right)),
        T::NE => return Ok(V::Bool(left != right)),
        T::LT => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 < *v2)),
                V::Float(v2) => return Ok(V::Bool((*v1 as f64) < *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 < *v2 as f64)),
                V::Float(v2) => return Ok(V::Bool(*v1 < *v2)),
                _ => {}
            }
            _ => {}
        }
        T::GT => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 > *v2)),
                V::Float(v2) => return Ok(V::Bool(*v1 as f64 > *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 > *v2 as f64)),
                V::Float(v2) => return Ok(V::Bool(*v1 > *v2)),
                _ => {}
            }
            _ => {}
        }
        T::LE => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 <= *v2)),
                V::Float(v2) => return Ok(V::Bool(*v1 as f64 <= *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 <= *v2 as f64)),
                V::Float(v2) => return Ok(V::Bool(*v1 <= *v2)),
                _ => {}
            }
            _ => {}
        }
        T::GE => match left {
            V::Int(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 >= *v2)),
                V::Float(v2) => return Ok(V::Bool(*v1 as f64 >= *v2)),
                _ => {}
            }
            V::Float(v1) => match right {
                V::Int(v2) => return Ok(V::Bool(*v1 >= *v2 as f64)),
                V::Float(v2) => return Ok(V::Bool(*v1 >= *v2)),
                _ => {}
            }
            _ => {}
        }
        T::Is => match right {
            V::Type(typ) => return Ok(V::Bool(&left.typ() == typ)),
            V::Rule(_, _, _) => return Ok(V::Bool(check_rule(right, left, pos, context).is_ok())),
            _ => {}
        }
        T::Contains => match right {
            V::Vector(v, t) => return Ok(V::Bool(v.contains(left))),
            V::Tuple(v) => return Ok(V::Bool(v.contains(left))),
            V::Object(v) => match left {
                V::String(k) => return Ok(V::Bool(v.clone().into_keys().collect::<Vec<String>>().contains(k))),
                _ => {}
            }
            _ => {}
        }
        T::Or => if V::bool(left) == V::Bool(true) {
            return Ok(left.clone())
        } else {
            if V::bool(right) == V::Bool(true) {
                return Ok(right.clone())
            } else {
                return Ok(V::Bool(false))
            }
        }
        T::And => if V::bool(left) == V::Bool(true) {
            if V::bool(right) == V::Bool(true) {
                return Ok(right.clone())
            } else {
                return Ok(V::Bool(false))
            }
        } else {
            return Ok(V::Bool(false))
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

pub fn check_rule(rule_value: &V, value: &V, pos: &Position, context: &mut Context) -> Result<(), E> {
    if let V::Rule(name, id, rules) = rule_value {
        let mut rule_context = Context::proc(context);
        rule_context.set(id, value);
        for (rule, _) in rules.iter() {
            let (case, _) = interpret(rule, &mut rule_context)?;
            if V::bool(&case) == V::Bool(false) {
                context.trace(rule.1.clone());
                return Err(E::Rule(value.clone(), name.clone()))
            }
        }
        return Ok(())
    }
    context.trace(pos.clone());
    Err(E::ExpectedType(Type::Rule("<ANY>".into()), rule_value.typ()))
}
pub fn apply_rule(rule_value: &V, value: &V, pos: &Position, context: &mut Context) -> Result<V, E> {
    if let V::Rule(name, id, rules) = rule_value {
        let mut rule_context = Context::proc(context);
        let mut new_value = value.clone();
        rule_context.set(id, value);
        for (rule, new) in rules.iter() {
            let (case, _) = interpret(rule, &mut rule_context)?;
            if V::bool(&case) == V::Bool(false) {
                match new {
                    Some(new) => {
                        let res = interpret(new, &mut rule_context);
                        match res {
                            Ok((v, _)) => {
                                rule_context.set(id, &v);
                                new_value = v;
                            }
                            Err(_) => {
                                context.trace(rule.1.clone());
                                return Err(E::RuleCast(value.clone(), name.clone()))
                            }
                        }
                    }
                    None => {
                        context.trace(rule.1.clone());
                        return Err(E::RuleCast(value.clone(), name.clone()))
                    }
                }
            }
        }
        return Ok(new_value)
    }
    context.trace(pos.clone());
    Err(E::ExpectedType(Type::Rule("<ANY>".into()), rule_value.typ()))
}

pub fn assign_params(params: &ProcValueParams, arg_values: Vec<V>, pos: &Position, context: &mut Context) -> Result<(), E> {
    for i in 0..params.len() {
        let (param, type_node_, apply) = &params[i];
        let mut value = match arg_values.get(i) {
            Some(v) => v.clone(),
            None => V::Null
        };
        if let Some(type_node) = type_node_ {
            let (typ_, _) = interpret(type_node, context)?;
            if let V::Type(typ) = typ_ {
                if typ != value.typ() {
                    if *apply {
                        match typ.cast(&value) {
                            Some(new_value) => value = new_value,
                            None => {
                                context.trace(pos.clone());
                                return Err(E::ExpectedTypeArg(format!("{i}"), typ.clone(), value.typ()))
                            }
                        }
                    } else {
                        context.trace(pos.clone());
                        return Err(E::ExpectedTypeArg(format!("{i}"), typ.clone(), value.typ()))
                    }
                }
            } else if let Type::Rule(_) = typ_.typ() {
                if *apply {
                    value = apply_rule(&typ_, &value, &type_node.1, context)?;
                } else {
                    check_rule(&typ_, &value, &type_node.1, context)?;
                }
            } else {
                context.trace(type_node.1.clone());
                return Err(E::ExpectedType(Type::Union(vec![Type::Type, Type::Rule("<ANY>".into())]), typ_.typ()))
            }
        }
        context.set(param, &value);
    }
    Ok(())
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
                        T::EQ | T::NE | T::LT | T::GT | T::LE | T::GE => {
                            for i in 0..nodes.len()-1 {
                                let (left, _) = interpret(&nodes[i], context)?;
                                let (right, _) = interpret(&nodes[i+1], context)?;
                                let value = binary(op, &left, &right, &nodes[i].1, context)?;
                                if value == V::Bool(false) {
                                    return Ok((V::Bool(false), R::None))
                                }
                            }
                            Ok((V::Bool(true), R::None))
                        }
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
        Node(N::CallExpr { id: id_node, args }, pos) => {
            context.trace(pos.clone());
            let (proc, _) = interpret(id_node, context)?;
            let mut arg_values: Vec<V> = vec![];
            for arg in args.iter() {
                let (value, _) = interpret(arg, context)?;
                arg_values.push(value);
            }
            let mut call_context = Context::proc(context);
            let mut value = V::Null;
            match proc {
                V::Proc(ref params, ref body) => {
                    assign_params(params, arg_values, pos, &mut call_context)?;
                    (value, _) = interpret(body, &mut call_context)?;
                }
                V::ForeignProc(ref params, ref func) => {
                    assign_params(params, arg_values, pos, &mut call_context)?;
                    value = func(&mut call_context, pos)?;
                }
                V::Type(typ) => {
                    let arg = arg_values.get(0).unwrap_or_else(|| &V::Null);
                    match typ.cast(arg) {
                        Some(v) => value = v,
                        None => {
                            context.trace(pos.clone());
                            return Err(E::Cast(typ.clone(), arg.clone()))
                        }
                    }
                }
                V::Rule(_, _, _) => {
                    let arg = arg_values.get(0).unwrap_or_else(|| &V::Null);
                    value = apply_rule(&proc, arg, pos, &mut call_context)?;
                }
                _ => {
                    context.trace(pos.clone());
                    return Err(E::ExpectedType(Type::Union(vec![
                        Type::Proc, Type::ForeignProc, Type::Type, Type::Rule("<ANY>".into())
                    ]), proc.typ()))
                }
            }
            Ok((value, R::None))
        }
        Node(N::Field { head: head_node, field: field_node }, pos) => {
            let (head, _) = interpret(head_node, context)?;
            match &head {
                V::Object(obj) => if let Node(N::ID(field), field_pos) = field_node.as_ref() {
                    match obj.get(field) {
                        Some(value) => Ok((value.clone(), R::None)),
                        None => {
                            context.trace(field_pos.clone());
                            Err(E::FieldNotFound(field.clone()))
                        }
                    }
                } else {
                    let (field, _) = interpret(field_node, context)?;
                    context.trace(field_node.1.clone());
                    Err(E::InvalidField(head.typ(), field.typ()))
                }
                V::Vector(values, _) => if let Node(N::ID(field), field_pos) = field_node.as_ref() {
                    // todo
                    context.trace(field_pos.clone());
                    Err(E::FieldNotFound(field.clone()))
                } else {
                    let (field, _) = interpret(field_node, context)?;
                    match field {
                        V::Int(index) => if index >= 0 {
                            match values.get(index as usize) {
                                Some(value) => Ok((value.clone(), R::None)),
                                None => {
                                    context.trace(field_node.1.clone());
                                    Err(E::IndexRange(values.len(), index))
                                }
                            }
                        } else {
                            if values.len() as i64 - index >= 0 {
                                match values.get((values.len() as i64 - index) as usize) {
                                    Some(value) => Ok((value.clone(), R::None)),
                                    None => {
                                        context.trace(field_node.1.clone());
                                        Err(E::IndexRange(values.len(), index))
                                    }
                                }
                            } else {
                                context.trace(field_node.1.clone());
                                Err(E::IndexRange(values.len(), index))
                            }
                        }
                        _ => {
                            context.trace(field_node.1.clone());
                            Err(E::InvalidField(head.typ(), field.typ()))
                        }
                    }
                }
                V::Tuple(values) => if let Node(N::ID(field), field_pos) = field_node.as_ref() {
                    // todo
                    context.trace(field_pos.clone());
                    Err(E::FieldNotFound(field.clone()))
                } else {
                    let (field, _) = interpret(field_node, context)?;
                    match field {
                        V::Int(index) => if index >= 0 {
                            match values.get(index as usize) {
                                Some(value) => Ok((value.clone(), R::None)),
                                None => {
                                    context.trace(field_node.1.clone());
                                    Err(E::IndexRange(values.len(), index))
                                }
                            }
                        } else {
                            if values.len() as i64 - index >= 0 {
                                match values.get((values.len() as i64 - index) as usize) {
                                    Some(value) => Ok((value.clone(), R::None)),
                                    None => {
                                        context.trace(field_node.1.clone());
                                        Err(E::IndexRange(values.len(), index))
                                    }
                                }
                            } else {
                                context.trace(field_node.1.clone());
                                Err(E::IndexRange(values.len(), index))
                            }
                        }
                        _ => {
                            context.trace(field_node.1.clone());
                            Err(E::InvalidField(head.typ(), field.typ()))
                        }
                    }
                }
                _ => {
                    context.trace(head_node.1.clone());
                    Err(E::InvalidHead(head.typ()))
                }
            }
        }
        Node(N::IfExpr { cond: cond_node, node, else_node }, pos) => {
            let (cond, _) = interpret(cond_node, context)?;
            if V::bool(&cond) == V::Bool(true) {
                return interpret(node, context)
            }
            interpret(else_node, context)
        }

        // structure
        Node(N::Return(node), _) => {
            let (value, _) = interpret(node, context)?;
            Ok((value, R::Return))
        }
        Node(N::Break, _) => {
            Ok((V::Null, R::Break))
        }
        Node(N::Continue, _) => {
            Ok((V::Null, R::Continue))
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
        Node(N::Proc { name: name_node, params: param_nodes, body: body_node }, pos) => {
            if let Node(N::ID(id), name_pos) = name_node.as_ref() {
                let mut params: Vec<(String, Option<Node>, bool)> = vec![];
                for (param_node, type_node_, apply) in param_nodes {
                    if let Node(N::ID(id), param_pos) = param_node {
                        let mut typ: Option<Node> = None;
                        if let Some(type_node) = type_node_ {
                            let (type_value, _) = interpret(type_node, context)?;
                            if let V::Type(_) = type_value {
                                typ = Some(type_node.clone());
                            } else if let V::Rule(_, _, _) = type_value {
                                typ = Some(type_node.clone());
                            } else {
                                context.trace(type_node.1.clone());
                                return Err(E::ExpectedType(Type::Type, type_value.typ()))
                            }
                        }
                        params.push((id.clone(), typ, *apply))
                    } else {
                        context.trace(param_node.1.clone());
                        return Err(E::ExpectedNode(N::ID("_".into()), param_node.0.clone()))
                    }
                }
                context.def(id, &V::Proc(params.clone(), body_node.as_ref().clone()));
                return Ok((V::Null, R::None))
            }
            context.trace(name_node.1.clone());
            Err(E::ExpectedNode(N::ID("_".into()), name_node.0.clone()))
        }
        Node(N::Rule { name: name_node, id: id_node, rules }, _) => {
            if let Node(N::ID(name), name_pos) = name_node.as_ref() {
                if let Node(N::ID(id), id_pos) = id_node.as_ref() {
                    context.def(name, &V::Rule(name.clone(), id.clone(), rules.clone()));
                    return Ok((V::Null, R::None))
                }
                context.trace(id_node.1.clone());
                return Err(E::ExpectedNode(N::ID("_".into()), id_node.0.clone()))
            }
            context.trace(name_node.1.clone());
            Err(E::ExpectedNode(N::ID("_".into()), name_node.0.clone()))
        }
        Node(N::Call { id: id_node, args }, pos) => {
            context.trace(pos.clone());
            let (proc, _) = interpret(id_node, context)?;
            let mut arg_values: Vec<V> = vec![];
            for arg in args.iter() {
                let (value, _) = interpret(arg, context)?;
                arg_values.push(value);
            }
            let mut old_context = Context::from(context);
            *context = Context::proc(context);
            match proc {
                V::Proc(ref params, ref body) => {
                    assign_params(params, arg_values, pos, context)?;
                    interpret(body, context)?;
                }
                V::ForeignProc(ref params, ref func) => {
                    assign_params(params, arg_values, pos, context)?;
                    func(context, pos)?;
                }
                _ => {
                    context.trace(pos.clone());
                    return Err(E::ExpectedType(Type::Union(vec![Type::Proc, Type::ForeignProc]), proc.typ()))
                }
            }
            *context = old_context;
            Ok((V::Null, R::None))
        }
        Node(N::If { cond: cond_node, body, else_body }, _) => {
            let (cond, _) = interpret(cond_node, context)?;
            if V::bool(&cond) == V::Bool(true) {
                return interpret(body, context)
            } else if let Some(else_body) = else_body {
                return interpret(else_body, context)
            }
            Ok((V::Null, R::None))
        }
        Node(N::While { cond: cond_node, body }, _) => {
            let (mut cond, _) = interpret(cond_node, context)?;
            while V::bool(&cond) == V::Bool(true) {
                let (value, ret) = interpret(body, context)?;
                if ret == R::Return { return Ok((value, ret)) }
                if ret == R::Break { break }
                (cond, _) = interpret(cond_node, context)?;
            }
            Ok((V::Null, R::None))
        }
        Node(N::For { param: param_node, iter: iter_node, body }, pos) => {
            if let Node(N::ID(param), param_pos) = param_node.as_ref() {
                let (iter, _) = interpret(iter_node, context)?;
                match iter {
                    V::Vector(values, _) => {
                        for v in values.iter() {
                            context.set(param, v);
                            let (value, ret) = interpret(body, context)?;
                            if ret == R::Return { return Ok((value, ret)) }
                            if ret == R::Break { break }
                        }
                        Ok((V::Null, R::None))
                    }
                    V::Tuple(values) => {
                        for v in values.iter() {
                            context.set(param, v);
                            let (value, ret) = interpret(body, context)?;
                            if ret == R::Return { return Ok((value, ret)) }
                            if ret == R::Break { break }
                        }
                        Ok((V::Null, R::None))
                    }
                    V::Object(values) => {
                        for (k, v) in values.iter() {
                            context.set(param, &V::Tuple(vec![V::String(k.clone()), v.clone()]));
                            let (value, ret) = interpret(body, context)?;
                            if ret == R::Return { return Ok((value, ret)) }
                            if ret == R::Break { break }
                        }
                        Ok((V::Null, R::None))
                    }
                    _ => {
                        context.trace(iter_node.1.clone());
                        Err(E::InvalidIterator(iter.typ()))
                    }
                }
            } else {
                context.trace(param_node.1.clone());
                Err(E::ExpectedNode(N::ID("_".into()), param_node.0.clone()))
            }
        }
    }
}