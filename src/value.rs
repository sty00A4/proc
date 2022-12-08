use std::{collections::{HashSet, HashMap}, hash::Hash};
use crate::*;

pub type ProcFn = fn(&mut Context, &Position) -> Result<V, E>;
pub type ProcValueParams = Vec<(String, Option<Node>, bool)>;
pub type Rules = Vec<(Node, Option<Node>)>;

#[derive(Clone)]
pub enum V {
    Wildcard, Null,
    Int(i64), Float(f64), Bool(bool), String(String),
    Tuple(Vec<V>), Vector(Vec<V>, Type), Object(HashMap<String, V>), Container(Context),
    Proc(ProcValueParams, Node),
    ForeignProc(ProcValueParams, ProcFn),
    Rule(String, String, Rules),
    Type(Type)
}
impl std::fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::Tuple(v) => write!(f, "({})", v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Vector(v, _) => write!(f, "{v:?}"),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v}")).collect::<Vec<String>>().join(", ")),
            Self::Container(context) => write!(f, "container:{:?}", context as *const Context),
            Self::Proc(_, body) => write!(f, "proc:{:?}", body as *const Node),
            Self::ForeignProc(_, func) => write!(f, "foreign-proc:{:?}", func as *const ProcFn),
            Self::Rule(name, _, rules) => write!(f, "{name}-rule:{:?}", rules as *const Rules),
            Self::Type(v) => write!(f, "{v}"),
        }
    }
}
impl std::fmt::Debug for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Null => write!(f, "null"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Tuple(v) => write!(f, "({})", v.iter().map(|x| format!("{x:?}")).collect::<Vec<String>>().join(", ")),
            Self::Vector(v, _) => write!(f, "{v:?}"),
            Self::Object(v) => write!(f, "{{ {} }}", v.iter().map(|(k, v)| format!("{k} = {v:?}")).collect::<Vec<String>>().join(", ")),
            Self::Container(context) => write!(f, "container:{:?}", context as *const Context),
            Self::Proc(_, body) => write!(f, "proc:{:?}", body as *const Node),
            Self::ForeignProc(_, func) => write!(f, "foreign-proc:{:?}", func as *const ProcFn),
            Self::Rule(name, _, rules) => write!(f, "{name}-rule:{:?}", rules as *const Rules),
            Self::Type(v) => write!(f, "{v:?}"),
        }
    }
}
impl PartialEq for V {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Wildcard => true,
            Self::Null => match other {
                Self::Null => true,
                Self::Wildcard => true,
                _ => false
            }
            Self::Int(v1) => match other {
                Self::Int(v2) => *v1 == *v2,
                Self::Float(v2) => *v1 as f64 == *v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Float(v1) => match other {
                Self::Int(v2) => *v1 == *v2 as f64,
                Self::Float(v2) => *v1 == *v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Bool(v1) => match other {
                Self::Bool(v2) => *v1 == *v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::String(v1) => match other {
                Self::String(v2) => v1 == v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Tuple(v1) => match other {
                Self::Tuple(v2) => v1 == v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Vector(v1, t1) => match other {
                Self::Vector(v2, t2) => v1 == v2 && t1 == t2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Object(v1) => match other {
                Self::Object(v2) => v1 == v2,
                Self::Wildcard => true,
                _ => false
            }
            Self::Container(c1) => match other {
                Self::Container(c2) => c1 as *const Context == c2 as *const Context,
                Self::Wildcard => true,
                _ => false
            }
            Self::Proc(params1, body1) => match other {
                Self::Proc(params2, body2) => (body1 as *const Node) == (body2 as *const Node),
                Self::Wildcard => true,
                _ => false
            }
            Self::ForeignProc(params1, func1) => match other {
                Self::ForeignProc(params2, func2) => (func1 as *const ProcFn) == (func2 as *const ProcFn),
                Self::Wildcard => true,
                _ => false
            }
            Self::Rule(name1, _, rules1) => match other {
                Self::Rule(name2, _, rules2) => name1 == name2 &&
                (rules1 as *const Rules) == (rules2 as *const Rules),
                Self::Wildcard => true,
                _ => false
            }
            Self::Type(v1) => match other {
                Self::Type(v2) => v1 == v2,
                _ => false
            }
        }
    }
}
impl V {
    pub fn typ(&self) -> Type {
        match self {
            Self::Wildcard => Type::Any,
            Self::Null => Type::Undefined,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::String(_) => Type::String,
            Self::Tuple(v) => Type::Tuple(v.iter().map(|x| x.typ()).collect()),
            Self::Vector(_, t) => Type::Vector(Box::new(t.clone())),
            Self::Object(_) => Type::Object,
            Self::Container(_) => Type::Container,
            Self::Proc(_, _) => Type::Proc,
            Self::ForeignProc(_, _) => Type::ForeignProc,
            Self::Rule(name, _, _) => Type::Rule(name.clone()),
            Self::Type(_) => Type::Type,
        }
    }
    pub fn bool(value: &V) -> Self {
        Type::Bool.cast(value).unwrap_or_else(|| Self::Bool(false))
    }
    pub fn create_union(values: Vec<Self>) -> Vec<Type> {
        let mut collected: Vec<Type> = vec![];
        for v in values {
            if !collected.contains(&v.typ()) {
                collected.push(v.typ());
            }
        }
        collected
    }
}

#[derive(Clone)]
pub enum Type {
    Any, Undefined,
    Int, Float, Bool, String,
    Tuple(Vec<Type>), Vector(Box<Type>), Object, Container,
    Proc, ForeignProc, Rule(String),
    Type,
    Union(Vec<Type>), Scission(Vec<Type>)
}
impl Type {
    pub fn create_union(types: Vec<Self>) -> Self {
        let mut collected: Vec<Self> = vec![];
        for t in types {
            if !collected.contains(&t) {
                if let Self::Union(sub_types) = t {
                    let typ = Self::create_union(sub_types);
                    if let Self::Union(sub_types) = typ {
                        for typ in sub_types {
                            collected.push(typ);
                        }
                    } else {
                        collected.push(typ);
                    }
                } else {
                    collected.push(t);
                }
            }
        }
        if collected.len() == 1 { return collected[0].clone() }
        Self::Union(collected)
    }
    pub fn create_scission(types: Vec<Self>) -> Self {
        let mut collected: Vec<Self> = vec![];
        for t in types {
            if !collected.contains(&t) {
                if let Self::Union(sub_types) = t {
                    let typ = Self::create_scission(sub_types);
                    if let Self::Scission(sub_types) = typ {
                        for typ in sub_types {
                            collected.push(typ);
                        }
                    } else {
                        collected.push(typ);
                    }
                } else {
                    collected.push(t);
                }
            }
        }
        Self::Scission(collected)
    }
    pub fn cast(&self, value: &V) -> Option<V> {
        match self {
            Type::Any => Some(value.clone()),
            Type::Undefined => Some(V::Null),
            Type::Int => match value {
                V::Int(_) => Some(value.clone()),
                V::Float(v) => Some(V::Int(*v as i64)),
                V::Bool(v) => Some(V::Int(*v as i64)),
                V::Wildcard | V::Null => Some(V::Int(0)),
                V::String(v) => match v.parse::<i64>() {
                    Ok(v) => Some(V::Int(v)),
                    Err(_) => None,
                }
                _ => None
            }
            Type::Float => match value {
                V::Float(_) => Some(value.clone()),
                V::Int(v) => Some(V::Float(*v as f64)),
                V::Bool(v) => Some(V::Float(*v as u8 as f64)),
                V::Wildcard | V::Null => Some(V::Float(0.0)),
                V::String(v) => match v.parse::<f64>() {
                    Ok(v) => Some(V::Float(v)),
                    Err(_) => None,
                }
                _ => None
            }
            Type::Bool => match value {
                V::Bool(_) => Some(value.clone()),
                V::Int(v) => Some(V::Bool(*v == 0)),
                V::Float(v) => Some(V::Bool(*v == 0.0)),
                V::Null => Some(V::Bool(false)),
                V::String(v) => match v.parse::<bool>() {
                    Ok(v) => Some(V::Bool(v)),
                    Err(_) => None,
                }
                _ => Some(V::Bool(true))
            }
            Type::String => Some(V::String(value.to_string())),
            Type::Tuple(typ) => match value {
                V::Tuple(_) => Some(value.clone()),
                _ => None
            }
            Type::Vector(typ) => match value {
                V::Vector(_, _) => Some(value.clone()),
                _ => None
            }
            Type::Object => None,
            Type::Container => None,
            Type::Proc => None,
            Type::ForeignProc => None,
            Type::Rule(_) => None,
            Type::Type => Some(V::Type(value.typ())),
            Type::Union(_) => None,
            Type::Scission(_) => None,
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "any"),
            Self::Undefined => write!(f, "undefined"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "str"),
            Self::Tuple(types) => write!(f, "({})", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Vector(t) => write!(f, "vec[{t}]"),
            Self::Object => write!(f, "obj"),
            Self::Container => write!(f, "container"),
            Self::Proc => write!(f, "proc"),
            Self::ForeignProc => write!(f, "foreign-proc"),
            Self::Rule(name) => write!(f, "{name}-rule"),
            Self::Type => write!(f, "type"),
            Self::Union(types) => write!(f, "{}", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("|")),
            Self::Scission(types) => write!(f, "scission[{}]", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("|")),
        }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Any => true,
            Self::Undefined => match other {
                Self::Undefined => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Int => match other {
                Self::Int => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Float => match other {
                Self::Float => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Bool => match other {
                Self::Bool => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::String => match other {
                Self::String => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Tuple(t1) => match other {
                Self::Tuple(t2) => t1 == t2,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Vector(t1) => match other {
                Self::Vector(t2) => t1.as_ref() == t2.as_ref(),
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Object => match other {
                Self::Object => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Container => match other {
                Self::Container => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Proc => match other {
                Self::Proc => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::ForeignProc => match other {
                Self::ForeignProc => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Rule(name1) => match other {
                // todo rule comp, node comp
                Self::Rule(name2) => name1 == name2,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Type => match other {
                Self::Type => true,
                Self::Any => true,
                Self::Union(_) => other == self,
                Self::Scission(_) => other == self,
                _ => false
            }
            Self::Union(t1) => match other {
                Self::Union(t2) => {
                    for type1 in t1.iter() {
                        let mut matches = false;
                        for type2 in t2.iter() {
                            if type1 == type2 { matches = true; break }
                        }
                        if !matches { return false }
                    }
                    true
                }
                Self::Scission(t2) => {
                    for type1 in t1.iter() {
                        let mut matches = false;
                        for type2 in t2.iter() {
                            if type1 == type2 { matches = true; break }
                        }
                        if matches { return false }
                    }
                    true
                }
                Self::Any => true,
                _ => {
                    for t in t1.iter() {
                        if t == other { return true }
                    }
                    false
                }
            }
            Self::Scission(t1) => match other {
                Self::Union(t2) => {
                    for type1 in t1.iter() {
                        let mut matches = false;
                        for type2 in t2.iter() {
                            if type1 == type2 { matches = true; break }
                        }
                        if matches { return false }
                    }
                    true
                }
                Self::Scission(t2) => {
                    for type1 in t1.iter() {
                        let mut matches = false;
                        for type2 in t2.iter() {
                            if type1 == type2 { matches = true; break }
                        }
                        if !matches { return false }
                    }
                    true
                }
                Self::Any => true,
                _ => {
                    for t in t1.iter() {
                        if t == other { return false }
                    }
                    true
                }
            }
        }
    }
}