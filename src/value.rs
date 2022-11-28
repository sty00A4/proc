use std::{collections::{HashSet, HashMap}, hash::Hash};

#[derive(Clone)]
pub enum V {
    Wildcard, Null,
    Int(i64), Float(f64), Bool(bool), String(String),
    Tuple(Vec<V>),
    Vector(Vec<V>, Type), Object(HashMap<String, V>),
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
            Self::Object(v) => write!(f, "obj:{:?}", v as *const HashMap<String, V>),
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
            Self::Object(v) => write!(f, "obj:{:?}", v as *const HashMap<String, V>),
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
                _ => false
            }
            Self::Vector(v1, t1) => match other {
                Self::Vector(v2, t2) => v1 == v2 && t1 == t2,
                _ => false
            }
            Self::Object(v1) => match other {
                Self::Object(v2) => v1 == v2,
                _ => false
            }
            Self::Type(v1) => match other {
                Self::Type(v2) => v1 == v2,
                _ => false
            }
        }
    }
}

#[derive(Clone)]
pub enum Type {
    Any, Undefiend,
    Int, Float, Bool, String,
    Tuple(Vec<Type>), Vector(Box<Type>), Object,
    Union(Vec<Type>), Scission(Vec<Type>)
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
            Self::Undefiend => write!(f, "undefined"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::String => write!(f, "str"),
            Self::Tuple(types) => write!(f, "({})", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Vector(t) => write!(f, "vec"),
            Self::Object => write!(f, "obj"),
            Self::Union(types) => write!(f, "union[{}]", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("|")),
            Self::Scission(types) => write!(f, "scission[{}]", types.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("|")),
        }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Any => true,
            Self::Undefiend => match other {
                Self::Undefiend => true,
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