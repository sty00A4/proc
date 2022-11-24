#[derive(Clone)]
pub enum V {
    Wildcard, Null,
    Int(i64), Float(f64), Bool(bool), String(String)
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
                Self::String(v2) => v1.to_owned() == v2.to_owned(),
                Self::Wildcard => true,
                _ => false
            }
            _ => false
        }
    }
}