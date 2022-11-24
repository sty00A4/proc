#[derive(Clone, Debug)]
pub enum E {
    TargetFile(String),
    FileNotFound(String),
    Todo(String),
}
impl std::fmt::Display for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TargetFile(v) => write!(f, "ERROR: target file '{v}' not found"),
            Self::FileNotFound(v) => write!(f, "ERROR: file '{v}' not found"),
            Self::Todo(v) => write!(f, "ERROR: todo - {v}"),
        }
    }
}