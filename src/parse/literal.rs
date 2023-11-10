#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Boolean(bool),
    Int(String),
    Float(String),
}
