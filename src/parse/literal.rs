#[derive(Debug)]
pub enum Literal<'src> {
    Boolean(bool),
    Int(&'src str),
    Float(&'src str),
}
