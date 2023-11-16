pub enum Statement {
    Compound(Vec<Statement>),
    ContinuingCompound(Vec<Statement>),
    Assignment,
}
