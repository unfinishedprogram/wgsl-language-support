use super::expression::Expression;

pub enum Attribute {
    Align(Expression),
    Binding(Expression),
    Builtin(Expression),
    Const,
    Diagnostic(DiagnosticControl),
    Group(Expression),
    Id(Expression),
    Interpolate(Interpolate),
    Invariant,
    Location(Expression),
    MustUse,
    Size(Expression),
    WorkgroupSize(WorkgroupSize),
    Vertex,
    Fragment,
    Compute,
}

// TODO Implement Diagnostics
pub struct DiagnosticControl;

pub enum WorkgroupSize {
    D1(Expression),
    D2(Expression, Expression),
    D3(Expression, Expression, Expression),
}

pub enum Interpolate {
    D1(Expression),
    D2(Expression, Expression),
}
