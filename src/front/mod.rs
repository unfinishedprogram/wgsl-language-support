use chumsky::span::SimpleSpan;

pub mod ast;
pub mod span;
pub mod token;

type Span = SimpleSpan<usize>;
