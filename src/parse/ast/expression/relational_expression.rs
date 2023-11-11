use super::{
    super::{ParserInput, RichErr},
    Expression,
};
use chumsky::prelude::*;

pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

pub enum AdditiveOperator {
    Plus,
    Minus,
}

pub enum UnaryOperator {}

pub fn relational_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> {
    todo()
}

pub fn shift_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> {
    todo()
}
