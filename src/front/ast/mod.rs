pub mod expression;
pub mod statement;

use self::statement::{statement, Statement};
use super::token::Token;
use chumsky::prelude::*;

type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<
    Token<'src>,
    SimpleSpan<usize>,
    &'tokens [(Token<'src>, SimpleSpan<usize>)],
>;

type RichErr<'src, 'tokens> = extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>;

pub enum Declaration {
    Type,
    TypeGenerator,
    Value,
    Variable,
    Function,
    FormalParameter,
    Enumerant,
}

pub fn ast_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Statement>, RichErr<'src, 'tokens>> {
    just(Token::Trivia)
        .repeated()
        .ignore_then(statement())
        .then_ignore(just(Token::Trivia).repeated())
        .repeated()
        .collect()
}
