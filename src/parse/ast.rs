use chumsky::prelude::*;
pub mod expression;
pub mod statement;
use expression::Expression;

pub type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<
    Token<'src>,
    SimpleSpan<usize>,
    &'tokens [(Token<'src>, SimpleSpan<usize>)],
>;

use super::{span::WithSpan, tokenizer::Token};

type Span = SimpleSpan<usize>;

pub struct Ast<'src> {
    pub src: &'src str,
    pub nodes: Vec<WithSpan<AstNode>>,
}

pub struct Block {
    expressions: Vec<WithSpan<Expression>>,
}

pub enum Declaration {
    Type,
    TypeGenerator,
    Value,
    Variable,
    Function,
    FormalParameter,
    Enumerant,
}

pub enum AstNode {
    Block(Block),
    Declaration(Declaration),
    Expression(Expression),
}

type RichErr<'src, 'tokens> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn ast_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<AstNode>, RichErr<'src, 'tokens>> {
    todo()
    // choice((function(), function_call())).repeated().collect()
}
