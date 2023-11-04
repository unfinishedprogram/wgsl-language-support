use chumsky::prelude::*;

use super::literal::Literal;

type Span = SimpleSpan<usize>;
type RichErr<'src> = extra::Err<Rich<'src, char, Span>>;

#[derive(Debug)]
pub enum Token<'src> {
    Literal(Literal<'src>),
    Keyword(&'src str),
    ReservedWord(&'src str),
    SyntaxToken(&'src str),
    Ident(&'src str),
    ContextDependantName(&'src str),
    TemplateArgsStart,
    TemplateArgsEnd,

    Separator(char),
    Paren(char),
    Attribute,
    Number(&'src str),
    Word(&'src str),
    Operation(char),
    LogicalOperation(char),
    ShiftOperation(char),
    AssignmentOperation(char),
    IncrementOperation,
    DecrementOperation,
    Arrow,
    Unknown(char),
    Trivia,
    End,
    Comment,
}

pub fn literal<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    let boolean_literal = choice((
        just("true").map(|_| Literal::Boolean(true)),
        just("false").map(|_| Literal::Boolean(false)),
    ));

    let int_literal = {
        let decimal = regex("0[iu]?").or(regex("[1-9][0-9]*[iu]?"));
        let hex = regex("0[xX][0-9a-fA-F]+[iu]?");

        choice((hex, decimal)).map(Literal::Int)
    };

    let float_literal = {
        let decimal = choice((
            regex("0[fh]"),
            regex("[1-9][0-9]*[fh]"),
            regex("[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?[fh]?"),
            regex("[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?[fh]?"),
            regex("[0-9]+[eE][+-]?[0-9]+[fh]?"),
        ));

        let hex = choice((
            regex("0[xX][0-9a-fA-F]*\\.[0-9a-fA-F]+([pP][+-]?[0-9]+[fh]?)?"),
            regex("0[xX][0-9a-fA-F]+\\.[0-9a-fA-F]*([pP][+-]?[0-9]+[fh]?)?"),
            regex("0[xX][0-9a-fA-F]+[pP][+-]?[0-9]+[fh]?"),
        ));

        choice((decimal, hex)).map(Literal::Float)
    };

    choice((boolean_literal, int_literal, float_literal)).map(Token::Literal)
}

pub fn keyword<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    choice((
        just("alias"),
        just("break"),
        just("case"),
        just("const"),
        just("const_assert"),
        just("continue"),
        just("continuing"),
        just("default"),
        just("diagnostic"),
        just("discard"),
        just("else"),
        just("enable"),
        just("false"),
        just("fn"),
        just("for"),
        just("if"),
        just("let"),
        just("loop"),
        just("override"),
        just("requires"),
        just("return"),
        just("struct"),
        just("switch"),
        just("true"),
        just("var"),
        just("while"),
    ))
    .map(Token::Keyword)
}

pub fn syntax_token<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    todo()
}

pub fn template_delimiter<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    choice((
        just('⋖').map(|_| Token::TemplateArgsStart),
        just('⋗').map(|_| Token::TemplateArgsEnd),
    ))
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, RichErr<'src>> {
    let line_comment = just("//").then(none_of('\n').repeated()).padded();
    // TODO: Make this recursive
    let block_comment = {
        let content = any().and_is(just("/*").or(just("*/")).not()).repeated();
        content.delimited_by(just("/*"), just("*/"))
    };

    // TODO: Reserved words
    let token = choice((keyword(), literal(), template_delimiter()));

    token
        // Add spans to all tokens
        .map_with(|tok, e| (tok, e.span()))
        // Remove comments
        .padded_by(line_comment.repeated())
        .padded_by(block_comment.repeated())
        // Remove whitespace
        .padded()
        // Error recovery
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
