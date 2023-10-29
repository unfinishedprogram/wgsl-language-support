use chumsky::prelude::*;

use super::literal::Literal;

type Span = SimpleSpan<usize>;

#[derive(Debug)]
pub enum Token<'src> {
    Literal(Literal<'src>),
    Keyword(&'src str),
    ReservedWord(&'src str),
    SyntaxToken(&'src str),
    Ident(&'src str),
    ContextDependantName(&'src str),

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

pub fn literal<'src>(
) -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>> {
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

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let separator = one_of(":;,").map(Token::Separator);

    let attribute = just("@").map(|_| Token::Attribute);

    let word = text::ascii::ident().map(Token::Word);

    let hex_int_literal = regex("/0[xX][0-9a-fA-F]+[iu]?/")
        .padded()
        .map(Token::Number);

    let line_comment = just("//").then(none_of('\n').repeated()).padded();

    // TODO: Make this recursive
    let block_comment = {
        let content = any().and_is(just("/*").or(just("*/")).not()).repeated();
        content.delimited_by(just("/*"), just("*/"))
    };

    let number = hex_int_literal;

    let token = choice((literal(), separator, attribute, number, word));

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
