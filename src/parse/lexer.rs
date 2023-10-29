use chumsky::prelude::*;

type Span = SimpleSpan<usize>;

type Number = i32;
type NumberError = i32;

#[derive(Debug)]
pub enum Token<'src> {
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

    let token = choice((separator, attribute, number, word));

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
