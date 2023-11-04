use chumsky::prelude::*;

use super::literal::Literal;

type Span = SimpleSpan<usize>;
type RichErr<'src> = extra::Err<Rich<'src, char, Span>>;

#[derive(Debug)]
pub enum Token<'src> {
    Literal(Literal<'src>),
    SyntaxToken(&'src str),
    Ident(&'src str),
    Trivia,
    TemplateArgsStart,
    TemplateArgsEnd,
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

pub fn syntax_token<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    let l3 = choice((just("<<="), just(">>=")));

    let l2 = choice((
        just("=="),
        just("!="),
        just("<="),
        just(">="),
        just("&&"),
        just("||"),
        just("<<"),
        just(">>"),
        just("->"),
        just("=>"),
        just("++"),
        just("--"),
        just("+="),
        just("-="),
        just("*="),
        just("/="),
        just("%="),
        just("&="),
        just("|="),
        just("^="),
    ));

    let l1 = choice((
        just("("),
        just(")"),
        just("["),
        just("]"),
        just("{"),
        just("}"),
        just(";"),
        just("."),
        just(","),
        just(":"),
        just("&"),
        just("@"),
        just("="),
        just(">"),
        just("<"),
        just("%"),
        just("/"),
        just("+"),
        just("-"),
        just("*"),
    ));

    // Order is important here
    choice((l3, l2, l1)).map(Token::SyntaxToken)
}

pub fn template_delimiter<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    choice((
        just('⋖').map(|_| Token::TemplateArgsStart),
        just('⋗').map(|_| Token::TemplateArgsEnd),
    ))
}

pub fn ident<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    text::unicode::ident().map(Token::Ident)
}

pub fn trivia<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    let line_comment = just("//")
        .then(none_of('\n').repeated())
        .padded()
        .map(|_| Token::Trivia);

    let block_comment = {
        // TODO: Make this recursive
        let content = any().and_is(just("/*").or(just("*/")).not()).repeated();
        content.delimited_by(just("/*"), just("*/"))
    }
    .map(|_| Token::Trivia);

    choice((line_comment, block_comment))
}

pub fn tokenizer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, RichErr<'src>> {
    let token = choice((trivia(), literal(), template_delimiter(), syntax_token())).or(ident());

    token
        // Add spans to all tokens
        .map_with(|tok, e| (tok, e.span()))
        // Remove whitespace
        .padded()
        // Error recovery
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
