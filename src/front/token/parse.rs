use super::{ident, Keyword, Literal, RichErr, Token};
use crate::front::Span;

use chumsky::prelude::*;

pub fn keyword<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    use Keyword::*;

    choice((
        just("alias").map(|_| Alias),
        just("break").map(|_| Break),
        just("case").map(|_| Case),
        just("const").map(|_| Const),
        just("constAssert").map(|_| ConstAssert),
        just("continue").map(|_| Continue),
        just("continuing").map(|_| Continuing),
        just("default").map(|_| Default),
        just("diagnostic").map(|_| Diagnostic),
        just("discard").map(|_| Discard),
        just("else").map(|_| Else),
        just("enable").map(|_| Enable),
        just("fn").map(|_| Fn),
        just("for").map(|_| For),
        just("if").map(|_| If),
        just("let").map(|_| Let),
        just("loop").map(|_| Loop),
        just("override").map(|_| Override),
        just("requires").map(|_| Requires),
        just("return").map(|_| Return),
        just("struct").map(|_| Struct),
        just("switch").map(|_| Switch),
        just("var").map(|_| Var),
        just("while").map(|_| While),
    ))
    .map(Token::Keyword)
}

pub fn literal<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    let boolean_literal = choice((
        just("true").map(|_| Literal::Boolean(true)),
        just("false").map(|_| Literal::Boolean(false)),
    ));

    let int_literal = {
        let decimal = regex("0[iu]?").or(regex("[1-9][0-9]*[iu]?"));
        let hex = regex("0[xX][0-9a-fA-F]+[iu]?");

        choice((hex, decimal)).map(|s: &str| Literal::Int(s.to_string()))
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

        choice((decimal, hex)).map(|s: &str| Literal::Float(s.to_string()))
    };

    choice((float_literal, boolean_literal, int_literal)).map(Token::Literal)
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
        just("|"),
        just("^"),
        just("@"),
        just("="),
        just(">"),
        just("<"),
        just("%"),
        just("/"),
        just("+"),
        just("-"),
        just("*"),
        just("~"),
        just("!"),
    ));

    // Order is important here
    choice((l3, l2, l1)).map(Token::SyntaxToken)
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
    let token = choice((trivia(), keyword(), literal(), syntax_token(), ident()));
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
