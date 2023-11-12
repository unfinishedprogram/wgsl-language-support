use crate::parse::tokenizer::Token;

use super::{
    super::{ParserInput, RichErr},
    singular_expression, Expression,
};
use chumsky::prelude::*;

#[derive(Debug, Clone)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Clone)]
pub enum AdditiveOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum ShiftOperator {
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negative,
    Not,
    BitNot,
    Deref,
    AddrOf,
}

#[derive(Debug, Clone)]
pub enum ShortCircuitOperator {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum BitwiseOperator {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
pub enum RelationalOperator {
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Multiplicative(MultiplicativeOperator),
    Additive(AdditiveOperator),
    Shift(ShiftOperator),
    ShortCircuit(ShortCircuitOperator),
    Bitwise(BitwiseOperator),
    Relational(RelationalOperator),
}

pub fn relational_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        shift_expression(expr.clone())
            .then(select! {
                Token::SyntaxToken("<") => RelationalOperator::LessThan,
                Token::SyntaxToken(">") => RelationalOperator::GreaterThan,
                Token::SyntaxToken("<=") => RelationalOperator::LessThanEqual,
                Token::SyntaxToken(">=") => RelationalOperator::GreaterThanEqual,
                Token::SyntaxToken("==") => RelationalOperator::Equal,
                Token::SyntaxToken("!=") => RelationalOperator::NotEqual,
            })
            .then(shift_expression(expr.clone()))
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Relational(op),
                    Box::new(right),
                )
            }),
        shift_expression(expr),
    ))
    .memoized()
}

pub fn shift_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        unary_expression(expr.clone())
            .then(select! {
                Token::SyntaxToken("<<") => ShiftOperator::ShiftLeft,
                Token::SyntaxToken(">>") => ShiftOperator::ShiftRight,
            })
            .then(unary_expression(expr.clone()))
            .map(|((left, op), right)| {
                Expression::Binary(Box::new(left), BinaryOperator::Shift(op), Box::new(right))
            }),
        additive_expression(expr.clone()),
    ))
    .memoized()
}

// DONE
pub fn additive_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(select! {
                Token::SyntaxToken("+") => AdditiveOperator::Plus,
                Token::SyntaxToken("-") => AdditiveOperator::Minus,
            })
            .then(multiplicative_expression(expr.clone()))
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Additive(op),
                    Box::new(right),
                )
            }),
            multiplicative_expression(expr.clone()),
        ))
        .memoized()
    })
}

// DONE
pub fn multiplicative_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(select! {
                Token::SyntaxToken("*") => MultiplicativeOperator::Multiply,
                Token::SyntaxToken("/") => MultiplicativeOperator::Divide,
                Token::SyntaxToken("%") => MultiplicativeOperator::Modulo,
            })
            .then(unary_expression(expr.clone()))
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Multiplicative(op),
                    Box::new(right),
                )
            }),
            unary_expression(expr.clone()),
        ))
        .memoized()
    })
}

// DONE
pub fn unary_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            choice((
                just(Token::SyntaxToken("-")).map(|_| UnaryOperator::Negative),
                just(Token::SyntaxToken("!")).map(|_| UnaryOperator::Not),
                just(Token::SyntaxToken("~")).map(|_| UnaryOperator::BitNot),
                just(Token::SyntaxToken("*")).map(|_| UnaryOperator::Deref),
                just(Token::SyntaxToken("&")).map(|_| UnaryOperator::AddrOf),
            ))
            .then(this)
            .map(|(op, expr)| Expression::Unary(op, Box::new(expr))),
            singular_expression(expr),
        ))
    })
}

pub fn short_circuit_or_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(just(Token::SyntaxToken("||")))
                .then(relational_expression(expr.clone()))
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::ShortCircuit(ShortCircuitOperator::Or),
                        Box::new(right),
                    )
                }),
            relational_expression(expr.clone()),
        ))
        .memoized()
    })
}

pub fn short_circuit_and_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(just(Token::SyntaxToken("&&")))
                .then(relational_expression(expr.clone()))
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::ShortCircuit(ShortCircuitOperator::And),
                        Box::new(right),
                    )
                }),
            relational_expression(expr.clone()),
        ))
        .memoized()
    })
}

pub fn binary_and_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(just(Token::SyntaxToken("&")))
                .then(unary_expression(expr.clone()))
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::And),
                        Box::new(right),
                    )
                }),
            unary_expression(expr.clone()),
        ))
        .memoized()
    })
}

pub fn binary_or_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(just(Token::SyntaxToken("|")))
                .then(unary_expression(expr.clone()))
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::Or),
                        Box::new(right),
                    )
                }),
            unary_expression(expr.clone()),
        ))
        .memoized()
    })
}

pub fn binary_xor_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            this.then(just(Token::SyntaxToken("^")))
                .then(unary_expression(expr.clone()))
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::Xor),
                        Box::new(right),
                    )
                }),
            unary_expression(expr.clone()),
        ))
        .memoized()
    })
}

pub fn bitwise_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        binary_and_expression(expr.clone())
            .then(just(Token::SyntaxToken("&")).map(|_| BitwiseOperator::And)),
        binary_or_expression(expr.clone())
            .then(just(Token::SyntaxToken("|")).map(|_| BitwiseOperator::Or)),
        binary_xor_expression(expr.clone())
            .then(just(Token::SyntaxToken("^")).map(|_| BitwiseOperator::Xor)),
    ))
    .then(unary_expression(expr))
    .map(|((left, operator), right)| {
        Expression::Binary(
            Box::new(left),
            BinaryOperator::Bitwise(operator),
            Box::new(right),
        )
    })
}
