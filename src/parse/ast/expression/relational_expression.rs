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
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        shift_expression()
            .then(select! {
                Token::SyntaxToken("<") => RelationalOperator::LessThan,
                Token::SyntaxToken(">") => RelationalOperator::GreaterThan,
                Token::SyntaxToken("<=") => RelationalOperator::LessThanEqual,
                Token::SyntaxToken(">=") => RelationalOperator::GreaterThanEqual,
                Token::SyntaxToken("==") => RelationalOperator::Equal,
                Token::SyntaxToken("!=") => RelationalOperator::NotEqual,
            })
            .then(shift_expression())
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Relational(op),
                    Box::new(right),
                )
            }),
        shift_expression(),
    ))
}

pub fn shift_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        additive_expression(),
        unary_expression()
            .then(select! {
                Token::SyntaxToken("<<") => ShiftOperator::ShiftLeft,
                Token::SyntaxToken(">>") => ShiftOperator::ShiftRight,
            })
            .then(unary_expression())
            .map(|((left, op), right)| {
                Expression::Binary(Box::new(left), BinaryOperator::Shift(op), Box::new(right))
            }),
    ))
}

// DONE
pub fn additive_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            multiplicative_expression(),
            this.then(select! {
                Token::SyntaxToken("+") => AdditiveOperator::Plus,
                Token::SyntaxToken("-") => AdditiveOperator::Minus,
            })
            .then(multiplicative_expression())
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Additive(op),
                    Box::new(right),
                )
            }),
        ))
    })
}

// DONE
pub fn multiplicative_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            unary_expression(),
            this.then(select! {
                Token::SyntaxToken("*") => MultiplicativeOperator::Multiply,
                Token::SyntaxToken("/") => MultiplicativeOperator::Divide,
                Token::SyntaxToken("%") => MultiplicativeOperator::Modulo,
            })
            .then(unary_expression())
            .map(|((left, op), right)| {
                Expression::Binary(
                    Box::new(left),
                    BinaryOperator::Multiplicative(op),
                    Box::new(right),
                )
            }),
        ))
    })
}

// DONE
pub fn unary_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            singular_expression(),
            choice((
                just(Token::SyntaxToken("-")).map(|_| UnaryOperator::Negative),
                just(Token::SyntaxToken("!")).map(|_| UnaryOperator::Not),
                just(Token::SyntaxToken("~")).map(|_| UnaryOperator::BitNot),
                just(Token::SyntaxToken("*")).map(|_| UnaryOperator::Deref),
                just(Token::SyntaxToken("&")).map(|_| UnaryOperator::AddrOf),
            ))
            .then(this)
            .map(|(op, expr)| Expression::Unary(op, Box::new(expr))),
        ))
    })
}

pub fn short_circuit_or_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            relational_expression(),
            this.then(just(Token::SyntaxToken("||")))
                .then(relational_expression())
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::ShortCircuit(ShortCircuitOperator::Or),
                        Box::new(right),
                    )
                }),
        ))
    })
}

pub fn short_circuit_and_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            relational_expression(),
            this.then(just(Token::SyntaxToken("&&")))
                .then(relational_expression())
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::ShortCircuit(ShortCircuitOperator::And),
                        Box::new(right),
                    )
                }),
        ))
    })
}

pub fn binary_and_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            unary_expression(),
            this.then(just(Token::SyntaxToken("&")))
                .then(unary_expression())
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::And),
                        Box::new(right),
                    )
                }),
        ))
    })
}

pub fn binary_or_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            unary_expression(),
            this.then(just(Token::SyntaxToken("|")))
                .then(unary_expression())
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::Or),
                        Box::new(right),
                    )
                }),
        ))
    })
}

pub fn binary_xor_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            unary_expression(),
            this.then(just(Token::SyntaxToken("^")))
                .then(unary_expression())
                .map(|((left, _), right)| {
                    Expression::Binary(
                        Box::new(left),
                        BinaryOperator::Bitwise(BitwiseOperator::Xor),
                        Box::new(right),
                    )
                }),
        ))
    })
}

pub fn bitwise_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        binary_and_expression().then(just(Token::SyntaxToken("&")).map(|_| BitwiseOperator::And)),
        binary_or_expression().then(just(Token::SyntaxToken("|")).map(|_| BitwiseOperator::Or)),
        binary_xor_expression().then(just(Token::SyntaxToken("^")).map(|_| BitwiseOperator::Xor)),
    ))
    .then(unary_expression())
    .map(|((left, operator), right)| {
        Expression::Binary(
            Box::new(left),
            BinaryOperator::Bitwise(operator),
            Box::new(right),
        )
    })
}
