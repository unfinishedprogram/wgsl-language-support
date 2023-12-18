use chumsky::prelude::*;

mod lhs_expression;
pub mod relational_expression;
pub use lhs_expression::{core_lhs_expression, lhs_expression, LHSExpression};

use self::relational_expression::{
    AdditiveOperator, BinaryOperator, BitwiseOperator, MultiplicativeOperator, RelationalOperator,
    ShiftOperator, ShortCircuitOperator, UnaryOperator,
};
use super::{ParserInput, RichErr};
use crate::front::token::{Literal, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ComponentOrSwizzleSpecifierInner {
    IndexExpression(Box<Expression>),
    MemberAccess(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComponentOrSwizzleSpecifier(
    ComponentOrSwizzleSpecifierInner,
    Option<Box<ComponentOrSwizzleSpecifier>>,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateElaboratedIdent(Ident, Option<TemplateList>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateList(Vec<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallPhrase(TemplateElaboratedIdent, ArgumentExpressionList);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentExpressionList(Vec<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    None,
    TemplateElaboratedIdent(TemplateElaboratedIdent),
    CallExpression(CallPhrase),
    Literal(Literal),
    ParenExpression(Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Singular(Box<Expression>, Option<ComponentOrSwizzleSpecifier>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
}

pub fn st<'tokens, 'src: 'tokens>(
    t: &'src str,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Token, RichErr<'src, 'tokens>> + Clone {
    just(Token::SyntaxToken(t))
}

pub fn expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    let ident_str = select!(Token::Ident(ident) => ident.to_owned());
    let primary_expression = primary_expression();
    recursive(|expression| {
        let component_or_swizzle_specifier = recursive(|this| {
            choice((
                expression
                    .delimited_by(st("["), st("]"))
                    .map(Box::new)
                    .map(ComponentOrSwizzleSpecifierInner::IndexExpression), // | `'['` expression `']'` component_or_swizzle_specifier ?
                st(".")
                    .ignore_then(ident_str)
                    .map(ComponentOrSwizzleSpecifierInner::MemberAccess), // | `'.'` swizzle_name component_or_swizzle_specifier ?
                st(".")
                    .ignore_then(ident_str)
                    .map(ComponentOrSwizzleSpecifierInner::MemberAccess), // | `'.'` member_ident component_or_swizzle_specifier ?
            ))
            .then(this.or_not())
            .map(|(inner, next)| ComponentOrSwizzleSpecifier(inner, next.map(Box::new)))
        })
        .boxed();

        let unary_expression = {
            let unary_op = choice((
                st("!").to(UnaryOperator::Not),
                st("&").to(UnaryOperator::AddrOf),
                st("*").to(UnaryOperator::Deref),
                st("-").to(UnaryOperator::Negative),
                st("~").to(UnaryOperator::BitNot),
            ))
            .boxed();

            unary_op.repeated().foldr(primary_expression, |op, expr| {
                Expression::Unary(op, Box::new(expr))
            })
        }
        .boxed();

        let bitwise_expression__post_unary_expression = {
            let make_rel =
                |prev, (op, next)| Expression::Binary(Box::new(prev), op, Box::new(next));

            let bit_and = st("&").to(BinaryOperator::Bitwise(BitwiseOperator::And));
            let bit_xor = st("^").to(BinaryOperator::Bitwise(BitwiseOperator::Xor));
            let bit_or = st("|").to(BinaryOperator::Bitwise(BitwiseOperator::Or));

            choice((
                unary_expression.clone().then(bit_and.clone()).then(
                    unary_expression
                        .clone()
                        .foldl(bit_and.then(unary_expression.clone()).repeated(), make_rel),
                ),
                unary_expression.clone().then(bit_xor.clone()).then(
                    unary_expression
                        .clone()
                        .foldl(bit_xor.then(unary_expression.clone()).repeated(), make_rel),
                ),
                unary_expression.clone().then(bit_or.clone()).then(
                    unary_expression
                        .clone()
                        .foldl(bit_or.then(unary_expression.clone()).repeated(), make_rel),
                ),
            ))
            .map(|((lhs, op), rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs)))
        }
        .boxed();

        let shift_expression__post_unary_expression = {
            use ShiftOperator::*;

            choice((
                unary_expression
                    .clone()
                    .then(st(">>").to(BinaryOperator::Shift(Right)))
                    .then(unary_expression.clone())
                    .map(|((expr1, op), expr2)| {
                        Expression::Binary(Box::new(expr1), op, Box::new(expr2))
                    }),
                unary_expression
                    .clone()
                    .then(st("<<").to(BinaryOperator::Shift(Left)))
                    .then(unary_expression.clone())
                    .map(|((expr1, op), expr2)| {
                        Expression::Binary(Box::new(expr1), op, Box::new(expr2))
                    }),
                // | ( multiplicative_operator unary_expression )* ( additive_operator unary_expression ( multiplicative_operator unary_expression )* )*,
            ))
        }
        .boxed();

        let relational_expression__post_unary_expression = {
            use RelationalOperator::*;

            let ops = choice((
                st("==").to(Equal),
                st("!=").to(NotEqual),
                st("<=").to(LessThanEqual),
                st("<").to(LessThan),
                st(">=").to(GreaterThanEqual),
                st(">").to(GreaterThan),
            ));

            let relational_ops = shift_expression__post_unary_expression
                .clone()
                .then(ops)
                .then(shift_expression__post_unary_expression.clone())
                .map(|((lhs, op), rhs)| {
                    Expression::Binary(Box::new(lhs), BinaryOperator::Relational(op), Box::new(rhs))
                });

            choice((relational_ops, shift_expression__post_unary_expression))
        }
        .boxed();

        let make_rel = |prev, (op, next)| {
            Expression::Binary(
                Box::new(prev),
                BinaryOperator::ShortCircuit(op),
                Box::new(next),
            )
        };

        let make_unary = |op_char, op| {
            st(op_char)
                .to(op)
                .then(unary_expression.clone())
                .repeated()
                .at_least(1)
        };

        // Expression
        choice((
            relational_expression__post_unary_expression
                .clone()
                .then(st("&&").to(BinaryOperator::ShortCircuit(ShortCircuitOperator::And)))
                .then(
                    relational_expression__post_unary_expression
                        .clone()
                        .foldl(make_unary("&&", ShortCircuitOperator::And), make_rel),
                )
                .map(|((lhs, op), rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs))),
            relational_expression__post_unary_expression
                .clone()
                .then(st("||").to(BinaryOperator::ShortCircuit(ShortCircuitOperator::Or)))
                .then(
                    relational_expression__post_unary_expression
                        .clone()
                        .foldl(make_unary("||", ShortCircuitOperator::Or), make_rel),
                )
                .map(|((lhs, op), rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs))),
            relational_expression__post_unary_expression.clone(),
            bitwise_expression__post_unary_expression,
        ))
    })
}

pub fn template_elaborated_ident__post_ident<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateElaboratedIdent, RichErr<'src, 'tokens>>
       + Clone {
    let ident = select!(Token::Ident(ident) => Ident(ident.to_owned()));

    let template_args = expression()
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .at_least(1)
        .collect()
        .delimited_by(just(Token::TemplateArgsStart), just(Token::TemplateArgsEnd))
        .map(TemplateList);

    ident
        .then(template_args.or_not())
        .map(|(ident, template)| TemplateElaboratedIdent(ident, template))
}

pub fn call_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, CallPhrase, RichErr<'src, 'tokens>> + Clone {
    template_elaborated_ident__post_ident()
        .then(
            expression()
                .separated_by(st(","))
                .allow_trailing()
                .collect()
                .delimited_by(st("("), st(")"))
                .map(ArgumentExpressionList),
        )
        .map(|(ident, args)| CallPhrase(ident, args))
}

#[allow(non_snake_case)]
pub fn primary_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    let expression = expression();

    let paren_expression = expression
        .clone()
        .delimited_by(st("("), st(")"))
        .map(Box::new)
        .map(Expression::ParenExpression);
    let literal = select!(Token::Literal(lit) => Expression::Literal(lit));

    choice((
        paren_expression,
        literal,
        call_expression().map(Expression::CallExpression),
        template_elaborated_ident__post_ident().map(Expression::TemplateElaboratedIdent),
    ))
}

#[cfg(test)]
mod test {

    use crate::front::{
        ast::expression::relational_expression::{AdditiveOperator, MultiplicativeOperator},
        token::{parse::tokenizer, template::insert_template_delimiters},
    };

    use super::*;

    fn parse_from_source(source: &'static str) -> Expression {
        let templated = insert_template_delimiters(source);
        let tokens = tokenizer().parse(&templated).unwrap();

        let ast = expression().parse(
            tokens
                .as_slice()
                .spanned((source.len()..source.len()).into()),
        );

        ast.unwrap()
    }

    #[test]
    fn binary_additive() {
        let result = parse_from_source("1 + 2");
        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(Literal::Int("1".to_owned()))),
                BinaryOperator::Additive(AdditiveOperator::Plus),
                Box::new(Expression::Literal(Literal::Int("2".to_owned())))
            )
        );
    }

    #[test]
    fn binary_additive_chain() {
        let result = parse_from_source("1 + 2 + 3");
        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Int("1".to_owned()))),
                    BinaryOperator::Additive(AdditiveOperator::Plus),
                    Box::new(Expression::Literal(Literal::Int("2".to_owned())))
                )),
                BinaryOperator::Additive(AdditiveOperator::Plus),
                Box::new(Expression::Literal(Literal::Int("3".to_owned())))
            )
        );
    }

    #[test]
    fn binary_additive_operator_precedence() {
        let result = parse_from_source("1 + 2 * 3");
        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(Literal::Int("1".to_owned()))),
                BinaryOperator::Additive(AdditiveOperator::Plus),
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Int("2".to_owned()))),
                    BinaryOperator::Multiplicative(MultiplicativeOperator::Multiply),
                    Box::new(Expression::Literal(Literal::Int("3".to_owned())))
                ))
            )
        );
    }

    #[test]
    fn test_complex_expression() {
        let result = parse_from_source("1 + 2 * 3 + 4");
        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Int("1".to_owned()))),
                    BinaryOperator::Additive(AdditiveOperator::Plus),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Int("2".to_owned()))),
                        BinaryOperator::Multiplicative(MultiplicativeOperator::Multiply),
                        Box::new(Expression::Literal(Literal::Int("3".to_owned())))
                    ))
                )),
                BinaryOperator::Additive(AdditiveOperator::Plus),
                Box::new(Expression::Literal(Literal::Int("4".to_owned())))
            )
        );
    }
}
