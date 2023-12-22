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
pub struct TemplateElaboratedIdent(String, Option<TemplateList>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateList(Vec<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallPhrase(TemplateElaboratedIdent, ArgumentExpressionList);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentExpressionList(Vec<Expression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    None,
    Ident(TemplateElaboratedIdent),
    CallExpression(CallPhrase),
    Literal(Literal),
    ParenExpression(Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Singular(Box<Expression>, ComponentOrSwizzleSpecifier),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
}

pub fn st<'tokens, 'src: 'tokens>(
    t: &'src str,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Token, RichErr<'src, 'tokens>> + Clone {
    just(Token::SyntaxToken(t))
}

pub fn component_or_swizzle_specifier<'tokens, 'src: 'tokens>(
    expression: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    ComponentOrSwizzleSpecifier,
    RichErr<'src, 'tokens>,
> + Clone {
    let ident_str = select!(Token::Ident(ident) => ident.to_owned());

    recursive(|this| {
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
    .boxed()
}

pub fn expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|expression| {
        let primary_expression = choice((
            select!(Token::Literal(lit) => Expression::Literal(lit)),
            primary_expression(expression.clone())
                .then(component_or_swizzle_specifier(expression.clone()).or_not())
                .map(|(expr, comp_or_swizz)| match comp_or_swizz {
                    Some(comp_or_swizz) => Expression::Singular(Box::new(expr), comp_or_swizz),
                    None => expr,
                }),
        ));

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

        let bitwise_expression_post_unary_expression = {
            let make_rel =
                |prev, (op, next)| Expression::Binary(Box::new(prev), op, Box::new(next));

            let bit_and = st("&").to(BinaryOperator::Bitwise(BitwiseOperator::And));
            let bit_xor = st("^").to(BinaryOperator::Bitwise(BitwiseOperator::Xor));
            let bit_or = st("|").to(BinaryOperator::Bitwise(BitwiseOperator::Or));

            choice((
                unary_expression.clone().foldl(
                    bit_and
                        .then(unary_expression.clone())
                        .repeated()
                        .at_least(1),
                    make_rel,
                ),
                unary_expression.clone().foldl(
                    bit_xor
                        .then(unary_expression.clone())
                        .repeated()
                        .at_least(1),
                    make_rel,
                ),
                unary_expression.clone().foldl(
                    bit_or.then(unary_expression.clone()).repeated().at_least(1),
                    make_rel,
                ),
            ))
        }
        .boxed();

        let shift_expression_post_unary_expression = {
            let multiplicative_operator = choice((
                st("*").to(BinaryOperator::Multiplicative(
                    MultiplicativeOperator::Multiply,
                )),
                st("/").to(BinaryOperator::Multiplicative(
                    MultiplicativeOperator::Divide,
                )),
                st("%").to(BinaryOperator::Multiplicative(
                    MultiplicativeOperator::Modulo,
                )),
            ));

            let additive_operator = choice((
                st("+").to(BinaryOperator::Additive(AdditiveOperator::Plus)),
                st("-").to(BinaryOperator::Additive(AdditiveOperator::Minus)),
            ));

            let shift_operator = choice((
                st(">>").to(BinaryOperator::Shift(ShiftOperator::Right)),
                st("<<").to(BinaryOperator::Shift(ShiftOperator::Left)),
            ));

            let multiplicative_fold = unary_expression.clone().foldl(
                multiplicative_operator
                    .then(unary_expression.clone())
                    .repeated(),
                |prev, (op, next)| Expression::Binary(Box::new(prev), op, Box::new(next)),
            );

            let additive_fold = multiplicative_fold
                .clone()
                .foldl(
                    additive_operator
                        .then(multiplicative_fold.clone())
                        .repeated(),
                    |prev, (op, next)| Expression::Binary(Box::new(prev), op, Box::new(next)),
                )
                .boxed();

            let shift = unary_expression
                .clone()
                .then(shift_operator)
                .then(unary_expression.clone())
                .map(|((expr1, op), expr2)| {
                    Expression::Binary(Box::new(expr1), op, Box::new(expr2))
                });

            choice((shift, additive_fold))
        }
        .boxed();

        let relational_expression_post_unary_expression = {
            use RelationalOperator::*;

            let ops = choice((
                st("==").to(Equal),
                st("!=").to(NotEqual),
                st("<=").to(LessThanEqual),
                st("<").to(LessThan),
                st(">=").to(GreaterThanEqual),
                st(">").to(GreaterThan),
            ));

            let relational_ops = shift_expression_post_unary_expression
                .clone()
                .then(ops)
                .then(shift_expression_post_unary_expression.clone())
                .map(|((lhs, op), rhs)| {
                    Expression::Binary(Box::new(lhs), BinaryOperator::Relational(op), Box::new(rhs))
                });

            choice((relational_ops, shift_expression_post_unary_expression))
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
            relational_expression_post_unary_expression.clone().foldl(
                make_unary("&&", ShortCircuitOperator::And).at_least(1),
                make_rel,
            ),
            relational_expression_post_unary_expression.clone().foldl(
                make_unary("||", ShortCircuitOperator::Or).at_least(1),
                make_rel,
            ),
            bitwise_expression_post_unary_expression,
            relational_expression_post_unary_expression.clone(),
            unary_expression,
        ))
        .boxed()
    })
}

pub fn spanned_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, (Expression, SimpleSpan), RichErr<'src, 'tokens>>
       + Clone {
    expression().map_with(|expr, e| (expr, e.span()))
}

pub fn template_list<'tokens, 'src: 'tokens>(
    expression: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateList, RichErr<'src, 'tokens>> + Clone
{
    expression
        .clone()
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .at_least(1)
        .collect()
        .delimited_by(just(Token::TemplateArgsStart), just(Token::TemplateArgsEnd))
        .map(TemplateList)
}

pub fn template_elaborated_ident<'tokens, 'src: 'tokens>(
    expression: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateElaboratedIdent, RichErr<'src, 'tokens>>
       + Clone {
    let ident = select!(Token::Ident(ident) => ident.to_owned());
    let template_list = template_list(expression);

    ident
        .then(template_list.or_not())
        .map(|(ident, template)| TemplateElaboratedIdent(ident, template))
}

pub fn call_expression<'tokens, 'src: 'tokens>(
    expression: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, CallPhrase, RichErr<'src, 'tokens>> + Clone {
    template_elaborated_ident(expression.clone())
        .then(
            expression
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
    expression: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    let paren_expression = expression
        .clone()
        .delimited_by(st("("), st(")"))
        .map(Box::new)
        .map(Expression::ParenExpression);
    let literal = select!(Token::Literal(lit) => Expression::Literal(lit));

    choice((
        paren_expression,
        literal,
        call_expression(expression.clone()).map(Expression::CallExpression),
        template_elaborated_ident(expression.clone()).map(Expression::Ident),
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
