use chumsky::prelude::*;

mod lhs_expression;
pub mod relational_expression;
pub use lhs_expression::{lhs_expression, LHSExpression};

use crate::parse::{literal::Literal, tokenizer::Token};

use self::relational_expression::{
    bitwise_expression, relational_expression, short_circuit_and_expression,
    short_circuit_or_expression, BinaryOperator, UnaryOperator,
};

use super::{ParserInput, RichErr};

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
pub struct TemplateList(Vec<TemplateArgExpression>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateArgExpression(Expression);

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

fn template_arg_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateArgExpression, RichErr<'src, 'tokens>>
       + Clone {
    expr.map(TemplateArgExpression)
}

fn argument_expression_list<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Expression>, RichErr<'src, 'tokens>> + Clone
{
    expr.separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::SyntaxToken("(")), just(Token::SyntaxToken(")")))
}

fn template_list<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateList, RichErr<'src, 'tokens>> + Clone
{
    template_arg_expression(expr)
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::TemplateArgsStart), just(Token::TemplateArgsEnd))
        .map(TemplateList)
}

fn template_elaborated_ident<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateElaboratedIdent, RichErr<'src, 'tokens>>
       + Clone {
    select!(Token::Ident(ident) => ident.to_owned())
        .then(template_list(expr).or_not())
        .map(|(ident, templates)| TemplateElaboratedIdent(Ident(ident), templates))
}

fn call_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, CallPhrase, RichErr<'src, 'tokens>> + Clone {
    template_elaborated_ident(expr.clone())
        .then(argument_expression_list(expr))
        .map(|(ident, args)| CallPhrase(ident, ArgumentExpressionList(args)))
}

fn paren_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    expr.delimited_by(just(Token::SyntaxToken("(")), just(Token::SyntaxToken(")")))
}

fn literal<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Literal, RichErr<'src, 'tokens>> + Clone {
    select! { Token::Literal(lit) => lit }
}

fn primary_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    choice((
        paren_expression(expr.clone()).map(|expr| Expression::ParenExpression(Box::new(expr))),
        call_expression(expr.clone()).map(Expression::CallExpression),
        literal().map(Expression::Literal),
        template_elaborated_ident(expr.clone()).map(Expression::TemplateElaboratedIdent),
    ))
}

fn component_or_swizzle_specifier<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    ComponentOrSwizzleSpecifier,
    RichErr<'src, 'tokens>,
> + Clone {
    recursive(|this| {
        let index_expr = expr
            .delimited_by(just(Token::SyntaxToken("[")), just(Token::SyntaxToken("]")))
            .map(|expr| ComponentOrSwizzleSpecifierInner::IndexExpression(Box::new(expr)));

        let member_ident_or_swizzle = just(Token::SyntaxToken("."))
            .then(select! {Token::Ident(ident) => ident.to_owned()})
            .map(|(_, ident)| ComponentOrSwizzleSpecifierInner::MemberAccess(ident));

        choice((member_ident_or_swizzle, index_expr))
            .then(this.or_not())
            .map(|(inner, extra)| ComponentOrSwizzleSpecifier(inner, extra.map(Box::new)))
    })
}

fn singular_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    primary_expression(expr.clone())
        .then(component_or_swizzle_specifier(expr).or_not())
        .map(|(primary, access)| match access {
            Some(access) => Expression::Singular(Box::new(primary), Some(access)),
            None => primary,
        })
}

pub fn expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|expr| {
        choice((
            bitwise_expression(expr.clone()),
            short_circuit_and_expression(expr.clone()),
            short_circuit_or_expression(expr.clone()),
            relational_expression(expr.clone()),
        ))
        .memoized()
    })
    .recover_with(skip_then_retry_until(any().ignored(), end()))
}

#[cfg(test)]
mod test {
    use crate::parse::ast::expression::relational_expression::{
        AdditiveOperator, MultiplicativeOperator,
    };

    use super::*;

    fn parse_from_source(source: &'static str) -> Expression {
        let templated = crate::parse::templates::insert_template_delimiters(source);
        let tokens = crate::parse::tokenizer::tokenizer()
            .parse(&templated)
            .unwrap();

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
