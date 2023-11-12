use chumsky::prelude::*;

mod relational_expression;
use regex::Regex;

use crate::parse::{literal::Literal, tokenizer::Token};

use self::relational_expression::{
    bitwise_expression, relational_expression, short_circuit_and_expression,
    short_circuit_or_expression, BinaryOperator, UnaryOperator,
};

use super::{ParserInput, RichErr};

#[derive(Debug, Clone)]
enum ComponentOrSwizzleSpecifierInner {
    IndexExpression(Box<Expression>),
    MemberAccess(MemberIdent),
    Swizzle(SwizzleName),
}

#[derive(Debug, Clone)]
pub struct ComponentOrSwizzleSpecifier(
    ComponentOrSwizzleSpecifierInner,
    Box<Option<ComponentOrSwizzleSpecifier>>,
);

#[derive(Debug, Clone)]
pub struct Ident(String);

#[derive(Debug, Clone)]
pub struct MemberIdent(String);

#[derive(Debug, Clone)]
pub struct SwizzleName(String);

#[derive(Debug, Clone)]
pub struct TemplateElaboratedIdent(Ident, Option<TemplateList>);

#[derive(Debug, Clone)]
pub struct TemplateList(Vec<TemplateArgExpression>);

#[derive(Debug, Clone)]
pub struct TemplateArgExpression(Expression);

#[derive(Debug, Clone)]
pub struct CallExpression(CallPhrase);

#[derive(Debug, Clone)]
pub struct CallPhrase(TemplateElaboratedIdent, ArgumentExpressionList);

#[derive(Debug, Clone)]
pub struct ArgumentExpressionList(Vec<Expression>);

#[derive(Debug, Clone)]
pub enum Expression {
    None,
    TemplateElaboratedIdent(TemplateElaboratedIdent),
    CallExpression(CallExpression),
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
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, CallExpression, RichErr<'src, 'tokens>> + Clone
{
    template_elaborated_ident(expr.clone())
        .then(argument_expression_list(expr))
        .map(|(ident, args)| CallExpression(CallPhrase(ident, ArgumentExpressionList(args))))
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
        template_elaborated_ident(expr.clone()).map(Expression::TemplateElaboratedIdent),
        literal().map(Expression::Literal),
        call_expression(expr.clone()).map(Expression::CallExpression),
        paren_expression(expr).map(|expr| Expression::ParenExpression(Box::new(expr))),
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
    let swizzle_regex = Regex::new("^[rgba]{1,4}|[xyzw]{1,4}$").unwrap();

    recursive(|this| {
        let index_expr = expr
            .delimited_by(just(Token::SyntaxToken("[")), just(Token::SyntaxToken("]")))
            .map(|expr| ComponentOrSwizzleSpecifierInner::IndexExpression(Box::new(expr)));

        let swizzle_name = just(Token::SyntaxToken("."))
            .then(
                select! {Token::Ident(ident) if swizzle_regex.is_match(ident) => ident.to_owned()},
            )
            .map(|(_, ident)| ComponentOrSwizzleSpecifierInner::Swizzle(SwizzleName(ident)));

        let member_ident = just(Token::SyntaxToken("."))
            .then(select! {Token::Ident(ident) => ident.to_owned()})
            .map(|(_, ident)| ComponentOrSwizzleSpecifierInner::MemberAccess(MemberIdent(ident)));

        choice((index_expr, swizzle_name, member_ident))
            .then(this.or_not())
            .map(|(inner, extra)| ComponentOrSwizzleSpecifier(inner, Box::new(extra)))
    })
}

fn singular_expression<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    primary_expression(expr.clone())
        .then(component_or_swizzle_specifier(expr).or_not())
        .map(|(primary, access)| Expression::Singular(Box::new(primary), access))
}

pub fn expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    recursive(|expr| {
        choice((
            relational_expression(expr.clone()),
            short_circuit_or_expression(expr.clone()),
            short_circuit_and_expression(expr.clone()),
            bitwise_expression(expr),
        ))
    })
}
