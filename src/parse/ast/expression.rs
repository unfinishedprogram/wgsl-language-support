use chumsky::prelude::*;

mod relational_expression;
use regex::Regex;

use crate::parse::{
    literal::Literal,
    span::{SpanAble, WithSpan},
    tokenizer::Token,
};

use super::{ParserInput, RichErr};

// https://www.w3.org/TR/WGSL/#syntax-primary_expression
#[derive(Debug, Clone)]
pub enum PrimaryExpression {
    TemplateElaboratedIdent(TemplateElaboratedIdent),
    CallExpression(CallExpression),
    Literal(Literal),
    ParenExpression(Expression),
}

#[derive(Debug, Clone)]
enum ComponentOrSwizzleSpecifierInner {
    IndexExpression(Expression),
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
pub struct SingularExpression(PrimaryExpression, Option<ComponentOrSwizzleSpecifier>);

#[derive(Debug, Clone)]
pub enum Expression {
    EXPR,
}

pub fn expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> + Clone {
    any().map(|_| Expression::EXPR)
}

pub fn template_arg_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateArgExpression, RichErr<'src, 'tokens>>
{
    expression_parser().map(TemplateArgExpression)
}

pub fn argument_expression_list_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Expression>, RichErr<'src, 'tokens>> {
    expression_parser()
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::SyntaxToken("(")), just(Token::SyntaxToken(")")))
}

pub fn template_list_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateList, RichErr<'src, 'tokens>> {
    template_arg_expression_parser()
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::TemplateArgsStart), just(Token::TemplateArgsEnd))
        .map(TemplateList)
}

pub fn template_elaborated_ident_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, TemplateElaboratedIdent, RichErr<'src, 'tokens>>
{
    select!(Token::Ident(ident) => ident.to_owned())
        .then(template_list_parser().or_not())
        .map(|(ident, templates)| TemplateElaboratedIdent(Ident(ident), templates))
}

pub fn call_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, CallExpression, RichErr<'src, 'tokens>> {
    template_elaborated_ident_parser()
        .then(argument_expression_list_parser())
        .map(|(ident, args)| CallExpression(CallPhrase(ident, ArgumentExpressionList(args))))
}

pub fn paren_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> {
    expression_parser().delimited_by(just(Token::SyntaxToken("(")), just(Token::SyntaxToken(")")))
}

pub fn literal_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Literal, RichErr<'src, 'tokens>> {
    select! { Token::Literal(lit) => lit }
}

pub fn primary_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, WithSpan<PrimaryExpression>, RichErr<'src, 'tokens>>
{
    choice((
        template_elaborated_ident_parser().map(PrimaryExpression::TemplateElaboratedIdent),
        literal_parser().map(PrimaryExpression::Literal),
        call_expression_parser().map(PrimaryExpression::CallExpression),
        paren_expression_parser().map(PrimaryExpression::ParenExpression),
    ))
    .map_with(|ident, extra| ident.with_span(extra.span()))
}

pub fn component_or_swizzle_specifier_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ComponentOrSwizzleSpecifier, RichErr<'src, 'tokens>>
{
    let swizzle_regex = Regex::new("^[rgba]{1,4}|[xyzw]{1,4}$").unwrap();

    recursive(|this| {
        let index_expr = expression_parser()
            .delimited_by(just(Token::SyntaxToken("[")), just(Token::SyntaxToken("]")))
            .map(ComponentOrSwizzleSpecifierInner::IndexExpression);

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

pub fn singular_expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, SingularExpression, RichErr<'src, 'tokens>> {
    primary_expression_parser()
        .then(component_or_swizzle_specifier_parser().or_not())
        .map(|(primary, access)| SingularExpression(primary.into_inner(), access))
}
