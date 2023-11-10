use chumsky::prelude::*;

use crate::parse::{
    literal::Literal,
    span::{SpanAble, WithSpan},
    tokenizer::Token,
};

use super::{ParserInput, RichErr};

// https://www.w3.org/TR/WGSL/#syntax-primary_expression
pub enum PrimaryExpression {
    TemplateElaboratedIdent(TemplateElaboratedIdent),
    CallExpression(CallExpression),
    Literal(Literal),
    ParenExpression(Expression),
}

pub struct Ident(String);

pub struct TemplateElaboratedIdent(Ident, Option<TemplateList>);

pub struct TemplateList(Vec<TemplateArgExpression>);

pub struct TemplateArgExpression(Expression);

pub struct CallExpression(CallPhrase);

pub struct CallPhrase(TemplateElaboratedIdent, ArgumentExpressionList);

pub struct ArgumentExpressionList(Vec<Expression>);

pub enum Expression {}

pub fn expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>> {
    todo()
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
