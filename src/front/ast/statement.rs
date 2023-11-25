pub mod declaration;

use chumsky::prelude::*;

use crate::front::token::Keyword;

use self::declaration::{declaration, Declaration};

use super::{
    expression::{
        expression, lhs_expression,
        relational_expression::{
            AdditiveOperator, BinaryOperator, BitwiseOperator, MultiplicativeOperator,
            ShiftOperator,
        },
        template_elaborated_ident, Expression, LHSExpression, TemplateElaboratedIdent,
    },
    ParserInput, RichErr, Token,
};
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Compound(Vec<Statement>),
    ContinuingCompound(Vec<Statement>),
    Assignment(LHSExpression, AssignmentOperator, Expression),
    Increment(LHSExpression),
    Decrement(LHSExpression),
    Return(Expression),
    If(
        (Expression, Vec<Statement>),
        Vec<(Expression, Vec<Statement>)>,
        Option<Vec<Statement>>,
    ),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignmentOperator {
    Simple,
    Compound(BinaryOperator),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OptionallyTypedIdent(String, Option<TemplateElaboratedIdent>);

fn assignment_operator<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, AssignmentOperator, RichErr<'src, 'tokens>> + Clone
{
    select! {
        Token::SyntaxToken("=") => AssignmentOperator::Simple,
        Token::SyntaxToken("+=") => AssignmentOperator::Compound(BinaryOperator::Additive(AdditiveOperator::Plus)),
        Token::SyntaxToken("-=") => AssignmentOperator::Compound(BinaryOperator::Additive(AdditiveOperator::Minus)),
        Token::SyntaxToken("*=") => AssignmentOperator::Compound(BinaryOperator::Multiplicative(MultiplicativeOperator::Multiply)),
        Token::SyntaxToken("/=") => AssignmentOperator::Compound(BinaryOperator::Multiplicative(MultiplicativeOperator::Divide)),
        Token::SyntaxToken("%=") => AssignmentOperator::Compound(BinaryOperator::Multiplicative(MultiplicativeOperator::Modulo)),
        Token::SyntaxToken("&=") => AssignmentOperator::Compound(BinaryOperator::Bitwise(BitwiseOperator::And)),
        Token::SyntaxToken("|=") => AssignmentOperator::Compound(BinaryOperator::Bitwise(BitwiseOperator::Or)),
        Token::SyntaxToken("^=") => AssignmentOperator::Compound(BinaryOperator::Bitwise(BitwiseOperator::Xor)),
        Token::SyntaxToken(">>=") => AssignmentOperator::Compound(BinaryOperator::Shift(ShiftOperator::Right)),
        Token::SyntaxToken("<<=") => AssignmentOperator::Compound(BinaryOperator::Shift(ShiftOperator::Left)),
    }
}

fn assignment_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    lhs_expression()
        .then(assignment_operator())
        .then(expression())
        .map(|((lhs, operator), rhs)| Statement::Assignment(lhs, operator, rhs))
}

fn inc_dec_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    lhs_expression()
        .then(choice((
            just(Token::SyntaxToken("++")),
            just(Token::SyntaxToken("--")),
        )))
        .map(|(lhs, op)| match op {
            Token::SyntaxToken("++") => Statement::Increment(lhs),
            Token::SyntaxToken("--") => Statement::Decrement(lhs),
            _ => unreachable!(),
        })
}

fn return_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    just(Token::Keyword(Keyword::Return))
        .ignore_then(expression())
        .map(Statement::Return)
}

fn compound_statement<'tokens, 'src: 'tokens>(
    stmt: impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Statement>, RichErr<'src, 'tokens>> + Clone
{
    stmt.repeated()
        .collect()
        .delimited_by(just(Token::SyntaxToken("{")), just(Token::SyntaxToken("}")))
}

fn if_statement<'tokens, 'src: 'tokens>(
    stmt: impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    let if_clause = just(Token::Keyword(Keyword::If))
        .ignore_then(expression())
        .then(compound_statement(stmt.clone()));

    let else_if_clause = just(Token::Keyword(Keyword::Else))
        .ignore_then(just(Token::Keyword(Keyword::If)))
        .ignore_then(expression())
        .then(compound_statement(stmt.clone()));

    let else_clause =
        just(Token::Keyword(Keyword::Else)).ignore_then(compound_statement(stmt.clone()));

    if_clause
        .then(else_if_clause.repeated().collect())
        .then(else_clause.or_not())
        .map(|(((if_expr, if_body), else_ifs), else_body)| {
            Statement::If((if_expr, if_body), else_ifs, else_body)
        })
}

fn optionally_typed_ident<'tokens, 'src: 'tokens>(
    expr: impl Parser<'tokens, ParserInput<'tokens, 'src>, Expression, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, OptionallyTypedIdent, RichErr<'src, 'tokens>> + Clone
{
    let type_specifier = template_elaborated_ident(expr);

    select!(Token::Ident(ident) => ident.to_owned())
        .then(
            just(Token::SyntaxToken(":"))
                .ignore_then(type_specifier)
                .or_not(),
        )
        .map(|(ident, type_specifier)| OptionallyTypedIdent(ident, type_specifier))
}

pub fn statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    recursive(|this| {
        choice((
            assignment_statement(),
            inc_dec_statement(),
            return_statement(),
            if_statement(this.clone()),
            declaration().map(Statement::Declaration),
        ))
        .memoized()
    })
}
