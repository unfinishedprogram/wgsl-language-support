pub mod declaration;

use chumsky::prelude::*;

use crate::front::token::Keyword;

use self::declaration::{declaration, variable_or_value_decl, Declaration};

use super::{
    attribute::Attribute,
    expression::{
        call_expression, core_lhs_expression, expression, lhs_expression,
        relational_expression::{
            AdditiveOperator, BinaryOperator, BitwiseOperator, MultiplicativeOperator,
            ShiftOperator,
        },
        template_elaborated_ident, CallPhrase, Expression, LHSExpression, TemplateElaboratedIdent,
    },
    ParserInput, RichErr, Token,
};
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Trivia,
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
    FuncCall(CallPhrase),
    Discard,
    Loop {
        loop_attributes: Vec<Attribute>,
        body_attributes: Vec<Attribute>,
        body: Vec<Statement>,
    },

    For {
        attributes: Vec<Attribute>,
        init: Box<Option<Statement>>,
        expression: Box<Option<Expression>>,
        update: Box<Option<Statement>>,
        body: Vec<Statement>,
    },
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
        .labelled("assignment statement")
}

fn inc_dec_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    core_lhs_expression(lhs_expression())
        .then(choice((
            just(Token::SyntaxToken("++")),
            just(Token::SyntaxToken("--")),
        )))
        .map(|(lhs, op)| match op {
            Token::SyntaxToken("++") => Statement::Increment(lhs),
            Token::SyntaxToken("--") => Statement::Decrement(lhs),
            _ => unreachable!(),
        })
        .labelled("increment/decrement statement")
}

fn return_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    just(Token::Keyword(Keyword::Return))
        .ignore_then(expression())
        .map(Statement::Return)
        .labelled("return statement")
}

fn discard_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    just(Token::Keyword(Keyword::Discard))
        .map(|_| Statement::Discard)
        .labelled("discard statement")
}

fn compound_statement<'tokens, 'src: 'tokens>(
    stmt: impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Statement>, RichErr<'src, 'tokens>> + Clone
{
    just(Token::Trivia)
        .or_not()
        .ignore_then(stmt)
        .then_ignore(just(Token::Trivia).or_not())
        .repeated()
        .collect()
        .delimited_by(just(Token::SyntaxToken("{")), just(Token::SyntaxToken("}")))
        .labelled("compound statement")
}

fn loop_statement<'tokens, 'src: 'tokens>(
    stmt: impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    let loop_attributes = Attribute::list_parser();
    let body_attributes = Attribute::list_parser();

    loop_attributes
        .then(
            just(Token::Keyword(Keyword::Loop))
                .ignore_then(body_attributes.then(compound_statement(stmt.clone()))),
        )
        .map(
            |(loop_attributes, (body_attributes, body))| Statement::Loop {
                loop_attributes,
                body_attributes,
                body,
            },
        )
        .labelled("loop statement")
}

fn for_statement<'tokens, 'src: 'tokens>(
    stmt: impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Statement, RichErr<'src, 'tokens>> + Clone {
    let init = choice((
        variable_or_value_decl().map(Statement::Declaration),
        assignment_statement(),
        inc_dec_statement(),
        call_expression(expression()).map(Statement::FuncCall),
    ));

    let update = choice((
        call_expression(expression()).map(Statement::FuncCall),
        assignment_statement(),
        inc_dec_statement(),
    ));

    let header = init
        .or_not()
        .then_ignore(just(Token::SyntaxToken(";")))
        .then(expression().or_not())
        .then_ignore(just(Token::SyntaxToken(";")))
        .then(update.or_not())
        .map(|((a, b), c)| (a, b, c));

    Attribute::list_parser()
        .then_ignore(just(Token::Keyword(Keyword::For)))
        .then(header.delimited_by(just(Token::SyntaxToken("(")), just(Token::SyntaxToken(")"))))
        .then(compound_statement(stmt))
        .map(
            |((attributes, (init, expression, update)), body)| Statement::For {
                attributes,
                init: Box::new(init),
                expression: Box::new(expression),
                update: Box::new(update),
                body,
            },
        )
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
        .labelled("if statement")
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
            just(Token::Trivia)
                .map(|_| Statement::Trivia)
                .labelled("trivia"),
            if_statement(this.clone()),
            loop_statement(this.clone()),
            for_statement(this.clone()),
            declaration(this.clone()).map(Statement::Declaration),
            choice((
                inc_dec_statement(),
                call_expression(expression()).map(Statement::FuncCall),
                assignment_statement(),
                return_statement(),
                discard_statement(),
            ))
            .then_ignore(just(Token::SyntaxToken(";"))),
        ))
        .memoized()
    })
    .labelled("statement")
}
