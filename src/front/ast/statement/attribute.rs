use chumsky::prelude::*;

use crate::front::token::Keyword;

use crate::front::ast::{
    expression::{expression, Expression},
    ParserInput, RichErr, Token,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Align(Expression),
    Binding(Expression),
    Builtin(Expression),
    Const,
    Diagnostic(DiagnosticControl),
    Group(Expression),
    Id(Expression),
    Interpolate(Interpolate),
    Invariant,
    Location(Expression),
    MustUse,
    Size(Expression),
    WorkgroupSize(WorkgroupSize),
    Vertex,
    Fragment,
    Compute,
}

impl Attribute {
    fn parser<'tokens, 'src: 'tokens>(
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Attribute, RichErr<'src, 'tokens>> + Clone
    {
        let attr_name = |name| just(Token::Ident(name)).labelled("attribute name");

        let expr_param = || {
            just(Token::SyntaxToken("("))
                .ignore_then(expression())
                .then_ignore(just(Token::SyntaxToken(",")).or_not())
                .then_ignore(just(Token::SyntaxToken(")")))
        };

        let no_expr = select! {
            Token::Keyword(Keyword::Const) => Attribute::Const,
            Token::Ident("invariant") => Attribute::Invariant,
            Token::Ident("invariant") => Attribute::Invariant,
            Token::Ident("must_use") => Attribute::MustUse,
            Token::Ident("vertex") => Attribute::Vertex,
            Token::Ident("fragment") => Attribute::Fragment,
            Token::Ident("compute") => Attribute::Compute,
        }
        .labelled("attribute name");

        let special = choice((
            attr_name("diagnostic")
                .ignore_then(DiagnosticControl::parser().map(Attribute::Diagnostic)),
            attr_name("interpolate").ignore_then(Interpolate::parser().map(Attribute::Interpolate)),
            attr_name("workgroup_size")
                .ignore_then(WorkgroupSize::parser().map(Attribute::WorkgroupSize)),
        ));

        let with_expr = choice((
            attr_name("align").ignore_then(expr_param().map(Attribute::Align)),
            attr_name("binding").ignore_then(expr_param().map(Attribute::Binding)),
            attr_name("builtin").ignore_then(expr_param().map(Attribute::Builtin)),
            attr_name("group").ignore_then(expr_param().map(Attribute::Group)),
            attr_name("id").ignore_then(expr_param().map(Attribute::Id)),
            attr_name("location").ignore_then(expr_param().map(Attribute::Location)),
            attr_name("size").ignore_then(expr_param().map(Attribute::Size)),
        ));

        just(Token::SyntaxToken("@")).ignore_then(choice((no_expr, special, with_expr)))
    }

    pub fn list_parser<'tokens, 'src: 'tokens>(
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<Attribute>, RichErr<'src, 'tokens>> + Clone
    {
        Attribute::parser().repeated().collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticControl {
    severity: DiagnosticControlSeverity,
    name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticControlSeverity {
    Error,
    Warning,
    Info,
    Off,
}

impl DiagnosticControl {
    pub fn parser<'tokens, 'src: 'tokens>(
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, DiagnosticControl, RichErr<'src, 'tokens>>
           + Clone {
        let severity = select! {
            Token::Ident("error") => DiagnosticControlSeverity::Error,
            Token::Ident("warning") => DiagnosticControlSeverity::Warning,
            Token::Ident("info") => DiagnosticControlSeverity::Info,
            Token::Ident("off") => DiagnosticControlSeverity::Off,
        };

        just(Token::SyntaxToken("("))
            .ignore_then(severity)
            .then_ignore(just(Token::SyntaxToken(",")))
            .then(select! { Token::Ident(v) => v.to_string() })
            .then_ignore(just(Token::SyntaxToken(",")).or_not())
            .then_ignore(just(Token::SyntaxToken(")")))
            .map(|(severity, name)| Self { severity, name })
            .labelled("diagnostic_control")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WorkgroupSize {
    D1(Expression),
    D2(Expression, Expression),
    D3(Expression, Expression, Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Interpolate {
    D1(Expression),
    D2(Expression, Expression),
}

impl WorkgroupSize {
    pub fn parser<'tokens, 'src: 'tokens>(
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, WorkgroupSize, RichErr<'src, 'tokens>> + Clone
    {
        let d1 = just(Token::SyntaxToken("(")).ignore_then(
            expression()
                .then_ignore(just(Token::SyntaxToken(",")).or_not())
                .then_ignore(just(Token::SyntaxToken(")")))
                .map(Self::D1),
        );

        let d2 = just(Token::SyntaxToken("(")).ignore_then(
            expression()
                .then_ignore(just(Token::SyntaxToken(",")))
                .then(expression())
                .then_ignore(just(Token::SyntaxToken(",")).or_not())
                .then_ignore(just(Token::SyntaxToken(")")))
                .map(|(x, y)| Self::D2(x, y)),
        );

        let d3 = just(Token::SyntaxToken("(")).ignore_then(
            expression()
                .then_ignore(just(Token::SyntaxToken(",")))
                .then(expression())
                .then_ignore(just(Token::SyntaxToken(",")))
                .then(expression())
                .then_ignore(just(Token::SyntaxToken(",")).or_not())
                .then_ignore(just(Token::SyntaxToken(")")))
                .map(|((x, y), z)| Self::D3(x, y, z)),
        );

        choice((d3, d2, d1)).labelled("workgroup_size")
    }
}

impl Interpolate {
    pub fn parser<'tokens, 'src: 'tokens>(
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Interpolate, RichErr<'src, 'tokens>> + Clone
    {
        let d1 = expression()
            .then_ignore(just(Token::SyntaxToken(",")).or_not())
            .then_ignore(just(Token::SyntaxToken(")")))
            .map(Self::D1);

        let d2 = expression()
            .then_ignore(just(Token::SyntaxToken(",")))
            .then(expression())
            .then_ignore(just(Token::SyntaxToken(",")).or_not())
            .then_ignore(just(Token::SyntaxToken(")")))
            .map(|(x, y)| Self::D2(x, y));

        just(Token::SyntaxToken("(")).ignore_then(choice((d2, d1)))
    }
}
