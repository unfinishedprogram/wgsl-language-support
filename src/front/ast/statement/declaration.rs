use chumsky::prelude::*;

use crate::front::{
    ast::{
        expression::{
            expression, template_elaborated_ident, template_list, Expression,
            TemplateElaboratedIdent, TemplateList,
        },
        ParserInput, RichErr,
    },
    token::{Keyword, Token},
};

use super::{optionally_typed_ident, OptionallyTypedIdent};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    Variable {
        ident: OptionallyTypedIdent,
        scope: Option<TemplateList>,
        initial_value: Option<Expression>,
    },

    ModuleConstant {
        ident: OptionallyTypedIdent,
        value: Expression,
    },

    LocalConstant {
        ident: OptionallyTypedIdent,
        value: Expression,
    },

    TypeAlias {
        ident: String,
        value: TemplateElaboratedIdent,
    },

    Struct {
        // TODO: Add attributes to members
        ident: String,
        members: Vec<(String, TemplateElaboratedIdent)>,
    },
}

fn variable_or_value_decl<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Declaration, RichErr<'src, 'tokens>> + Clone {
    let variable_decl = just(Token::Keyword(Keyword::Var))
        .ignore_then(template_list(expression()).or_not())
        .then(optionally_typed_ident(expression()))
        .then(
            just(Token::SyntaxToken("="))
                .ignore_then(expression())
                .or_not(),
        )
        .map(|((scope, ident), initial_value)| Declaration::Variable {
            scope,
            ident,
            initial_value,
        });

    let module_const = just(Token::Keyword(Keyword::Const))
        .ignore_then(optionally_typed_ident(expression()))
        .then(just(Token::SyntaxToken("=")).ignore_then(expression()))
        .map(|(ident, value)| Declaration::ModuleConstant { ident, value });

    let local_const = just(Token::Keyword(Keyword::Let))
        .ignore_then(optionally_typed_ident(expression()))
        .then(just(Token::SyntaxToken("=")).ignore_then(expression()))
        .map(|(ident, value)| Declaration::LocalConstant { ident, value });

    choice((variable_decl, module_const, local_const))
}

fn type_alias_decl<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Declaration, RichErr<'src, 'tokens>> + Clone {
    just(Token::Keyword(Keyword::Alias))
        .ignore_then(select!(Token::Ident(ident) => ident.to_owned()))
        .then(just(Token::SyntaxToken("=")).ignore_then(template_elaborated_ident(expression())))
        .map(|(ident, value)| Declaration::TypeAlias { ident, value })
}

fn struct_decl<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Declaration, RichErr<'src, 'tokens>> + Clone {
    let struct_member = select!(Token::Ident(ident) => ident.to_owned())
        .then(just(Token::SyntaxToken(":")).ignore_then(template_elaborated_ident(expression())))
        .map(|(ident, value)| (ident, value));

    let struct_body = struct_member
        .separated_by(just(Token::SyntaxToken(",")))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::SyntaxToken("{")), just(Token::SyntaxToken("}")));

    just(Token::Keyword(Keyword::Struct))
        .ignore_then(select!(Token::Ident(ident) => ident.to_owned()))
        .then(struct_body)
        .map(|(ident, members)| Declaration::Struct { ident, members })
}

pub fn declaration<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Declaration, RichErr<'src, 'tokens>> + Clone {
    choice((variable_or_value_decl(), type_alias_decl(), struct_decl()))
}
