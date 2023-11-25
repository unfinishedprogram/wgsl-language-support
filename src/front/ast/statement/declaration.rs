use chumsky::prelude::*;

use crate::front::{
    ast::{
        expression::{expression, template_list, Expression, TemplateList},
        ParserInput, RichErr,
    },
    token::{Keyword, Token},
};

use super::{optionally_typed_ident, OptionallyTypedIdent, Statement};

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
}

fn variable_or_value_declaration<'tokens, 'src: 'tokens>(
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

pub fn declaration<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Declaration, RichErr<'src, 'tokens>> + Clone {
    choice((variable_or_value_declaration(),))
}
