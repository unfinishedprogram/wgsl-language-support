use chumsky::prelude::*;

use super::{
    ast::{ast_parser, statement::Statement},
    token::{parse::tokenizer, template::insert_template_tokens, Token},
};

pub struct Module<'src> {
    pub source: &'src str,
    pub tokens: Vec<(Token<'src>, SimpleSpan)>,
    pub ast: Vec<Statement>,
    // pub errors: Vec<ModuleError<'src>>,
}

#[derive(Clone)]
pub enum ModuleError<'a> {
    Tokenizer(Rich<'a, char>),
    AstParser(Rich<'a, Token<'a>>),
}

// TODO: Better error handling
impl<'src> Module<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut errors: Vec<ModuleError> = vec![];

        let (mut tokens, tokenizer_errors) = tokenizer().parse(source).into_output_errors();

        if let Some(tokens) = &mut tokens {
            insert_template_tokens(source, tokens);
        }

        let tokens = tokens.unwrap_or_default();

        let (ast, ast_errors) = ast_parser()
            .parse(
                tokens
                    .as_slice()
                    .spanned((source.len()..source.len()).into()),
            )
            .into_output_errors();

        let ast = ast.unwrap_or_default();

        Self {
            tokens,
            source,
            ast,
        }
    }
}
