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

#[derive(Debug)]
pub enum ModuleError<'a> {
    Tokenizer(Rich<'a, char>),
    AstParser(Rich<'a, Token<'a>>),
}

#[derive(Debug)]
pub struct TokenizationResult<'a> {
    pub tokens: Vec<(Token<'a>, SimpleSpan)>,
    pub errors: Vec<ModuleError<'a>>,
}

#[derive(Debug)]
pub struct AstResult<'a> {
    pub ast: Vec<Statement>,
    pub errors: Vec<ModuleError<'a>>,
}

pub fn tokenize(source: &str) -> TokenizationResult {
    let (mut tokens, errors) = tokenizer().parse(source).into_output_errors();
    if let Some(tokens) = &mut tokens {
        insert_template_tokens(source, tokens);
    }

    TokenizationResult {
        tokens: tokens.unwrap_or_default(),
        errors: errors.into_iter().map(ModuleError::Tokenizer).collect(),
    }
}

pub fn create_ast<'a>(source: &'a TokenizationResult) -> AstResult<'a> {
    let (ast, errors) = ast_parser()
        .parse(
            source
                .tokens
                .as_slice()
                .spanned((source.tokens.len()..source.tokens.len()).into()),
        )
        .into_output_errors();

    AstResult {
        ast: ast.unwrap_or_default(),
        errors: errors.into_iter().map(ModuleError::AstParser).collect(),
    }
}
