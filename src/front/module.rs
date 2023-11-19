use chumsky::prelude::*;

use super::{
    ast::{ast_parser, statement::Statement},
    token::{parse::tokenizer, template::insert_template_tokens, Token},
};

pub struct Module<'src> {
    pub source: &'src str,
    pub tokens: Vec<(Token<'src>, SimpleSpan)>,
    pub ast: Vec<Statement>,
}

impl<'src> Module<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut tokens = tokenizer().parse(source).unwrap();
        insert_template_tokens(source, &mut tokens);

        let ast = ast_parser()
            .parse(
                tokens
                    .as_slice()
                    .spanned((source.len()..source.len()).into()),
            )
            .unwrap();

        Self {
            source,
            tokens,
            ast,
        }
    }
}
