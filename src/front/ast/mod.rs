pub mod expression;
pub mod statement;

use self::statement::{statement, Statement};
use super::token::{parse::tokenizer, template::insert_template_tokens, Token};
use chumsky::prelude::*;

type RichErr<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, SimpleSpan>>;
type ParserInput<'tokens, 'src> = chumsky::input::SpannedInput<
    Token<'src>,
    SimpleSpan<usize>,
    &'tokens [(Token<'src>, SimpleSpan<usize>)],
>;

pub fn ast_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Vec<(Statement, SimpleSpan)>, RichErr<'tokens, 'src>>
{
    statement()
        .then_ignore(just(Token::SyntaxToken(";")).or_not())
        .repeated()
        .collect()
}

pub struct AbstractSyntax<'src> {
    pub source: &'src str,
    pub tokens: Vec<(Token<'src>, SimpleSpan)>,
    pub ast: Vec<Statement>,
}

#[derive(Debug)]
pub enum ModuleError<'a> {
    Tokenizer(Rich<'a, char>),
    AstParser(Rich<'a, Token<'a>>),
}

impl ModuleError<'_> {
    pub fn span(&self) -> &SimpleSpan {
        match self {
            ModuleError::Tokenizer(err) => err.span(),
            ModuleError::AstParser(err) => err.span(),
        }
    }

    pub fn message(&self) -> String {
        match self {
            ModuleError::Tokenizer(err) => err.to_string(),
            ModuleError::AstParser(err) => format!("{:?}", err),
        }
    }
}

#[derive(Debug)]
pub struct TokenizationResult<'a> {
    pub tokens: Vec<(Token<'a>, SimpleSpan)>,
    pub errors: Vec<ModuleError<'a>>,
}

#[derive(Debug)]
pub struct AstResult<'a> {
    pub ast: Vec<(Statement, SimpleSpan)>,
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
