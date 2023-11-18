use chumsky::{error::Rich, extra};

use super::Span;

pub mod parse;
pub mod template;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Boolean(bool),
    Int(String),
    Float(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'src> {
    Literal(Literal),
    Keyword(Keyword),
    SyntaxToken(&'src str),
    Ident(&'src str),
    Trivia,
    TemplateArgsStart,
    TemplateArgsEnd,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Alias,
    Break,
    Case,
    Const,
    ConstAssert,
    Continue,
    Continuing,
    Default,
    Diagnostic,
    Discard,
    Else,
    Enable,
    Fn,
    For,
    If,
    Let,
    Loop,
    Override,
    Requires,
    Return,
    Struct,
    Switch,
    Var,
    While,
}

// A rich error type only for the tokenization step
type RichErr<'src> = extra::Err<Rich<'src, char, Span>>;
