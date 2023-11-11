use chumsky::{prelude::Input, Parser};
use wgsl_ast::parse::{ast::expression::singular_expression_parser, tokenizer::tokenizer};

fn main() {
    let source = include_str!("test.wgsl");
    let templated = wgsl_ast::parse::templates::insert_template_delimiters(source);
    let tokens = tokenizer().parse(&templated).unwrap();

    let ast = singular_expression_parser()
        .parse(
            tokens
                .as_slice()
                .spanned((source.len()..source.len()).into()),
        )
        .unwrap();
    dbg!(ast);
    // tokens.unwrap()
    dbg!(tokens);
}
