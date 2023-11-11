use chumsky::{prelude::Input, Parser};
use wgsl_ast::parse::{ast::expression::expression, tokenizer::tokenizer};

fn main() {
    // unsafe { backtrace_on_stack_overflow::enable() };

    let source = include_str!("test.wgsl");
    let templated = wgsl_ast::parse::templates::insert_template_delimiters(source);
    let tokens = tokenizer().parse(&templated).unwrap();
    dbg!(&tokens);

    let ast = expression()
        .parse(
            tokens
                .as_slice()
                .spanned((source.len()..source.len()).into()),
        )
        .unwrap();
    dbg!(ast);
}
