use chumsky::{prelude::Input, Parser};
use wgsl_ast::front::ast::expression::{expression, Expression};
use wgsl_ast::front::token::{parse::tokenizer, template::insert_template_delimiters};

fn main() {
    unsafe { backtrace_on_stack_overflow::enable() };

    let source = include_str!("test.wgsl");
    let templated = insert_template_delimiters(source);
    let tokens = tokenizer().parse(&templated).unwrap();
    dbg!(&tokens);

    let ast = expression().parse(
        tokens
            .as_slice()
            .spanned((source.len()..source.len()).into()),
    );
    dbg!(ast);
}
