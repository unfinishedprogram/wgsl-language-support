use chumsky::Parser;
use wgsl_ast::parse::{
    lexer,
    templates::{extract_template_lists, templates},
};

fn main() {
    let source = include_str!("../../test_data/collatz.wgsl");
    // dbg!(extract_template_lists(source, templates(source)));
    dbg!(lexer::lexer(source).parse(source));
}
