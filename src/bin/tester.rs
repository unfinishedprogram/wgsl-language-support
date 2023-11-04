use chumsky::Parser;
use wgsl_ast::parse::lexer::lexer;

fn main() {
    let source = include_str!("../../test_data/collatz.wgsl");
    let templated = wgsl_ast::parse::templates::insert_template_delimiters(source);
    let tokens = lexer().parse(&templated);
    dbg!(tokens);
}
