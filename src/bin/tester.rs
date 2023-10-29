use chumsky::Parser;
use wgsl_ast::parse::lexer;

fn main() {
    let source = include_str!("../../test_data/collatz.wgsl");
    dbg!(lexer::lexer().parse(source));
}
