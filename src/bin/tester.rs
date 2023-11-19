use wgsl_ast::front::module::{create_ast, tokenize};

fn main() {
    let source = include_str!("test.wgsl");
    let tokens = tokenize(source);
    let ast = create_ast(&tokens);

    dbg!(&tokens);
    dbg!(&ast);
}
