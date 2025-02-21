use ariadne::{ColorGenerator, Label, Report, Source};
use wgsl_ast::front::ast::{create_ast, tokenize};

fn main() {
    let source = include_str!("test.wgsl");
    let token_result = tokenize(source);
    let ast_result = create_ast(&token_result);

    // dbg!(token_result.tokens.len());
    // dbg!(&token_result.tokens);
    // dbg!(&ast_result.ast);

    println!("{:#?}", ast_result);

    let mut colors = ColorGenerator::new();
    for err in ast_result.errors {
        Report::build(ariadne::ReportKind::Error, err.span().into_range())
            .with_label(
                Label::new(err.span().into_range())
                    .with_message(err.message())
                    .with_color(colors.next()),
            )
            .finish()
            .print(Source::from(include_str!("test.wgsl")))
            .unwrap();
    }
}
