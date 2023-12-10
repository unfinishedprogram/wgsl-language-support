use ariadne::{ColorGenerator, Label, Report, Source};
use wgsl_ast::front::module::{create_ast, tokenize};
fn main() {
    let source = include_str!("test.wgsl");
    let token_result = tokenize(source);
    let ast_result = create_ast(&token_result);

    dbg!(token_result.tokens.len());
    dbg!(&ast_result.ast);

    let mut colors = ColorGenerator::new();

    for err in ast_result.errors {
        Report::build(ariadne::ReportKind::Error, "test.wgsl", err.span().start)
            .with_label(
                Label::new(("test.wgsl", err.span().into_range()))
                    .with_message(err.message())
                    .with_color(colors.next()),
            )
            .finish()
            .print(("test.wgsl", Source::from(include_str!("test.wgsl"))))
            .unwrap();
    }
}
