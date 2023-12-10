use ariadne::{ColorGenerator, Label, Report, Source};
use wgsl_ast::front::module::{create_ast, tokenize};
fn main() {
    let source = include_str!("test.wgsl");
    let tokens = tokenize(source);
    let ast = create_ast(&tokens);

    dbg!(tokens.tokens.len());
    let mut colors = ColorGenerator::new();
    dbg!(&ast);

    for err in ast.errors {
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
