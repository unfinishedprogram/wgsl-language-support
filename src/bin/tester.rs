use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use wgsl_ast::front::module::{create_ast, tokenize};
fn main() {
    let source = include_str!("test.wgsl");
    let tokens = tokenize(source);
    let ast = create_ast(&tokens);

    // dbg!(&tokens);
    // dbg!(&ast);

    dbg!(tokens.tokens.len());
    // dbg!(ast);
    let mut colors = ColorGenerator::new();

    let b = colors.next();

    for err in ast.errors {
        Report::build(ariadne::ReportKind::Error, "test.wgsl", err.span().start)
            .with_code(3)
            .with_message(err.message())
            .with_label(
                Label::new(("test.wgsl", err.span().into_range()))
                    .with_message("Unexpected token")
                    .with_color(b),
            )
            .finish()
            .print(("test.wgsl", Source::from(include_str!("test.wgsl"))))
            .unwrap();
    }
}
