use criterion::{criterion_group, criterion_main, Criterion};
use wgsl_ast::front;

fn tokenization(c: &mut Criterion) {
    let source = include_str!("./nested.wgsl");
    c.bench_function("nested function calls", |b| {
        b.iter(|| front::ast::tokenize(source))
    });

    let source = include_str!("./large_file.wgsl");
    c.bench_function("large file normal use", |b| {
        b.iter(|| front::ast::tokenize(source))
    });
}

fn ast_construction(c: &mut Criterion) {
    let tokens = front::ast::tokenize(include_str!("./nested.wgsl"));
    c.bench_function("nested function calls", |b| {
        b.iter(|| front::ast::create_ast(&tokens))
    });

    let tokens = front::ast::tokenize(include_str!("./large_file.wgsl"));
    c.bench_function("large file normal use", |b| {
        b.iter(|| front::ast::create_ast(&tokens))
    });
}

criterion_group!(benches, tokenization, ast_construction);
criterion_main!(benches);
