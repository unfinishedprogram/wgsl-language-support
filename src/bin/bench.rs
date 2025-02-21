use std::time::Duration;

use wgsl_ast::front;

#[derive(Debug)]
pub struct BenchResult {
    pub tokenization: Duration,
    pub ast_parsing: Duration,
    pub error_count: usize,
    pub name: &'static str,
}

pub fn bench_all(source: &str, name: &'static str, repeat_times: usize) -> BenchResult {
    let source = source.repeat(repeat_times);

    let (token_result, tokenization) = time_call(|| front::ast::tokenize(&source));
    let (ast_result, ast_parsing) = time_call(|| front::ast::create_ast(&token_result));

    let error_count = ast_result.errors.len() + token_result.errors.len();

    BenchResult {
        tokenization,
        ast_parsing,
        error_count,
        name,
    }
}

pub fn time_call<T, F: FnOnce() -> T>(f: F) -> (T, Duration) {
    let start = std::time::Instant::now();
    let result = f();
    let end = std::time::Instant::now();
    (result, end - start)
}

const REPEAT_TIMES: usize = 1;

pub fn main() {
    println!("Starting!");

    println!(
        "{:?}",
        bench_all(
            include_str!("./bench/source_examples/top.wgsl"),
            "Stress",
            REPEAT_TIMES
        )
    );

    println!(
        "{:?}",
        bench_all(include_str!("./test.wgsl"), "Normal Use", REPEAT_TIMES)
    );
}
