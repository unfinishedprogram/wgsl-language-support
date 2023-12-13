use std::time::Duration;

use crate::front::{self};

use super::utils::time_call;

#[derive(Debug)]
pub struct BenchResult {
    pub tokenization: Duration,
    pub ast_parsing: Duration,
    pub error_count: usize,
    pub name: &'static str,
}

// Benchmarks tokenization speed
pub fn bench_tokenize(source: &str) -> (Duration, usize) {
    let start = std::time::Instant::now();
    let token_result = front::module::tokenize(source);
    let end = std::time::Instant::now();
    (end - start, token_result.tokens.len())
}

pub fn bench_once(source: &str) -> (Duration, usize) {
    let start = std::time::Instant::now();
    let token_result = front::module::tokenize(source);
    let end = std::time::Instant::now();
    (end - start, token_result.tokens.len())
}

pub fn bench_all(source: &str, name: &'static str, repeat_times: usize) -> BenchResult {
    let source = source.repeat(repeat_times);

    let (token_result, tokenization) = time_call(|| front::module::tokenize(&source));
    let (ast_result, ast_parsing) = time_call(|| front::module::create_ast(&token_result));

    let error_count = ast_result.errors.len() + token_result.errors.len();

    BenchResult {
        tokenization,
        ast_parsing,
        error_count,
        name,
    }
}
