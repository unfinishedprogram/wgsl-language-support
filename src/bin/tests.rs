use std::fs;

use wgsl_ast::front::ast::{create_ast, tokenize};
fn main() {
    let paths: Vec<(String, String)> = fs::read_dir("src/test_files")
        .unwrap()
        .flat_map(|f| f.ok())
        .flat_map(|entry| {
            if entry.file_type().unwrap().is_file() {
                Some(entry.path())
            } else {
                None
            }
        })
        .map(|path| {
            (
                path.to_str().unwrap().to_owned(),
                fs::read_to_string(path).unwrap(),
            )
        })
        .collect();

    for _ in 0..1000 {
        for (_path, source) in &paths {
            let token_result = tokenize(source);
            let _ast_result = create_ast(&token_result);
        }
    }
}
