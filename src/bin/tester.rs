use wgsl_ast::front::module::Module;

fn main() {
    let source = include_str!("test.wgsl");
    let module = Module::new(source);
    dbg!(module.ast);
}
