use lalrpop::Configuration;

extern crate lalrpop;

fn main() {
    Configuration::new()
        .emit_rerun_directives(true)
        .process_current_dir()
        .unwrap()
}
