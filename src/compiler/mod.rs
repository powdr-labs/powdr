use std::collections::HashSet;
use std::fs;

use crate::parser::ast::*;
use crate::parser::{self, ast::PILFile};

pub fn compile(path: &str) {
    let input = fs::read_to_string(path).unwrap();
    match parser::parse(&input) {
        Ok(pil_file) => {
            let mut ctx = Context::default();
            ctx.process(&pil_file);
        }
        Err(err) => println!("Parse error: {err}"),
    }
}

#[derive(Default)]
struct Context {
    _namespace: String,
    _included_files: HashSet<String>,
}

impl Context {
    pub fn process(&mut self, file: &PILFile) {
        for statement in &file.0 {
            match statement {
                Statement::Include(include) => self.handle_include(include),
                Statement::Namespace(name, degree) => self.handle_namespace(name, degree),
                Statement::PolynomialDefinition(_, _) => todo!(),
                Statement::PolynomialConstantDeclaration(_) => todo!(),
                Statement::PolynomialCommitDeclaration(_) => todo!(),
                Statement::PolynomialIdentity(_) => todo!(),
                Statement::PlookupIdentity(_, _) => todo!(),
                Statement::ConstantDefinition(_, _) => todo!(),
            }
        }
    }

    fn handle_include(&mut self, _path: &str) {
        // TODO
    }

    fn handle_namespace(&mut self, _name: &str, _degree: &Expression) {}
}
