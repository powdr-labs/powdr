use std::{collections::HashMap, ops::ControlFlow};

use number::FieldElement;
use parser::ast::*;

use crate::pil_analyzer::MacroDefinition;

//TODO this could also go into the parser.

pub fn expand_macros<T>(
    macros: &HashMap<String, MacroDefinition<T>>,
    statement: Statement<T>,
) -> Vec<Statement<T>>
where
    T: FieldElement,
{
    let mut expander = MacroExpander {
        macros,
        arguments: vec![],
        parameter_names: Default::default(),
        statements: vec![],
    };
    expander.handle_statement(statement);
    std::mem::take(&mut expander.statements)
}

struct MacroExpander<'a, T> {
    macros: &'a HashMap<String, MacroDefinition<T>>,
    arguments: Vec<Expression<T>>,
    parameter_names: HashMap<String, usize>,
    statements: Vec<Statement<T>>,
}

impl<'a, T> MacroExpander<'a, T>
where
    T: FieldElement,
{
    pub fn expand_macro(
        &mut self,
        name: &str,
        arguments: Vec<Expression<T>>,
    ) -> Option<Expression<T>> {
        let old_arguments = std::mem::replace(&mut self.arguments, arguments);

        let mac = &self
            .macros
            .get(name)
            .unwrap_or_else(|| panic!("Macro {name} not found."));
        let parameters = mac
            .parameters
            .iter()
            .enumerate()
            .map(|(i, n)| (n.clone(), i))
            .collect();
        let old_parameters = std::mem::replace(&mut self.parameter_names, parameters);

        let expression = mac.expression.clone();
        let identities = mac.identities.clone();
        for identity in identities {
            self.handle_statement(identity)
        }
        let result = expression.map(|expr| self.process_expression(expr));
        self.arguments = old_arguments;
        self.parameter_names = old_parameters;
        result
    }

    pub fn handle_statement(&mut self, statement: Statement<T>) {
        if let Statement::FunctionCall(_start, name, arguments) = statement {
            if !self.macros.contains_key(&name) {
                panic!(
                    "Macro {name} not found - only macros allowed at this point, no fixed columns."
                );
            }
            // TODO check that it does not contain local variable references.
            // But we also need to do some other well-formedness checks.
            if self.expand_macro(&name, arguments).is_some() {
                panic!("Invoked a macro in statement context with non-empty expression.");
            }
            return;
        }
        let statement = match statement {
            Statement::PolynomialIdentity(start, expression) => {
                Statement::PolynomialIdentity(start, self.process_expression(expression))
            }
            Statement::PlookupIdentity(start, key, haystack) => Statement::PlookupIdentity(
                start,
                self.process_selected_expressions(key),
                self.process_selected_expressions(haystack),
            ),
            Statement::PermutationIdentity(start, left, right) => Statement::PermutationIdentity(
                start,
                self.process_selected_expressions(left),
                self.process_selected_expressions(right),
            ),
            Statement::ConnectIdentity(start, left, right) => Statement::ConnectIdentity(
                start,
                self.process_expressions(left),
                self.process_expressions(right),
            ),
            // TODO at some point, these should all be caught by the type checker.
            _ => {
                panic!("Only identities allowed inside macros.")
            }
        };
        self.statements.push(statement);
    }

    fn process_expression(&mut self, mut expression: Expression<T>) -> Expression<T> {
        postvisit_expression_mut(&mut expression, &mut |e| {
            if let Expression::PolynomialReference(poly) = e {
                if poly.namespace.is_none() && self.parameter_names.contains_key(&poly.name) {
                    // TODO to make this work inside macros, "next" and "index" need to be
                    // their own ast nodes / operators.
                    assert!(!poly.next);
                    assert!(poly.index.is_none());
                    *e = self.arguments[self.parameter_names[&poly.name]].clone()
                }
            } else if let Expression::FunctionCall(name, arguments) = e {
                if self.macros.contains_key(name.as_str()) {
                    *e = self
                        .expand_macro(name, std::mem::take(arguments))
                        .expect("Invoked a macro in expression context with empty expression.")
                }
            }

            ControlFlow::<()>::Continue(())
        });
        expression
    }

    fn process_expressions(&mut self, exprs: Vec<Expression<T>>) -> Vec<Expression<T>> {
        exprs
            .into_iter()
            .map(|e| self.process_expression(e))
            .collect()
    }

    fn process_selected_expressions(
        &mut self,
        exprs: SelectedExpressions<T>,
    ) -> SelectedExpressions<T> {
        SelectedExpressions {
            selector: exprs.selector.map(|e| self.process_expression(e)),
            expressions: self.process_expressions(exprs.expressions),
        }
    }
}
