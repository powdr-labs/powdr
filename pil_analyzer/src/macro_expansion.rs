use std::collections::HashMap;

use number::FieldElement;
use parser::ast::*;

use crate::pil_analyzer::MacroDefinition;

//TODO this could also go into the parser.

fn expand_macro<T>(
    macros: &HashMap<String, MacroDefinition<T>>,
    name: &str,
    arguments: Vec<Expression<T>>,
) -> (Vec<Statement<T>>, Option<Expression<T>>)
where
    T: FieldElement,
{
    let mut expander = MacroExpander {
        macros,
        arguments: vec![],
        statements: vec![],
    };
    let expression = expander.expand_macro(name, arguments);
    (std::mem::take(&mut expander.statements), expression)
}

struct MacroExpander<'a, T> {
    macros: &'a HashMap<String, MacroDefinition<T>>,
    arguments: Vec<Expression<T>>,
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
        let mut old_arguments = arguments;
        std::mem::swap(&mut self.arguments, &mut old_arguments);
        let mac = &self
            .macros
            .get(name)
            .unwrap_or_else(|| panic!("Macro {name} not found."));
        let expression = mac.expression.clone();
        let identities = mac.identities.clone();
        for identity in identities {
            self.handle_statement(identity)
        }
        let result = expression.map(|expr| self.process_expression(expr));
        self.arguments = old_arguments;
        result
    }

    fn handle_statement(&mut self, statement: Statement<T>) {
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

    fn process_expression(&mut self, expression: Expression<T>) -> Expression<T> {
        match expression {
            Expression::Constant(_) | Expression::Number(_) | Expression::String(_) => expression,
            Expression::PolynomialReference(_) => todo!(),
            Expression::PublicReference(_) => todo!(),
            Expression::Tuple(_) => todo!(),
            Expression::BinaryOperation(_, _, _) => todo!(),
            Expression::UnaryOperation(_, _) => todo!(),
            Expression::FunctionCall(_, _) => todo!(),
            Expression::FreeInput(_) => todo!(),
            Expression::MatchExpression(_, _) => todo!(),
        }
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
        todo!();
    }
}
