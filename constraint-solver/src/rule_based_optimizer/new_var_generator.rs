use std::collections::HashMap;

use crate::{
    constraint_system::ComputationMethod, grouped_expression::GroupedExpression,
    rule_based_optimizer::types::Var,
};

/// A request for a new variable from the rule system. The variable will be assigned a tentative ID and name
/// generated from the prefix. Both the ID and the name will be re-generated when the replacements are processed.
pub struct NewVarRequest<T> {
    /// The final ID computed when the replacements are processed.
    pub final_id: Option<Var>,
    /// A prefix to be used for generating a descriptive name.
    pub prefix: String,
    /// The way to compute the variable during witness generation.
    pub computation_method: ComputationMethod<T, GroupedExpression<T, Var>>,
}

pub struct NewVarGenerator<T> {
    counter: usize,
    requests: HashMap<Var, NewVarRequest<T>>,
}

impl<T> NewVarGenerator<T> {
    pub fn new(initial_counter: usize) -> Self {
        Self {
            counter: initial_counter,
            requests: Default::default(),
        }
    }

    pub fn generate(
        &mut self,
        prefix: &str,
        computation_method: ComputationMethod<T, GroupedExpression<T, Var>>,
    ) -> Var {
        let var = Var::from(self.counter);
        self.requests.insert(
            var,
            NewVarRequest {
                final_id: None,
                prefix: prefix.to_string(),
                computation_method,
            },
        );
        self.counter += 1;
        var
    }

    pub fn requests(self) -> HashMap<Var, NewVarRequest<T>> {
        self.requests
    }
}
