use std::collections::HashMap;

use crate::{
    constraint_system::ComputationMethod, grouped_expression::GroupedExpression,
    rule_based_optimizer::types::Var,
};

pub struct NewVarRequest<T> {
    pub final_id: Option<Var>,
    pub prefix: String,
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
