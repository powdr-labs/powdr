use std::collections::HashMap;

use crate::{
    constraint_system::ComputationMethod, grouped_expression::GroupedExpression,
    rule_based_optimizer::types::Var,
};

pub struct NewVarGenerator<T> {
    counter: usize,
    requests: Vec<(Var, String)>,
    computation_methods: HashMap<Var, ComputationMethod<T, GroupedExpression<T, Var>>>,
}

impl<T> NewVarGenerator<T> {
    pub fn new(initial_counter: usize) -> Self {
        Self {
            counter: initial_counter,
            requests: Default::default(),
            computation_methods: Default::default(),
        }
    }

    pub fn generate(
        &mut self,
        prefix: &str,
        computation_method: ComputationMethod<T, GroupedExpression<T, Var>>,
    ) -> Var {
        let var = Var::from(self.counter);
        self.requests.push((var, prefix.to_string()));
        self.computation_methods.insert(var, computation_method);
        self.counter += 1;
        var
    }

    pub fn requests(&self) -> &Vec<(Var, String)> {
        &self.requests
    }

    pub fn computation_method(
        &self,
        var: &Var,
    ) -> &ComputationMethod<T, GroupedExpression<T, Var>> {
        self.computation_methods.get(var).unwrap()
    }
}
