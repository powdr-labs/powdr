use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression as Expression,
    AlgebraicReference, AlgebraicUnaryOperation, AlgebraicUnaryOperator, PolynomialIdentity,
    PolynomialType,
};
use powdr_executor::witgen::data_structures::identity::Identity;
use powdr_executor::witgen::range_constraints::RangeConstraint;
use powdr_executor::witgen::{jit::symbolic_expression::SymbolicExpression, FixedData};
use powdr_number::FieldElement;

pub struct StaticAnalyzer<'a, T: FieldElement> {
    pub data: &'a FixedData<'a, T>,
    pub symbolic_identities: Vec<SymbolicExpression<T, String>>,
}

impl<'a, T: FieldElement> StaticAnalyzer<'a, T> {
    pub fn new(data: &'a FixedData<'a, T>) -> Self {
        let s = Self {
            data,
            symbolic_identities: Vec::new(),
        };
        let symbolic_identities = data
            .identities
            .iter()
            .filter(|id| matches!(id, Identity::Polynomial(_)))
            .map(|id| match id {
                Identity::Polynomial(PolynomialIdentity { expression, .. }) => {
                    s.evaluate(expression)
                }
                _ => unreachable!(),
            })
            .collect();
        Self {
            data,
            symbolic_identities,
        }
    }

    pub fn analyze(&self) {
        for expr in &self.symbolic_identities {
            self.analyze_symbolic_expr(expr);
        }
    }

    fn analyze_symbolic_expr(&self, expr: &SymbolicExpression<T, String>) {
        let range_con = expr.range_constraint();
        let mask = range_con.mask();
        println!("mask for expr {expr} is {mask}");
        if mask > &T::modulus() {
            println!("The mask is larger than the modulus");
        }

        expr.children().for_each(|child| {
            self.analyze_symbolic_expr(child);
        });
    }

    pub fn evaluate(&self, expr: &Expression<T>) -> SymbolicExpression<T, String> {
        match expr {
            Expression::Reference(AlgebraicReference {
                name,
                poly_id,
                next: _,
            }) => match poly_id.ptype {
                PolynomialType::Constant => {
                    let con = &self.data.global_range_constraints.fixed_constraints[&poly_id];
                    SymbolicExpression::from_symbol(
                        name.into(),
                        con.clone().unwrap_or(RangeConstraint::unconstrained()),
                    )
                }
                PolynomialType::Committed => {
                    let con = &self.data.global_range_constraints.witness_constraints[&poly_id];
                    SymbolicExpression::from_symbol(
                        name.into(),
                        con.clone().unwrap_or(RangeConstraint::unconstrained()),
                    )
                }
                _ => todo!(),
            },
            Expression::PublicReference(_) | Expression::Challenge(_) => todo!(),
            Expression::Number(n) => (*n).into(),
            Expression::BinaryOperation(op) => self.evaluate_binary_operation(op),
            Expression::UnaryOperation(op) => self.evaluate_unary_operation(op),
        }
    }

    fn evaluate_binary_operation(
        &self,
        op: &AlgebraicBinaryOperation<T>,
    ) -> SymbolicExpression<T, String> {
        let left = self.evaluate(&op.left);
        let right = self.evaluate(&op.right);
        match op.op {
            AlgebraicBinaryOperator::Add => left + right,
            AlgebraicBinaryOperator::Sub => left + (-right),
            AlgebraicBinaryOperator::Mul => left * right,
            AlgebraicBinaryOperator::Pow => {
                todo!("Implement the power operation")
            }
        }
    }

    fn evaluate_unary_operation(
        &self,
        op: &AlgebraicUnaryOperation<T>,
    ) -> SymbolicExpression<T, String> {
        match op.op {
            AlgebraicUnaryOperator::Minus => -self.evaluate(&op.expr),
        }
    }
}
