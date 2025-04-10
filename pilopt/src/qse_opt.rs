use std::{collections::HashMap, fmt::Display};

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Identity, PolynomialIdentity,
};
use powdr_constraint_solver::{
    algebraic_to_quadratic::algebraic_expression_to_quadratic_symbolic_expression,
    effect::Effect,
    quadratic_symbolic_expression::{QuadraticSymbolicExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    symbolic_expression::{BinaryOperator, SymbolicExpression, UnaryOperator},
};
use powdr_number::FieldElement;

pub fn run_qse_optimization<T: FieldElement>(
    pil_file: &mut Analyzed<T>,
    range_constraints: impl RangeConstraintProvider<T, AlgebraicReference>,
) {
    for result in optimize(pil_file, range_constraints) {
        match result {
            Result::Equality(var, value) => {
                pil_file.append_polynomial_identity(
                    AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                        left: Box::new(AlgebraicExpression::Reference(var)),
                        op: AlgebraicBinaryOperator::Sub,
                        right: Box::new(value),
                    }),
                    Default::default(),
                );
            }
        }
    }
}

pub fn optimize<'a, T: FieldElement>(
    pil_file: &Analyzed<T>,
    range_constraints: impl RangeConstraintProvider<T, AlgebraicReference>,
) -> Vec<Result<T>> {
    // TODO extract range constraints from bus interactions
    process_identities(create_identities(pil_file), range_constraints)
}

fn create_identities<T: FieldElement>(
    pil_file: &Analyzed<T>,
) -> Vec<QuadraticSymbolicExpression<T, Variable>> {
    // TODO add identities for intermediate columns
    pil_file
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::Polynomial(PolynomialIdentity { expression, .. }) => Some(
                algebraic_expression_to_quadratic_symbolic_expression(expression, &|r| {
                    QuadraticSymbolicExpression::from_unknown_variable(Variable(r.clone()))
                }),
            ),
            _ => None,
        })
        .collect()
}

fn process_identities<T: FieldElement>(
    identities: Vec<QuadraticSymbolicExpression<T, Variable>>,
    range_constraints: impl RangeConstraintProvider<T, AlgebraicReference>,
) -> Vec<Result<T>> {
    let mut results = vec![];
    let mut range_constraints = RangeConstraints::new(range_constraints);
    let mut complete_identities = vec![false; identities.len()];

    loop {
        let mut progress = false;
        for (i, id) in identities.iter().enumerate() {
            if complete_identities[i] {
                continue;
            }
            match id.solve(&range_constraints) {
                Ok(result) => {
                    for e in result.effects {
                        match e {
                            Effect::Assignment(var, symbolic_expression) => {
                                range_constraints
                                    .add(var.clone(), symbolic_expression.range_constraint());
                                results.push(Result::Equality(
                                    var.0.clone(),
                                    symbolic_expression_to_algebraic(&symbolic_expression),
                                ));
                                progress = true;
                            }
                            Effect::RangeConstraint(var, range_constraint) => {
                                if let Some(value) = range_constraint.try_to_single_value() {
                                    results.push(Result::Equality(
                                        var.0.clone(),
                                        AlgebraicExpression::Number(value),
                                    ));
                                };
                                if range_constraints.add(var, range_constraint) {
                                    progress = true;
                                }
                            }
                            Effect::ConditionalAssignment {
                                variable,
                                condition,
                                in_range_value,
                                out_of_range_value,
                            } => todo!(),
                            Effect::BitDecomposition(_) | Effect::Assertion(_) => {}
                        }
                    }
                    if result.complete {
                        complete_identities[i] = true;
                    }
                }
                Err(_) => todo!(),
            }
        }
        if !progress {
            break;
        }
    }
    results
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
// TODO could also use Thin
struct Variable(AlgebraicReference);

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let AlgebraicReferenceThin {
        //     poly_id: PolyID { id, ptype },
        //     next,
        // } = self.0;
        // let ptype = match ptype {
        //     PolynomialType::Committed => 'w',
        //     PolynomialType::Constant => 'f',
        //     PolynomialType::Intermediate => 'i',
        // };
        // let n = if next { "n" } else { "" };
        // write!(f, "{ptype}_{id}{n}")
        write!(f, "{}", self.0.name)
    }
}

enum Result<T: FieldElement> {
    Equality(AlgebraicReference, AlgebraicExpression<T>),
}

#[derive(Default)]
struct RangeConstraints<T: FieldElement, R: RangeConstraintProvider<T, AlgebraicReference>> {
    range_constraints: R,
    new_range_constraints: HashMap<Variable, RangeConstraint<T>>,
}

impl<T: FieldElement, R: RangeConstraintProvider<T, AlgebraicReference>>
    RangeConstraintProvider<T, Variable> for RangeConstraints<T, R>
{
    fn get(&self, variable: &Variable) -> RangeConstraint<T> {
        self.new_range_constraints
            .get(variable)
            .cloned()
            .unwrap_or_else(|| self.range_constraints.get(&variable.0))
    }
}

impl<T: FieldElement, R: RangeConstraintProvider<T, AlgebraicReference>> RangeConstraints<T, R> {
    fn new(range_constraints: R) -> Self {
        Self {
            range_constraints,
            new_range_constraints: HashMap::new(),
        }
    }

    fn add(&mut self, variable: Variable, range_constraint: RangeConstraint<T>) -> bool {
        let existing = self.get(&variable);
        let new = existing.conjunction(&range_constraint);
        if new != existing {
            self.new_range_constraints.insert(variable.clone(), new);
            true
        } else {
            false
        }
    }
}

fn symbolic_expression_to_algebraic<T: FieldElement>(
    e: &SymbolicExpression<T, Variable>,
) -> AlgebraicExpression<T> {
    // Problem: div does not work!
    match e {
        SymbolicExpression::Concrete(v) => AlgebraicExpression::Number(*v),
        SymbolicExpression::Symbol(var, _) => AlgebraicExpression::Reference(var.0.clone()),
        SymbolicExpression::BinaryOperation(left, op, right, _) => {
            let left = Box::new(symbolic_expression_to_algebraic(left));
            let right = Box::new(symbolic_expression_to_algebraic(right));
            let op = symbolic_op_to_algebraic(*op);
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right })
        }
        SymbolicExpression::UnaryOperation(op, inner, _) => match op {
            UnaryOperator::Neg => {
                let expr = Box::new(symbolic_expression_to_algebraic(inner));
                AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                    expr,
                    op: AlgebraicUnaryOperator::Minus,
                })
            }
        },
    }
}

fn symbolic_op_to_algebraic(op: BinaryOperator) -> AlgebraicBinaryOperator {
    match op {
        BinaryOperator::Add => AlgebraicBinaryOperator::Add,
        BinaryOperator::Sub => AlgebraicBinaryOperator::Sub,
        BinaryOperator::Mul => AlgebraicBinaryOperator::Mul,
        BinaryOperator::Div => todo!(),
    }
}
