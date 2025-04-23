use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::Itertools;
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
            OptimizationResult::Equality(var, value) => {
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
) -> Vec<OptimizationResult<T>> {
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
) -> Vec<OptimizationResult<T>> {
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
                                results.push(OptimizationResult::Equality(
                                    var.0.clone(),
                                    symbolic_expression_to_algebraic(&symbolic_expression),
                                ));
                                progress = true;
                            }
                            Effect::RangeConstraint(var, range_constraint) => {
                                if let Some(value) = range_constraint.try_to_single_value() {
                                    results.push(OptimizationResult::Equality(
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
                            } => {
                                println!("Conditional assignment: {variable} condition {in_range_value} {out_of_range_value}");
                            }
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

    results.extend(process_quadratic_equalities(&identities, range_constraints));
    results
}

fn process_quadratic_equalities<T: FieldElement>(
    identities: &Vec<QuadraticSymbolicExpression<T, Variable>>,
    range_constraints: impl RangeConstraintProvider<T, Variable>,
) -> Vec<OptimizationResult<T>> {
    let candidates = identities
        .iter()
        .filter_map(QuadraticEqualityCandidate::try_from_qse)
        .collect::<Vec<_>>();
    let mut result = vec![];
    for (i, c1) in candidates.iter().enumerate() {
        for c2 in &candidates[(i + 1)..] {
            if let Some((v1, v2)) =
                process_quadratic_equality_candidate_pair(c1, c2, &range_constraints)
            {
                result.push(OptimizationResult::Equality(
                    v1.0.clone(),
                    AlgebraicExpression::Reference(v2.0),
                ));
            }
        }
    }

    result
}

fn process_quadratic_equality_candidate_pair<T: FieldElement>(
    c1: &QuadraticEqualityCandidate<T>,
    c2: &QuadraticEqualityCandidate<T>,
    range_constraints: &impl RangeConstraintProvider<T, Variable>,
) -> Option<(Variable, Variable)> {
    if c1.offset != c2.offset {
        return None;
    }
    if !(c1.variables.len() == c2.variables.len() && c1.variables.len() >= 2) {
        return None;
    }
    let c1_var = c1.variables.difference(&c2.variables).exactly_one().ok()?;
    let c2_var = c2.variables.difference(&c1.variables).exactly_one().ok()?;
    println!("Candidate vars: {c1_var} {c2_var}");
    let rc1 = range_constraints.get(c1_var);
    let rc2 = range_constraints.get(c2_var);
    if rc1 != rc2 {
        return None;
    }
    println!("offset: {:x} or {:x}", c1.offset, -c1.offset);
    println!("RC width: {:x}", rc1.range_width());
    // TODO correct?
    // TODO do we need to check that range width is at least field size half? Probabyl not due to the negation.
    if c1.offset.to_integer() < rc1.range_width() || (-c1.offset).to_integer() < rc1.range_width() {
        return None;
    }
    // Now we need to show that the rest of the affine expression is the same.

    // TODO does it work with factors tat are not one? What about minus one?
    // let c1_coeff = c1.expr.coefficient_of(c1_var);
    // let c2_coeff = c2.expr.coefficient_of(c2_var);
    // println!("Coefficients: {c1_coeff} {c2_coeff}");
    if c1.expr.clone() - QuadraticSymbolicExpression::from_unknown_variable(c1_var.clone())
        != c2.expr.clone() - QuadraticSymbolicExpression::from_unknown_variable(c2_var.clone())
    {
        return None;
    }

    println!("OOOOOOOOOOOOOOOOOOOOOOKFound quadratic equality candidates: {c1_var} = {c2_var}");
    Some((c1_var.clone(), c2_var.clone()))
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

enum OptimizationResult<T: FieldElement> {
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

/// This represents an identity `expr * (expr + offset) = 0`,
/// where `expr` is an affine expression.
struct QuadraticEqualityCandidate<T: FieldElement> {
    expr: QuadraticSymbolicExpression<T, Variable>,
    offset: T,
    variables: HashSet<Variable>,
}

impl<T: FieldElement> QuadraticEqualityCandidate<T> {
    fn try_from_qse(constr: &QuadraticSymbolicExpression<T, Variable>) -> Option<Self> {
        println!("Trying to convert QSE to quadratic equality candidate: {constr}");
        let (left, right) = constr.try_as_single_product()?;
        if !left.is_affine() || !right.is_affine() {
            return None;
        }
        // `constr = 0` is equivalent to `left * right = 0`
        let offset = (left - right).try_to_known()?.try_to_number()?;
        // `offset + right = left`
        // `constr = 0` is equivalent to `right * (right + offset) = 0`
        let variables = right
            .referenced_unknown_variables()
            .cloned()
            .collect::<HashSet<_>>();
        println!("-> ({right}) * ({right} + {offset})");
        Some(Self {
            expr: right.clone(),
            offset,
            variables,
        })
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
