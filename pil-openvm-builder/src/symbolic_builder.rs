use crate::symbolic_variable::Entry;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::interaction::Interaction;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::interaction::InteractionBuilder;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::Air;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::AirBuilder;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::AirBuilderWithPublicValues;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::PairBuilder;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_field::Field;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_matrix::dense::RowMajorMatrix;
use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_util::log2_ceil_usize;
use tracing::instrument;

// Mostly copied from Plonky3!

use crate::symbolic_expression::SymbolicExpression;
use crate::symbolic_variable::SymbolicVariable;

#[instrument(name = "infer log of constraint degree", skip_all)]
pub fn get_log_quotient_degree<F, A>(
    air: &A,
    preprocessed_width: usize,
    num_public_values: usize,
) -> usize
where
    F: Field,
    A: Air<SymbolicAirBuilder<F>>,
{
    // We pad to at least degree 2, since a quotient argument doesn't make sense with smaller degrees.
    let constraint_degree =
        get_max_constraint_degree(air, preprocessed_width, num_public_values).max(2);

    // The quotient's actual degree is approximately (max_constraint_degree - 1) n,
    // where subtracting 1 comes from division by the zerofier.
    // But we pad it to a power of two so that we can efficiently decompose the quotient.
    log2_ceil_usize(constraint_degree - 1)
}

#[instrument(name = "infer constraint degree", skip_all, level = "debug")]
pub fn get_max_constraint_degree<F, A>(
    air: &A,
    preprocessed_width: usize,
    num_public_values: usize,
) -> usize
where
    F: Field,
    A: Air<SymbolicAirBuilder<F>>,
{
    get_symbolic_constraints(air, preprocessed_width, num_public_values)
        .iter()
        .map(|c| c.degree_multiple())
        .max()
        .unwrap_or(0)
}

#[instrument(name = "evaluate constraints symbolically", skip_all, level = "debug")]
pub fn get_symbolic_constraints<F, A>(
    air: &A,
    preprocessed_width: usize,
    num_public_values: usize,
) -> Vec<SymbolicExpression<F>>
where
    F: Field,
    A: Air<SymbolicAirBuilder<F>>,
{
    let mut builder = SymbolicAirBuilder::new(preprocessed_width, air.width(), num_public_values);
    air.eval(&mut builder);
    builder.constraints()
}

/// An `AirBuilder` for evaluating constraints symbolically, and recording them for later use.
#[derive(Debug)]
pub struct SymbolicAirBuilder<F: Field> {
    preprocessed: RowMajorMatrix<SymbolicVariable<F>>,
    main: RowMajorMatrix<SymbolicVariable<F>>,
    public_values: Vec<SymbolicVariable<F>>,
    pub constraints: Vec<SymbolicExpression<F>>,
    pub bus_interactions: Vec<Interaction<SymbolicExpression<F>>>,
}

impl<F: Field> SymbolicAirBuilder<F> {
    pub fn new(preprocessed_width: usize, width: usize, num_public_values: usize) -> Self {
        let prep_values = [0, 1]
            .into_iter()
            .flat_map(|offset| {
                (0..preprocessed_width)
                    .map(move |index| SymbolicVariable::new(Entry::Main { offset }, index))
            })
            .collect();
        let main_values = [0]
            .into_iter()
            .flat_map(|offset| {
                (0..width).map(move |index| SymbolicVariable::new(Entry::Main { offset }, index))
            })
            .collect();
        let public_values = (0..num_public_values)
            .map(move |index| SymbolicVariable::new(Entry::Public, index))
            .collect();
        Self {
            preprocessed: RowMajorMatrix::new(prep_values, preprocessed_width),
            main: RowMajorMatrix::new(main_values, width),
            public_values,
            constraints: vec![],
            bus_interactions: vec![],
        }
    }

    pub(crate) fn constraints(self) -> Vec<SymbolicExpression<F>> {
        self.constraints
    }
}

impl<F: Field> AirBuilder for SymbolicAirBuilder<F> {
    type F = F;
    type Expr = SymbolicExpression<F>;
    type Var = SymbolicVariable<F>;
    type M = RowMajorMatrix<Self::Var>;

    fn main(&self) -> Self::M {
        self.main.clone()
    }

    fn is_first_row(&self) -> Self::Expr {
        SymbolicExpression::IsFirstRow
    }

    fn is_last_row(&self) -> Self::Expr {
        SymbolicExpression::IsLastRow
    }

    fn is_transition_window(&self, size: usize) -> Self::Expr {
        if size == 2 {
            SymbolicExpression::IsTransition
        } else {
            panic!("uni-stark only supports a window size of 2")
        }
    }

    fn assert_zero<I: Into<Self::Expr>>(&mut self, x: I) {
        self.constraints.push(x.into());
    }
}

impl<F: Field> AirBuilderWithPublicValues for SymbolicAirBuilder<F> {
    type PublicVar = SymbolicVariable<F>;
    fn public_values(&self) -> &[Self::PublicVar] {
        &self.public_values
    }
}

impl<F: Field> PairBuilder for SymbolicAirBuilder<F> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed.clone()
    }
}

impl<F: Field> InteractionBuilder for SymbolicAirBuilder<F> {
    fn push_interaction<E: Into<Self::Expr>>(
        &mut self,
        bus_index: usize,
        fields: impl IntoIterator<Item = E>,
        count: impl Into<Self::Expr>,
        interaction_type: openvm_circuit::openvm_stark_sdk::openvm_stark_backend::interaction::InteractionType,
    ) {
        self.bus_interactions.push(Interaction {
            fields: fields.into_iter().map(|e| e.into()).collect(),
            count: count.into(),
            bus_index,
            interaction_type,
        });
    }

    fn num_interactions(&self) -> usize {
        self.bus_interactions.len()
    }

    fn all_interactions(
        &self,
    ) -> &[openvm_circuit::openvm_stark_sdk::openvm_stark_backend::interaction::Interaction<
        Self::Expr,
    >] {
        &self.bus_interactions
    }
}

// fn optimize_constraints<F: Field>(
//     constraints: Vec<SymbolicExpression<F>>,
// ) -> Vec<SymbolicExpression<F>> {
//     constraints
//         .into_iter()
//         .map(|c| optimize_expression(&c))
//         .filter(|c| !is_trivial_constraint(c))
//         .collect()
// }

// fn optimize_expression<F: Field>(expr: &SymbolicExpression<F>) -> SymbolicExpression<F> {
//     match expr {
//         SymbolicExpression::Mul {
//             x,
//             y,
//             degree_multiple,
//         } => {
//             let opt_x = optimize_expression(x);
//             let opt_y = optimize_expression(y);

//             match (&opt_x, &opt_y) {
//                 // Basic constant folding
//                 (SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
//                     SymbolicExpression::Constant(c1.mul(*c2))
//                 }
//                 (SymbolicExpression::Constant(c), expr)
//                 | (expr, SymbolicExpression::Constant(c)) => {
//                     if c.is_zero() {
//                         SymbolicExpression::Constant(F::ZERO)
//                     } else if c.is_one() {
//                         expr.clone()
//                     } else {
//                         // Try to extract nested constant from multiplication
//                         match expr {
//                             SymbolicExpression::Mul { x, y, .. } => {
//                                 if let SymbolicExpression::Constant(c2) = x.as_ref() {
//                                     // c * (c2 * expr) -> (c * c2) * expr
//                                     SymbolicExpression::Mul {
//                                         x: Arc::new(SymbolicExpression::Constant(c.mul(*c2))),
//                                         y: y.clone(),
//                                         degree_multiple: *degree_multiple,
//                                     }
//                                 } else if let SymbolicExpression::Constant(c2) = y.as_ref() {
//                                     // c * (expr * c2) -> (c * c2) * expr
//                                     SymbolicExpression::Mul {
//                                         x: Arc::new(SymbolicExpression::Constant(c.mul(*c2))),
//                                         y: x.clone(),
//                                         degree_multiple: *degree_multiple,
//                                     }
//                                 } else {
//                                     // No nested constant to combine with
//                                     SymbolicExpression::Mul {
//                                         x: Arc::new(opt_x),
//                                         y: Arc::new(opt_y),
//                                         degree_multiple: *degree_multiple,
//                                     }
//                                 }
//                             }
//                             _ => SymbolicExpression::Mul {
//                                 x: Arc::new(opt_x),
//                                 y: Arc::new(opt_y),
//                                 degree_multiple: *degree_multiple,
//                             },
//                         }
//                     }
//                 }
//                 // Double negation elimination
//                 (
//                     SymbolicExpression::Neg { x: neg_x, .. },
//                     SymbolicExpression::Neg { x: neg_y, .. },
//                 ) => optimize_expression(&SymbolicExpression::Mul {
//                     x: neg_x.clone(),
//                     y: neg_y.clone(),
//                     degree_multiple: *degree_multiple,
//                 }),
//                 _ => SymbolicExpression::Mul {
//                     x: Arc::new(opt_x),
//                     y: Arc::new(opt_y),
//                     degree_multiple: *degree_multiple,
//                 },
//             }
//         }
//         SymbolicExpression::Add {
//             x,
//             y,
//             degree_multiple,
//         } => {
//             let opt_x = optimize_expression(x);
//             let opt_y = optimize_expression(y);

//             match (&opt_x, &opt_y) {
//                 // Basic constant folding
//                 (SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
//                     SymbolicExpression::Constant(c1.add(*c2))
//                 }
//                 // Same variable addition -> multiplication by 2
//                 (expr1, expr2) if expr1 == expr2 => SymbolicExpression::Mul {
//                     x: Arc::new(SymbolicExpression::Constant(F::TWO)),
//                     y: Arc::new(expr1.clone()),
//                     degree_multiple: *degree_multiple,
//                 },
//                 // Handle nested additions with constants
//                 (
//                     SymbolicExpression::Add { x: x1, y: y1, .. },
//                     SymbolicExpression::Constant(c2),
//                 ) => {
//                     if let SymbolicExpression::Constant(c1) = y1.as_ref() {
//                         // Combine constants: (a + c1) + c2 -> a + (c1 + c2)
//                         SymbolicExpression::Add {
//                             x: x1.clone(),
//                             y: Arc::new(SymbolicExpression::Constant(c1.add(*c2))),
//                             degree_multiple: *degree_multiple,
//                         }
//                     } else if let SymbolicExpression::Constant(c1) = x1.as_ref() {
//                         // Combine constants when x1 is constant
//                         SymbolicExpression::Add {
//                             x: y1.clone(),
//                             y: Arc::new(SymbolicExpression::Constant(c1.add(*c2))),
//                             degree_multiple: *degree_multiple,
//                         }
//                     } else {
//                         // Default case
//                         SymbolicExpression::Add {
//                             x: Arc::new(opt_x),
//                             y: Arc::new(opt_y),
//                             degree_multiple: *degree_multiple,
//                         }
//                     }
//                 }
//                 // Handle constant + expression
//                 (SymbolicExpression::Constant(c), expr) => {
//                     if c.is_zero() {
//                         expr.clone()
//                     } else {
//                         // Move constant to the right
//                         SymbolicExpression::Add {
//                             x: Arc::new(expr.clone()),
//                             y: Arc::new(SymbolicExpression::Constant(*c)),
//                             degree_multiple: *degree_multiple,
//                         }
//                     }
//                 }
//                 // Handle expression + constant
//                 (expr, SymbolicExpression::Constant(c)) => {
//                     if c.is_zero() {
//                         expr.clone()
//                     } else {
//                         SymbolicExpression::Add {
//                             x: Arc::new(opt_x),
//                             y: Arc::new(opt_y),
//                             degree_multiple: *degree_multiple,
//                         }
//                     }
//                 }
//                 // Rest of existing matches remain the same
//                 _ => SymbolicExpression::Add {
//                     x: Arc::new(opt_x),
//                     y: Arc::new(opt_y),
//                     degree_multiple: *degree_multiple,
//                 },
//             }
//         }
//         SymbolicExpression::Sub {
//             x,
//             y,
//             degree_multiple,
//         } => {
//             let opt_x = optimize_expression(x);
//             let opt_y = optimize_expression(y);

//             match (&opt_x, &opt_y) {
//                 //(SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
//                 //    SymbolicExpression::Constant(c1.sub(*c2))
//                 //}
//                 (expr, SymbolicExpression::Constant(c)) => {
//                     if c.is_zero() {
//                         expr.clone()
//                     } else {
//                         SymbolicExpression::Sub {
//                             x: Arc::new(opt_x),
//                             y: Arc::new(opt_y),
//                             degree_multiple: *degree_multiple,
//                         }
//                     }
//                 }
//                 (e1, e2) if e1 == e2 => SymbolicExpression::Constant(F::ZERO),
//                 _ => SymbolicExpression::Sub {
//                     x: Arc::new(opt_x),
//                     y: Arc::new(opt_y),
//                     degree_multiple: *degree_multiple,
//                 },
//             }
//         }
//         SymbolicExpression::Neg { x, degree_multiple } => {
//             let opt_x = optimize_expression(x);
//             match &opt_x {
//                 SymbolicExpression::Constant(c) => SymbolicExpression::Constant(c.neg()),
//                 SymbolicExpression::Neg { x, .. } => (*x).as_ref().clone(),
//                 _ => SymbolicExpression::Neg {
//                     x: Arc::new(opt_x),
//                     degree_multiple: *degree_multiple,
//                 },
//             }
//         }
//         // Base cases that don't need optimization
//         SymbolicExpression::Variable(_)
//         | SymbolicExpression::IsFirstRow
//         | SymbolicExpression::IsLastRow
//         | SymbolicExpression::IsTransition
//         | SymbolicExpression::Constant(_) => expr.clone(),
//     }
// }
// fn is_trivial_constraint<F: Field>(expr: &SymbolicExpression<F>) -> bool {
//     match expr {
//         SymbolicExpression::Constant(c) => c.is_zero(),
//         SymbolicExpression::Mul { x, y, .. } => {
//             matches!(x.as_ref(), SymbolicExpression::Constant(c) if c.is_zero())
//                 || matches!(y.as_ref(), SymbolicExpression::Constant(c) if c.is_zero())
//         }
//         SymbolicExpression::Add { x, y, .. } => {
//             is_trivial_constraint(x) && is_trivial_constraint(y)
//         }
//         SymbolicExpression::Sub { x, y, .. } => {
//             x.as_ref() == y.as_ref() || (is_trivial_constraint(x) && is_trivial_constraint(y))
//         }
//         SymbolicExpression::Neg { x, .. } => is_trivial_constraint(x),
//         _ => false,
//     }
// }
