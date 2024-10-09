use alloc::vec;
use alloc::vec::Vec;

use p3_air::{AirBuilder, AirBuilderWithPublicValues, PairBuilder};
use p3_field::Field;
use p3_matrix::dense::RowMajorMatrix;
use p3_util::log2_ceil_usize;
use tracing::instrument;

use crate::traits::{MultiStageAir, MultistageAirBuilder};
use p3_uni_stark::Entry;
use p3_uni_stark::SymbolicExpression;
use p3_uni_stark::SymbolicVariable;

#[instrument(name = "infer log of constraint degree", skip_all)]
pub fn get_log_quotient_degree<F, A>(air: &A, public_values_counts: &[usize]) -> usize
where
    F: Field,
    A: MultiStageAir<SymbolicAirBuilder<F>>,
{
    // We pad to at least degree 2, since a quotient argument doesn't make sense with smaller degrees.
    let constraint_degree = get_max_constraint_degree(air, public_values_counts).max(2);

    // The quotient's actual degree is approximately (max_constraint_degree - 1) n,
    // where subtracting 1 comes from division by the zerofier.
    // But we pad it to a power of two so that we can efficiently decompose the quotient.
    log2_ceil_usize(constraint_degree - 1)
}

#[instrument(name = "infer constraint degree", skip_all, level = "debug")]
pub fn get_max_constraint_degree<F, A>(air: &A, public_values_counts: &[usize]) -> usize
where
    F: Field,
    A: MultiStageAir<SymbolicAirBuilder<F>>,
{
    get_symbolic_constraints(air, public_values_counts)
        .iter()
        .map(|c| c.degree_multiple())
        .max()
        .unwrap_or(0)
}

#[instrument(name = "evaluate constraints symbolically", skip_all, level = "debug")]
pub fn get_symbolic_constraints<F, A>(
    air: &A,
    public_values_counts: &[usize],
) -> Vec<SymbolicExpression<F>>
where
    F: Field,
    A: MultiStageAir<SymbolicAirBuilder<F>>,
{
    let widths: Vec<_> = (0..air.stage_count())
        .map(|i| air.stage_trace_width(i as u32))
        .collect();
    let challenges: Vec<_> = (0..air.stage_count())
        .map(|i| air.stage_challenge_count(i as u32))
        .collect();
    let mut builder = SymbolicAirBuilder::new(
        air.preprocessed_width(),
        &widths,
        public_values_counts,
        challenges,
    );
    air.eval(&mut builder);
    builder.constraints()
}

/// An `AirBuilder` for evaluating constraints symbolically, and recording them for later use.
#[derive(Debug)]
pub struct SymbolicAirBuilder<F: Field> {
    challenges: Vec<Vec<SymbolicVariable<F>>>,
    preprocessed: RowMajorMatrix<SymbolicVariable<F>>,
    traces_by_stage: Vec<RowMajorMatrix<SymbolicVariable<F>>>,
    public_values_by_stage: Vec<Vec<SymbolicVariable<F>>>,
    constraints: Vec<SymbolicExpression<F>>,
}

impl<F: Field> SymbolicAirBuilder<F> {
    pub(crate) fn new(
        preprocessed_width: usize,
        stage_widths: &[usize],
        public_value_counts: &[usize],
        challenges: Vec<usize>,
    ) -> Self {
        let prep_values = [0, 1]
            .into_iter()
            .flat_map(|offset| {
                (0..preprocessed_width)
                    .map(move |index| SymbolicVariable::new(Entry::Preprocessed { offset }, index))
            })
            .collect();
        let traces_by_stage = stage_widths
            .iter()
            .map(|width| {
                let values = [0, 1]
                    .into_iter()
                    .flat_map(|offset| {
                        (0..*width)
                            .map(move |index| SymbolicVariable::new(Entry::Main { offset }, index))
                    })
                    .collect();
                RowMajorMatrix::new(values, *width)
            })
            .collect();
        let mut challenge_index = 0;
        let challenges = challenges
            .iter()
            .map(|count| {
                (0..*count)
                    .map(|_| {
                        let res = SymbolicVariable::new(Entry::Challenge, challenge_index);
                        challenge_index += 1;
                        res
                    })
                    .collect()
            })
            .collect();
        let mut public_value_index = 0;
        let public_values_by_stage = public_value_counts
            .iter()
            .map(|count| {
                (0..*count)
                    .map(|_| {
                        let res = SymbolicVariable::new(Entry::Public, public_value_index);
                        public_value_index += 1;
                        res
                    })
                    .collect()
            })
            .collect();
        Self {
            challenges,
            preprocessed: RowMajorMatrix::new(prep_values, preprocessed_width),
            traces_by_stage,
            public_values_by_stage,
            constraints: vec![],
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
        self.traces_by_stage[0].clone()
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
        self.stage_public_values(0)
    }
}

impl<F: Field> MultistageAirBuilder for SymbolicAirBuilder<F> {
    type Challenge = Self::Var;

    fn stage_public_values(&self, stage: usize) -> &[Self::PublicVar] {
        &self.public_values_by_stage[stage]
    }

    fn stage_trace(&self, stage: usize) -> Self::M {
        self.traces_by_stage[stage].clone()
    }

    fn stage_challenges(&self, stage: usize) -> &[Self::Challenge] {
        &self.challenges[stage]
    }
}

impl<F: Field> PairBuilder for SymbolicAirBuilder<F> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed.clone()
    }
}
