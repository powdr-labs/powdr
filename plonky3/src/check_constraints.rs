use alloc::vec::Vec;

use itertools::Itertools;
use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues, PairBuilder};
use p3_field::Field;
use p3_matrix::dense::{RowMajorMatrix, RowMajorMatrixView};
use p3_matrix::stack::VerticalPair;
use p3_matrix::Matrix;
use tracing::instrument;

use crate::traits::MultistageAirBuilder;

#[instrument(name = "check constraints", skip_all)]
pub(crate) fn check_constraints<F, A>(
    air: &A,
    preprocessed: &RowMajorMatrix<F>,
    traces_by_stage: Vec<&RowMajorMatrix<F>>,
    public_values_by_stage: &Vec<&Vec<F>>,
    challenges: Vec<&Vec<F>>,
) where
    F: Field,
    A: for<'a> Air<DebugConstraintBuilder<'a, F>>,
{
    let num_stages = traces_by_stage.len();
    let height = traces_by_stage[0].height();

    (0..height).for_each(|i| {
        let i_next = (i + 1) % height;

        let local_preprocessed = preprocessed.row_slice(i);
        let next_preprocessed = preprocessed.row_slice(i_next);
        let preprocessed = VerticalPair::new(
            RowMajorMatrixView::new_row(&*local_preprocessed),
            RowMajorMatrixView::new_row(&*next_preprocessed),
        );

        let stages_local_next = traces_by_stage
            .iter()
            .map(|trace| {
                let stage_local = trace.row_slice(i);
                let stage_next = trace.row_slice(i_next);
                (stage_local, stage_next)
            })
            .collect_vec();

        let traces_by_stage = (0..num_stages)
            .map(|stage| {
                VerticalPair::new(
                    RowMajorMatrixView::new_row(&*stages_local_next[stage].0),
                    RowMajorMatrixView::new_row(&*stages_local_next[stage].1),
                )
            })
            .collect();

        let mut builder = DebugConstraintBuilder {
            row_index: i,
            challenges: challenges.clone(),
            preprocessed,
            traces_by_stage,
            public_values_by_stage,
            is_first_row: F::from_bool(i == 0),
            is_last_row: F::from_bool(i == height - 1),
            is_transition: F::from_bool(i != height - 1),
        };

        air.eval(&mut builder);
    });
}

/// An `AirBuilder` which asserts that each constraint is zero, allowing any failed constraints to
/// be detected early.
#[derive(Debug)]
pub struct DebugConstraintBuilder<'a, F: Field> {
    row_index: usize,
    preprocessed: VerticalPair<RowMajorMatrixView<'a, F>, RowMajorMatrixView<'a, F>>,
    challenges: Vec<&'a Vec<F>>,
    traces_by_stage: Vec<VerticalPair<RowMajorMatrixView<'a, F>, RowMajorMatrixView<'a, F>>>,
    public_values_by_stage: &'a [&'a Vec<F>],
    is_first_row: F,
    is_last_row: F,
    is_transition: F,
}

impl<'a, F> AirBuilder for DebugConstraintBuilder<'a, F>
where
    F: Field,
{
    type F = F;
    type Expr = F;
    type Var = F;
    type M = VerticalPair<RowMajorMatrixView<'a, F>, RowMajorMatrixView<'a, F>>;

    fn is_first_row(&self) -> Self::Expr {
        self.is_first_row
    }

    fn is_last_row(&self) -> Self::Expr {
        self.is_last_row
    }

    fn is_transition_window(&self, size: usize) -> Self::Expr {
        if size == 2 {
            self.is_transition
        } else {
            panic!("only supports a window size of 2")
        }
    }

    fn main(&self) -> Self::M {
        self.traces_by_stage[0]
    }

    fn assert_zero<I: Into<Self::Expr>>(&mut self, x: I) {
        assert_eq!(
            x.into(),
            F::zero(),
            "constraints had nonzero value on row {}",
            self.row_index
        );
    }

    fn assert_eq<I1: Into<Self::Expr>, I2: Into<Self::Expr>>(&mut self, x: I1, y: I2) {
        let x = x.into();
        let y = y.into();
        assert_eq!(
            x, y,
            "values didn't match on row {}: {} != {}",
            self.row_index, x, y
        );
    }
}

impl<'a, F: Field> AirBuilderWithPublicValues for DebugConstraintBuilder<'a, F> {
    type PublicVar = Self::F;

    fn public_values(&self) -> &[Self::PublicVar] {
        self.stage_public_values(0)
    }
}

impl<'a, F: Field> PairBuilder for DebugConstraintBuilder<'a, F> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed
    }
}

impl<'a, F: Field> MultistageAirBuilder for DebugConstraintBuilder<'a, F> {
    type Challenge = Self::Expr;

    fn stage_public_values(&self, stage: usize) -> &[Self::F] {
        self.public_values_by_stage[stage]
    }

    fn stage_trace(&self, stage: usize) -> Self::M {
        self.traces_by_stage[stage]
    }

    fn stage_challenges(&self, stage: usize) -> &[Self::Expr] {
        self.challenges[stage]
    }
}
