use alloc::vec::Vec;

use p3_air::{AirBuilder, AirBuilderWithPublicValues, PairBuilder};
use p3_field::AbstractField;
use p3_matrix::dense::{RowMajorMatrix, RowMajorMatrixView};
use p3_matrix::stack::VerticalPair;

use crate::traits::MultistageAirBuilder;
use p3_uni_stark::{PackedChallenge, PackedVal, StarkGenericConfig, Val};

#[derive(Debug)]
pub struct ProverConstraintFolder<'a, SC: StarkGenericConfig> {
    pub challenges: Vec<Vec<Val<SC>>>,
    pub traces_by_stage: Vec<RowMajorMatrix<PackedVal<SC>>>,
    pub preprocessed: RowMajorMatrix<PackedVal<SC>>,
    pub public_values_by_stage: &'a Vec<Vec<Val<SC>>>,
    pub is_first_row: PackedVal<SC>,
    pub is_last_row: PackedVal<SC>,
    pub is_transition: PackedVal<SC>,
    pub alpha: SC::Challenge,
    pub accumulator: PackedChallenge<SC>,
}

type ViewPair<'a, T> = VerticalPair<RowMajorMatrixView<'a, T>, RowMajorMatrixView<'a, T>>;

#[derive(Debug)]
pub struct VerifierConstraintFolder<'a, SC: StarkGenericConfig> {
    pub challenges: Vec<Vec<Val<SC>>>,
    pub traces_by_stage: Vec<ViewPair<'a, SC::Challenge>>,
    pub preprocessed: ViewPair<'a, SC::Challenge>,
    pub public_values_by_stage: Vec<&'a Vec<Val<SC>>>,
    pub is_first_row: SC::Challenge,
    pub is_last_row: SC::Challenge,
    pub is_transition: SC::Challenge,
    pub alpha: SC::Challenge,
    pub accumulator: SC::Challenge,
}

impl<'a, SC: StarkGenericConfig> AirBuilder for ProverConstraintFolder<'a, SC> {
    type F = Val<SC>;
    type Expr = PackedVal<SC>;
    type Var = PackedVal<SC>;
    type M = RowMajorMatrix<PackedVal<SC>>;

    fn main(&self) -> Self::M {
        self.traces_by_stage[0].clone()
    }

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
            panic!("uni-stark only supports a window size of 2")
        }
    }

    fn assert_zero<I: Into<Self::Expr>>(&mut self, x: I) {
        let x: PackedVal<SC> = x.into();
        self.accumulator *= PackedChallenge::<SC>::from_f(self.alpha);
        self.accumulator += x;
    }
}

impl<'a, SC: StarkGenericConfig> AirBuilderWithPublicValues for ProverConstraintFolder<'a, SC> {
    type PublicVar = Val<SC>;

    fn public_values(&self) -> &[Self::PublicVar] {
        self.stage_public_values(0)
    }
}

impl<'a, SC: StarkGenericConfig> MultistageAirBuilder for ProverConstraintFolder<'a, SC> {
    type Challenge = Val<SC>;

    fn stage_trace(&self, stage: usize) -> <Self as AirBuilder>::M {
        self.traces_by_stage[stage].clone()
    }

    fn stage_challenges(&self, stage: usize) -> &[Self::Challenge] {
        &self.challenges[stage]
    }
    fn stage_public_values(&self, stage: usize) -> &[Self::PublicVar] {
        &self.public_values_by_stage[stage]
    }
}

impl<'a, SC: StarkGenericConfig> PairBuilder for ProverConstraintFolder<'a, SC> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed.clone()
    }
}

impl<'a, SC: StarkGenericConfig> AirBuilder for VerifierConstraintFolder<'a, SC> {
    type F = Val<SC>;
    type Expr = SC::Challenge;
    type Var = SC::Challenge;
    type M = ViewPair<'a, SC::Challenge>;

    fn main(&self) -> Self::M {
        self.traces_by_stage[0]
    }

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
            panic!("uni-stark only supports a window size of 2")
        }
    }

    fn assert_zero<I: Into<Self::Expr>>(&mut self, x: I) {
        let x: SC::Challenge = x.into();
        self.accumulator *= self.alpha;
        self.accumulator += x;
    }
}

impl<'a, SC: StarkGenericConfig> AirBuilderWithPublicValues for VerifierConstraintFolder<'a, SC> {
    type PublicVar = Val<SC>;

    fn public_values(&self) -> &[Self::PublicVar] {
        self.stage_public_values(0)
    }
}

impl<'a, SC: StarkGenericConfig> MultistageAirBuilder for VerifierConstraintFolder<'a, SC> {
    type Challenge = Val<SC>;

    fn stage_trace(&self, stage: usize) -> <Self as AirBuilder>::M {
        self.traces_by_stage[stage]
    }

    fn stage_challenges(&self, stage: usize) -> &[Self::Challenge] {
        &self.challenges[stage]
    }
    fn stage_public_values(&self, stage: usize) -> &[Self::PublicVar] {
        self.public_values_by_stage[stage]
    }
}

impl<'a, SC: StarkGenericConfig> PairBuilder for VerifierConstraintFolder<'a, SC> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed
    }
}
