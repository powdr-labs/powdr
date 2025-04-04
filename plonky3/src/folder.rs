use alloc::vec::Vec;

use p3_air::{AirBuilder, PairBuilder};
use p3_field::AbstractField;
use p3_matrix::dense::RowMajorMatrixView;
use p3_matrix::stack::VerticalPair;

use crate::traits::MultistageAirBuilder;
use p3_uni_stark::{PackedChallenge, PackedVal, StarkGenericConfig, Val};

#[derive(Debug)]
pub struct ProverConstraintFolder<'a, SC: StarkGenericConfig> {
    pub challenges: &'a [Vec<Val<SC>>],
    pub traces_by_stage: Vec<RowMajorMatrixView<'a, PackedVal<SC>>>,
    pub preprocessed: RowMajorMatrixView<'a, PackedVal<SC>>,
    pub public_values_by_stage: &'a [Vec<Val<SC>>],
    pub is_first_row: PackedVal<SC>,
    pub is_last_row: PackedVal<SC>,
    pub is_transition: PackedVal<SC>,
    pub alpha: SC::Challenge,
    pub accumulator: PackedChallenge<SC>,
}

type ViewPair<'a, T> = VerticalPair<RowMajorMatrixView<'a, T>, RowMajorMatrixView<'a, T>>;

#[derive(Debug)]
pub struct VerifierConstraintFolder<'a, SC: StarkGenericConfig> {
    pub challenges: &'a [Vec<Val<SC>>],
    pub traces_by_stage: Vec<ViewPair<'a, SC::Challenge>>,
    pub preprocessed: ViewPair<'a, SC::Challenge>,
    pub public_values_by_stage: &'a [Vec<Val<SC>>],
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
    type M = RowMajorMatrixView<'a, PackedVal<SC>>;

    fn main(&self) -> Self::M {
        unimplemented!("use MultiStageAirBuilder instead")
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

impl<SC: StarkGenericConfig> MultistageAirBuilder for ProverConstraintFolder<'_, SC> {
    type Challenge = Val<SC>;
    type PublicVar = Val<SC>;

    fn stage_trace(&self, stage: u8) -> <Self as AirBuilder>::M {
        self.traces_by_stage[stage as usize]
    }

    fn stage_challenges(&self, stage: u8) -> &[Self::Challenge] {
        &self.challenges[stage as usize]
    }
    fn stage_public_values(&self, stage: u8) -> &[Self::PublicVar] {
        &self.public_values_by_stage[stage as usize]
    }
}

impl<SC: StarkGenericConfig> PairBuilder for ProverConstraintFolder<'_, SC> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed
    }
}

impl<'a, SC: StarkGenericConfig> AirBuilder for VerifierConstraintFolder<'a, SC> {
    type F = Val<SC>;
    type Expr = SC::Challenge;
    type Var = SC::Challenge;
    type M = ViewPair<'a, SC::Challenge>;

    fn main(&self) -> Self::M {
        unimplemented!("use MultiStageAirBuilder instead")
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

impl<SC: StarkGenericConfig> MultistageAirBuilder for VerifierConstraintFolder<'_, SC> {
    type Challenge = Val<SC>;
    type PublicVar = Val<SC>;

    fn stage_trace(&self, stage: u8) -> <Self as AirBuilder>::M {
        self.traces_by_stage[stage as usize]
    }

    fn stage_challenges(&self, stage: u8) -> &[Self::Challenge] {
        &self.challenges[stage as usize]
    }
    fn stage_public_values(&self, stage: u8) -> &[Self::PublicVar] {
        &self.public_values_by_stage[stage as usize]
    }
}

impl<SC: StarkGenericConfig> PairBuilder for VerifierConstraintFolder<'_, SC> {
    fn preprocessed(&self) -> Self::M {
        self.preprocessed
    }
}
