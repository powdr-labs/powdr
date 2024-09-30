use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues};

pub trait MultistageAirBuilder: AirBuilderWithPublicValues {
    type Challenge: Clone + Into<Self::Expr>;

    /// Traces from each stage.
    fn stage_trace(&self, stage: usize) -> Self::M;

    /// Challenges from each stage, drawn from the base field
    fn stage_challenges(&self, stage: usize) -> &[Self::Challenge];

    /// Public values for each stage
    fn stage_public_values(&self, stage: usize) -> &[Self::PublicVar] {
        match stage {
            0 => self.public_values(),
            _ => unimplemented!(),
        }
    }
}

pub trait MultiStageAir<AB: AirBuilder>: Air<AB> {
    fn stage_public_count(&self, stage: u32) -> usize;

    fn preprocessed_width(&self) -> usize;

    fn stage_count(&self) -> usize {
        1
    }

    /// The number of trace columns in this stage
    fn stage_trace_width(&self, stage: u32) -> usize {
        match stage {
            0 => self.width(),
            _ => unimplemented!(),
        }
    }

    /// The number of challenges produced at the end of each stage
    fn stage_challenge_count(&self, _stage: u32) -> usize {
        0
    }
}
