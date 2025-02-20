use p3_air::{Air, AirBuilder};

pub trait MultistageAirBuilder: AirBuilder {
    type Challenge: Clone + Into<Self::Expr>;
    type PublicVar: Into<Self::Expr> + Copy + alloc::fmt::Debug;

    /// Traces from each stage.
    fn stage_trace(&self, stage: u8) -> Self::M;

    /// Challenges from each stage, drawn from the base field
    fn stage_challenges(&self, stage: u8) -> &[Self::Challenge];

    /// Public values for each stage
    fn stage_public_values(&self, stage: u8) -> &[Self::PublicVar];
}

pub trait MultiStageAir<AB: AirBuilder>: Air<AB> {
    fn stage_public_count(&self, stage: u8) -> usize;

    fn preprocessed_width(&self) -> usize;

    fn stage_count(&self) -> u8;

    /// The number of trace columns in this stage
    fn stage_trace_width(&self, stage: u8) -> usize;

    /// The number of challenges produced at the end of each stage
    fn stage_challenge_count(&self, _stage: u8) -> usize;
}
