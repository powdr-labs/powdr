use p3_air::{Air, AirBuilder};

pub trait MultistageAirBuilder: AirBuilder {
    type Challenge: Clone + Into<Self::Expr>;
    type PublicVar: Into<Self::Expr> + Copy;

    /// Traces from each stage.
    fn stage_trace(&self, stage: usize) -> Self::M;

    /// Challenges from each stage, drawn from the base field
    fn stage_challenges(&self, stage: usize) -> &[Self::Challenge];

    /// Public values for each stage
    fn stage_public_values(&self, stage: usize) -> &[Self::PublicVar];
}

pub trait MultiStageAir<AB: AirBuilder>: Air<AB> {
    fn stage_public_count(&self, stage: u32) -> usize;

    fn preprocessed_width(&self) -> usize;

    fn stage_count(&self) -> usize;

    /// The number of trace columns in this stage
    fn stage_trace_width(&self, stage: u32) -> usize;

    /// The number of challenges produced at the end of each stage
    fn stage_challenge_count(&self, _stage: u32) -> usize;
}
