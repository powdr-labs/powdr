use std::sync::Arc;

use openvm_stark_backend::air_builders::symbolic::get_symbolic_builder;
use openvm_stark_backend::air_builders::symbolic::SymbolicRapBuilder;
use openvm_stark_backend::keygen::types::TraceWidth;
use openvm_stark_backend::p3_air::BaseAir;
use openvm_stark_backend::AnyAir;
use openvm_stark_backend::StarkProtocolConfig;
use openvm_stark_backend::Val;

pub struct AirKeygenBuilder<SC: StarkProtocolConfig> {
    air: Arc<dyn AnyAir<SC>>,
}

impl<SC: StarkProtocolConfig> AirKeygenBuilder<SC> {
    pub fn new(air: Arc<dyn AnyAir<SC>>) -> Self {
        AirKeygenBuilder { air }
    }

    pub fn get_symbolic_builder(
        &self,
        _max_constraint_degree: Option<usize>,
    ) -> SymbolicRapBuilder<Val<SC>> {
        let preprocessed_width: Option<usize> =
            BaseAir::<Val<SC>>::preprocessed_trace(self.air.as_ref()).map(|t| t.width);
        let width = TraceWidth {
            preprocessed: preprocessed_width,
            cached_mains: self.air.cached_main_widths(),
            common_main: self.air.common_main_width(),
            after_challenge: vec![],
        };
        get_symbolic_builder(self.air.as_ref(), &width, &[], &[])
    }
}
