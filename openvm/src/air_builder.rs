use std::sync::Arc;

use openvm_stark_backend::air_builders::symbolic::get_symbolic_builder;
use openvm_stark_backend::air_builders::symbolic::SymbolicRapBuilder;
use openvm_stark_backend::config::Com;
use openvm_stark_backend::config::StarkGenericConfig;
use openvm_stark_backend::config::Val;
use openvm_stark_backend::interaction::RapPhaseSeqKind;
use openvm_stark_backend::keygen::types::ProverOnlySinglePreprocessedData;
use openvm_stark_backend::keygen::types::TraceWidth;
use openvm_stark_backend::keygen::types::VerifierSinglePreprocessedData;
use openvm_stark_backend::p3_commit::Pcs;
use openvm_stark_backend::p3_matrix::Matrix;
use openvm_stark_backend::rap::AnyRap;

pub struct PrepKeygenData<SC: StarkGenericConfig> {
    pub _verifier_data: Option<VerifierSinglePreprocessedData<Com<SC>>>,
    pub prover_data: Option<ProverOnlySinglePreprocessedData<SC>>,
}

pub struct AirKeygenBuilder<SC: StarkGenericConfig> {
    air: Arc<dyn AnyRap<SC>>,
    prep_keygen_data: PrepKeygenData<SC>,
}

fn compute_prep_data_for_air<SC: StarkGenericConfig>(
    pcs: &SC::Pcs,
    air: &dyn AnyRap<SC>,
) -> PrepKeygenData<SC> {
    let preprocessed_trace = air.preprocessed_trace();
    let vpdata_opt = preprocessed_trace.map(|trace| {
        let domain = pcs.natural_domain_for_degree(trace.height());
        let (commit, data) = pcs.commit(vec![(domain, trace.clone())]);
        let vdata = VerifierSinglePreprocessedData { commit };
        let pdata = ProverOnlySinglePreprocessedData {
            trace: Arc::new(trace),
            data: Arc::new(data),
        };
        (vdata, pdata)
    });
    if let Some((vdata, pdata)) = vpdata_opt {
        PrepKeygenData {
            prover_data: Some(pdata),
            _verifier_data: Some(vdata),
        }
    } else {
        PrepKeygenData {
            prover_data: None,
            _verifier_data: None,
        }
    }
}

impl<SC: StarkGenericConfig> AirKeygenBuilder<SC> {
    pub fn new(pcs: &SC::Pcs, air: Arc<dyn AnyRap<SC>>) -> Self {
        let prep_keygen_data = compute_prep_data_for_air(pcs, air.as_ref());
        AirKeygenBuilder {
            air,
            prep_keygen_data,
        }
    }

    pub fn get_symbolic_builder(
        &self,
        max_constraint_degree: Option<usize>,
    ) -> SymbolicRapBuilder<Val<SC>> {
        let width = TraceWidth {
            preprocessed: self.prep_keygen_data.width(),
            cached_mains: self.air.cached_main_widths(),
            common_main: self.air.common_main_width(),
            after_challenge: vec![],
        };
        get_symbolic_builder(
            self.air.as_ref(),
            &width,
            &[],
            &[],
            RapPhaseSeqKind::FriLogUp,
            max_constraint_degree.unwrap_or(0),
        )
    }
}

impl<SC: StarkGenericConfig> PrepKeygenData<SC> {
    pub fn width(&self) -> Option<usize> {
        self.prover_data.as_ref().map(|d| d.trace.width())
    }
}
