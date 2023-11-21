use crate::file_writer::BBFiles;

pub trait ProverBuilder {
    fn create_prover_cpp(&mut self, name: &str, fixed: &[String], to_be_shifted: &[String]);

    fn create_prover_hpp(&mut self, name: &str);
}

impl ProverBuilder for BBFiles {
    fn create_prover_hpp(&mut self, name: &str) {
        let include_str = includes_hpp(name);
        let prover_hpp = format!("
    {include_str} 
    namespace proof_system::honk {{
    
    class {name}Prover {{
    
        using Flavor = honk::flavor::{name}Flavor;
        using FF = Flavor::FF;
        using PCS = Flavor::PCS;
        using PCSCommitmentKey = Flavor::CommitmentKey;
        using ProvingKey = Flavor::ProvingKey;
        using Polynomial = Flavor::Polynomial;
        using ProverPolynomials = Flavor::ProverPolynomials;
        using CommitmentLabels = Flavor::CommitmentLabels;
        using Curve = Flavor::Curve;
        using Transcript = Flavor::Transcript;
    
      public:
        explicit {name}Prover(std::shared_ptr<ProvingKey> input_key, std::shared_ptr<PCSCommitmentKey> commitment_key);
    
        void execute_preamble_round();
        void execute_wire_commitments_round();
        void execute_relation_check_rounds();
        void execute_zeromorph_rounds();
    
        plonk::proof& export_proof();
        plonk::proof& construct_proof();
    
        Transcript transcript;
    
        std::vector<FF> public_inputs;
    
        proof_system::RelationParameters<FF> relation_parameters;
    
        std::shared_ptr<ProvingKey> key;
    
        // Container for spans of all polynomials required by the prover (i.e. all multivariates evaluated by Sumcheck).
        ProverPolynomials prover_polynomials;
    
        CommitmentLabels commitment_labels;

        Polynomial quotient_W;
    
        sumcheck::SumcheckOutput<Flavor> sumcheck_output;
    
        std::shared_ptr<PCSCommitmentKey> commitment_key;
    
        using ZeroMorph = pcs::zeromorph::ZeroMorphProver_<Curve>;
    
      private:
        plonk::proof proof;
    }};
    
    }} // namespace proof_system::honk
     
    ");
        self.prover_hpp = Some(prover_hpp);
    }

    fn create_prover_cpp(&mut self, name: &str, fixed: &[String], to_be_shifted: &[String]) {
        let include_str = includes_cpp(name);

        // Create the wire assignments, prover_polynomial = key
        let fixed_assignments = fixed
            .iter()
            .map(|name| {
                let n = name.replace('.', "_");
                format!("prover_polynomials.{n} = key->{n};", n = n)
            })
            .collect::<Vec<_>>()
            .join("\n");

        let committed_assignments = to_be_shifted
            .iter()
            .map(|name| {
                let n = name.replace('.', "_");
                format!(
                    "
prover_polynomials.{n} = key->{n};
prover_polynomials.{n}_shift = key->{n}.shifted();
",
                    n = n
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Lmao yuck
        let all_assignments = format!(
            "
    {fixed_assignments}
    {committed_assignments}
    "
        );

        let prover_cpp = format!("
    {include_str}
    
    namespace proof_system::honk {{

    using Flavor = honk::flavor::{name}Flavor;
    
    /**
     * Create {name}Prover from proving key, witness and manifest.
     *
     * @param input_key Proving key.
     * @param input_manifest Input manifest
     *
     * @tparam settings Settings class.
     * */
    {name}Prover::{name}Prover(std::shared_ptr<Flavor::ProvingKey> input_key,
                                       std::shared_ptr<PCSCommitmentKey> commitment_key)
        : key(input_key)
        , commitment_key(commitment_key)
    {{
        // TODO: take every polynomial and assign it to the key!!
        {all_assignments}
    
        // prover_polynomials.lookup_inverses = key->lookup_inverses;
        // key->z_perm = Polynomial(key->circuit_size);
        // prover_polynomials.z_perm = key->z_perm;
    }}
    

    /**
     * @brief Add circuit size, public input size, and public inputs to transcript
     *
     */
    void {name}Prover::execute_preamble_round()
    {{
        const auto circuit_size = static_cast<uint32_t>(key->circuit_size);
    
        transcript.send_to_verifier(\"circuit_size\", circuit_size);
    }}
    
    /**
     * @brief Compute commitments to the first three wires
     *
     */
    void {name}Prover::execute_wire_commitments_round()
    {{
        auto wire_polys = key->get_wires();
        auto labels = commitment_labels.get_wires();
        for (size_t idx = 0; idx < wire_polys.size(); ++idx) {{
            transcript.send_to_verifier(labels[idx], commitment_key->commit(wire_polys[idx]));
        }}
    }}
    

    
    /**
     * @brief Run Sumcheck resulting in u = (u_1,...,u_d) challenges and all evaluations at u being calculated.
     *
     */
    void {name}Prover::execute_relation_check_rounds()
    {{
        using Sumcheck = sumcheck::SumcheckProver<Flavor>;
    
        auto sumcheck = Sumcheck(key->circuit_size, transcript);
        auto alpha = transcript.get_challenge(\"alpha\");
    
        sumcheck_output = sumcheck.prove(prover_polynomials, relation_parameters, alpha);
    }}


    /**
     * @brief Execute the ZeroMorph protocol to prove the multilinear evaluations produced by Sumcheck
     * @details See https://hackmd.io/dlf9xEwhTQyE3hiGbq4FsA?view for a complete description of the unrolled protocol.
     *
     * */
     void FibProver::execute_zeromorph_rounds()
    {{
        ZeroMorph::prove(prover_polynomials.get_unshifted(),
                         prover_polynomials.get_to_be_shifted(),
                         sumcheck_output.claimed_evaluations.get_unshifted(),
                         sumcheck_output.claimed_evaluations.get_shifted(),
                         sumcheck_output.challenge,
                         commitment_key,
                         transcript);

    }}

    
    plonk::proof& {name}Prover::export_proof()
    {{
        proof.proof_data = transcript.proof_data;
        return proof;
    }}
    
    plonk::proof& {name}Prover::construct_proof()
    {{
        // Add circuit size public input size and public inputs to transcript.
        execute_preamble_round();
    
        // Compute wire commitments
        execute_wire_commitments_round();
    
        // TODO: not implemented for codegen just yet
        // Compute sorted list accumulator and commitment
        // execute_log_derivative_commitments_round();
    
        // Fiat-Shamir: bbeta & gamma
        // Compute grand product(s) and commitments.
        // execute_grand_product_computation_round();
    
        // Fiat-Shamir: alpha
        // Run sumcheck subprotocol.
        execute_relation_check_rounds();
    
        // Fiat-Shamir: rho, y, x, z
        // Execute Zeromorph multilinear PCS
        execute_zeromorph_rounds();
    
        return export_proof();
    }}
    
    }} // namespace proof_system::honk
     
    
    ");
        self.prover_cpp = Some(prover_cpp);
    }
}

fn includes_hpp(name: &str) -> String {
    format!(
        "
#pragma once
#include \"barretenberg/flavor/generated/{name}_flavor.hpp\"
#include \"barretenberg/commitment_schemes/zeromorph/zeromorph.hpp\"
#include \"barretenberg/sumcheck/sumcheck_output.hpp\"
#include \"barretenberg/transcript/transcript.hpp\"
#include \"barretenberg/plonk/proof_system/types/proof.hpp\"
#include \"barretenberg/relations/relation_parameters.hpp\"

    "
    )
}

fn includes_cpp(name: &str) -> String {
    format!(
        "
    
    #include \"{name}_prover.hpp\"
    #include \"barretenberg/commitment_schemes/claim.hpp\"
    #include \"barretenberg/commitment_schemes/commitment_key.hpp\"
    #include \"barretenberg/honk/proof_system/lookup_library.hpp\"
    #include \"barretenberg/honk/proof_system/permutation_library.hpp\"
    #include \"barretenberg/honk/proof_system/power_polynomial.hpp\"
    #include \"barretenberg/polynomials/polynomial.hpp\"
    #include \"barretenberg/proof_system/library/grand_product_library.hpp\"
    #include \"barretenberg/relations/lookup_relation.hpp\"
    #include \"barretenberg/relations/permutation_relation.hpp\"
    #include \"barretenberg/sumcheck/sumcheck.hpp\"
    "
    )
}
