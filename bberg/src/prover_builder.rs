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
    
    template <typename Flavor> class {name}Prover_ {{
    
        using FF = typename Flavor::FF;
        using PCS = typename Flavor::PCS;
        using PCSCommitmentKey = typename Flavor::CommitmentKey;
        using ProvingKey = typename Flavor::ProvingKey;
        using Polynomial = typename Flavor::Polynomial;
        using ProverPolynomials = typename Flavor::ProverPolynomials;
        using CommitmentLabels = typename Flavor::CommitmentLabels;
        using Curve = typename Flavor::Curve;
    
      public:
        explicit {name}Prover_(std::shared_ptr<ProvingKey> input_key, std::shared_ptr<PCSCommitmentKey> commitment_key);
    
        void execute_preamble_round();
        void execute_wire_commitments_round();
        // void execute_log_derivative_commitments_round();
        // void execute_grand_product_computation_round();
        void execute_relation_check_rounds();
        void execute_univariatization_round();
        void execute_pcs_evaluation_round();
        void execute_shplonk_batched_quotient_round();
        void execute_shplonk_partial_evaluation_round();
        void execute_final_pcs_round();
    
        void compute_wire_commitments();
    
        plonk::proof& export_proof();
        plonk::proof& construct_proof();
    
        ProverTranscript<FF> transcript;
    
        std::vector<FF> public_inputs;
    
        proof_system::RelationParameters<FF> relation_parameters;
    
        std::shared_ptr<ProvingKey> key;
    
        // Container for spans of all polynomials required by the prover (i.e. all multivariates evaluated by Sumcheck).
        ProverPolynomials prover_polynomials;
    
        CommitmentLabels commitment_labels;
    
        // Container for d + 1 Fold polynomials produced by Gemini
        std::vector<Polynomial> gemini_polynomials;
    
        Polynomial batched_quotient_Q; // batched quotient poly computed by Shplonk
        FF nu_challenge;               // needed in both Shplonk rounds
    
        Polynomial quotient_W;
    
        sumcheck::SumcheckOutput<Flavor> sumcheck_output;
        pcs::gemini::ProverOutput<Curve> gemini_output;
        pcs::shplonk::ProverOutput<Curve> shplonk_output;
        std::shared_ptr<PCSCommitmentKey> commitment_key;
    
        using Gemini = pcs::gemini::GeminiProver_<Curve>;
        using Shplonk = pcs::shplonk::ShplonkProver_<Curve>;
    
      private:
        plonk::proof proof;
    }};
    
    extern template class {name}Prover_<honk::flavor::{name}Flavor>;
    
    using {name}Prover = {name}Prover_<honk::flavor::{name}Flavor>;
    
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
    
    /**
     * Create {name}Prover_ from proving key, witness and manifest.
     *
     * @param input_key Proving key.
     * @param input_manifest Input manifest
     *
     * @tparam settings Settings class.
     * */
    template <typename Flavor>
    {name}Prover_<Flavor>::{name}Prover_(std::shared_ptr<typename Flavor::ProvingKey> input_key,
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
     * @brief Commit to the wires
     *
     */
    template <typename Flavor> void {name}Prover_<Flavor>::compute_wire_commitments()
    {{
        auto wire_polys = key->get_wires();
        auto labels = commitment_labels.get_wires();
        for (size_t idx = 0; idx < wire_polys.size(); ++idx) {{
            transcript.send_to_verifier(labels[idx], commitment_key->commit(wire_polys[idx]));
        }}
    }}
    
    /**
     * @brief Add circuit size, public input size, and public inputs to transcript
     *
     */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_preamble_round()
    {{
        const auto circuit_size = static_cast<uint32_t>(key->circuit_size);
    
        transcript.send_to_verifier(\"circuit_size\", circuit_size);
    }}
    
    /**
     * @brief Compute commitments to the first three wires
     *
     */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_wire_commitments_round()
    {{
        auto wire_polys = key->get_wires();
        auto labels = commitment_labels.get_wires();
        for (size_t idx = 0; idx < wire_polys.size(); ++idx) {{
            transcript.send_to_verifier(labels[idx], commitment_key->commit(wire_polys[idx]));
        }}
    }}
    
    /**
     * @brief Compute sorted witness-table accumulator
     *
     */
    // template <typename Flavor> void {name}Prover_<Flavor>::execute_log_derivative_commitments_round()
    // {{
    //     // Compute and add beta to relation parameters
    //     auto [beta, gamma] = transcript.get_challenges(\"beta\", \"gamma\");
    //     // TODO(#583)(@zac-williamson): fix Transcript to be able to generate more than 2 challenges per round! oof.
    //     auto beta_sqr = beta * beta;
    //     relation_parameters.gamma = gamma;
    //     relation_parameters.beta = beta;
    //     relation_parameters.beta_sqr = beta_sqr;
    //     relation_parameters.beta_cube = beta_sqr * beta;
    //     relation_parameters.{name}_set_permutation_delta =
    //         gamma * (gamma + beta_sqr) * (gamma + beta_sqr + beta_sqr) * (gamma + beta_sqr + beta_sqr + beta_sqr);
    //     relation_parameters.{name}_set_permutation_delta = relation_parameters.{name}_set_permutation_delta.invert();
    //     // Compute inverse polynomial for our logarithmic-derivative lookup method
    //     lookup_library::compute_logderivative_inverse<Flavor, typename Flavor::LookupRelation>(
    //         prover_polynomials, relation_parameters, key->circuit_size);
    //     transcript.send_to_verifier(commitment_labels.lookup_inverses, commitment_key->commit(key->lookup_inverses));
    //     prover_polynomials.lookup_inverses = key->lookup_inverses;
    // }}
    
    /**
     * @brief Compute permutation and lookup grand product polynomials and commitments
     *
     */
    // template <typename Flavor> void {name}Prover_<Flavor>::execute_grand_product_computation_round()
    // {{
    //     // Compute permutation grand product and their commitments
    //     permutation_library::compute_permutation_grand_products<Flavor>(key, prover_polynomials, relation_parameters);
    
    //     transcript.send_to_verifier(commitment_labels.z_perm, commitment_key->commit(key->z_perm));
    // }}
    
    /**
     * @brief Run Sumcheck resulting in u = (u_1,...,u_d) challenges and all evaluations at u being calculated.
     *
     */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_relation_check_rounds()
    {{
        using Sumcheck = sumcheck::SumcheckProver<Flavor>;
    
        auto sumcheck = Sumcheck(key->circuit_size, transcript);
    
        sumcheck_output = sumcheck.prove(prover_polynomials, relation_parameters);
    }}
    
    /**
     * - Get rho challenge
     * - Compute d+1 Fold polynomials and their evaluations.
     *
     * */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_univariatization_round()
    {{
        const size_t NUM_POLYNOMIALS = Flavor::NUM_ALL_ENTITIES;
    
        // Generate batching challenge ρ and powers 1,ρ,…,ρᵐ⁻¹
        FF rho = transcript.get_challenge(\"rho\");
        std::vector<FF> rhos = pcs::gemini::powers_of_rho(rho, NUM_POLYNOMIALS);
    
        // Batch the unshifted polynomials and the to-be-shifted polynomials using ρ
        Polynomial batched_poly_unshifted(key->circuit_size); // batched unshifted polynomials
        size_t poly_idx = 0;                                  // TODO(#391) zip
        for (auto& unshifted_poly : prover_polynomials.get_unshifted()) {{
            batched_poly_unshifted.add_scaled(unshifted_poly, rhos[poly_idx]);
            ++poly_idx;
        }}
    
        Polynomial batched_poly_to_be_shifted(key->circuit_size); // batched to-be-shifted polynomials
        for (auto& to_be_shifted_poly : prover_polynomials.get_to_be_shifted()) {{
            batched_poly_to_be_shifted.add_scaled(to_be_shifted_poly, rhos[poly_idx]);
            ++poly_idx;
        }};
    
        // Compute d-1 polynomials Fold^(i), i = 1, ..., d-1.
        gemini_polynomials = Gemini::compute_gemini_polynomials(
            sumcheck_output.challenge, std::move(batched_poly_unshifted), std::move(batched_poly_to_be_shifted));
    
        // Compute and add to trasnscript the commitments [Fold^(i)], i = 1, ..., d-1
        for (size_t l = 0; l < key->log_circuit_size - 1; ++l) {{
            transcript.send_to_verifier(\"Gemini:FOLD_\" + std::to_string(l + 1),
                commitment_key->commit(gemini_polynomials[l + 2]));
        }}
    }}
    
    /**
     * - Do Fiat-Shamir to get \"r\" challenge
     * - Compute remaining two partially evaluated Fold polynomials Fold_{{r}}^(0) and Fold_{{-r}}^(0).
     * - Compute and aggregate opening pairs (challenge, evaluation) for each of d Fold polynomials.
     * - Add d-many Fold evaluations a_i, i = 0, ..., d-1 to the transcript, excluding eval of Fold_{{r}}^(0)
     * */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_pcs_evaluation_round()
    {{
        const FF r_challenge = transcript.get_challenge(\"Gemini:r\");
        gemini_output = Gemini::compute_fold_polynomial_evaluations(
            sumcheck_output.challenge, std::move(gemini_polynomials), r_challenge);
    
        for (size_t l = 0; l < key->log_circuit_size; ++l) {{
            std::string label = \"Gemini:a_\" + std::to_string(l);
            const auto& evaluation = gemini_output.opening_pairs[l + 1].evaluation;
            transcript.send_to_verifier(label, evaluation);
        }}
    }}
    
    /**
     * - Do Fiat-Shamir to get \"nu\" challenge.
     * - Compute commitment [Q]_1
     * */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_shplonk_batched_quotient_round()
    {{
        nu_challenge = transcript.get_challenge(\"Shplonk:nu\");
    
        batched_quotient_Q =
            Shplonk::compute_batched_quotient(gemini_output.opening_pairs, gemini_output.witnesses, nu_challenge);
    
        // commit to Q(X) and add [Q] to the transcript
        transcript.send_to_verifier(\"Shplonk:Q\", commitment_key->commit(batched_quotient_Q));
    }}
    
    /**
     * - Do Fiat-Shamir to get \"z\" challenge.
     * - Compute polynomial Q(X) - Q_z(X)
     * */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_shplonk_partial_evaluation_round()
    {{
        const FF z_challenge = transcript.get_challenge(\"Shplonk:z\");
    
        shplonk_output = Shplonk::compute_partially_evaluated_batched_quotient(
            gemini_output.opening_pairs, gemini_output.witnesses, std::move(batched_quotient_Q), nu_challenge, z_challenge);
    }}
    /**
     * - Compute final PCS opening proof:
     * - For KZG, this is the quotient commitment [W]_1
     * - For IPA, the vectors L and R
     * */
    template <typename Flavor> void {name}Prover_<Flavor>::execute_final_pcs_round()
    {{
        PCS::compute_opening_proof(commitment_key, shplonk_output.opening_pair, shplonk_output.witness, transcript);
    }}
    
    template <typename Flavor> plonk::proof& {name}Prover_<Flavor>::export_proof()
    {{
        proof.proof_data = transcript.proof_data;
        return proof;
    }}
    
    template <typename Flavor> plonk::proof& {name}Prover_<Flavor>::construct_proof()
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
    
        // Fiat-Shamir: rho
        // Compute Fold polynomials and their commitments.
        execute_univariatization_round();
    
        // Fiat-Shamir: r
        // Compute Fold evaluations
        execute_pcs_evaluation_round();
    
        // Fiat-Shamir: nu
        // Compute Shplonk batched quotient commitment Q
        execute_shplonk_batched_quotient_round();
    
        // Fiat-Shamir: z
        // Compute partial evaluation Q_z
        execute_shplonk_partial_evaluation_round();
    
        // Fiat-Shamir: z
        // Compute PCS opening proof (either KZG quotient commitment or IPA opening proof)
        execute_final_pcs_round();
    
        return export_proof();
    }}
    
    template class {name}Prover_<honk::flavor::{name}Flavor>;
    
    }} // namespace proof_system::honk
     
    
    ");
        self.prover_cpp = Some(prover_cpp);
    }
}

fn includes_hpp(name: &str) -> String {
    format!(
        "
#pragma once
#include \"barretenberg/honk/flavor/generated/{name}_flavor.hpp\"
#include \"barretenberg/honk/pcs/gemini/gemini.hpp\"
#include \"barretenberg/honk/pcs/shplonk/shplonk.hpp\"
#include \"barretenberg/honk/sumcheck/sumcheck_output.hpp\"
#include \"barretenberg/honk/transcript/transcript.hpp\"
#include \"barretenberg/plonk/proof_system/types/proof.hpp\"
#include \"barretenberg/proof_system/relations/relation_parameters.hpp\"

    "
    )
}

fn includes_cpp(name: &str) -> String {
    format!(
        "
    
    #include \"{name}_prover.hpp\"
    #include \"barretenberg/honk/pcs/claim.hpp\"
    #include \"barretenberg/honk/pcs/commitment_key.hpp\"
    #include \"barretenberg/honk/proof_system/lookup_library.hpp\"
    #include \"barretenberg/honk/proof_system/permutation_library.hpp\"
    #include \"barretenberg/honk/sumcheck/sumcheck.hpp\"
    #include \"barretenberg/honk/utils/power_polynomial.hpp\"
    #include \"barretenberg/polynomials/polynomial.hpp\"
    #include \"barretenberg/polynomials/univariate.hpp\" // will go away
    #include \"barretenberg/proof_system/relations/lookup_relation.hpp\"
    #include \"barretenberg/proof_system/relations/permutation_relation.hpp\"
    #include \"barretenberg/transcript/transcript_wrappers.hpp\"
    #include <algorithm>
    #include <array>
    #include <cstddef>
    #include <memory>
    #include <span>
    #include <string>
    #include <utility>
    #include <vector>
    "
    )
}
