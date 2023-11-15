use crate::file_writer::BBFiles;

pub trait ComposerBuilder {
    fn create_composer_cpp(&mut self, name: &str);
    fn create_composer_hpp(&mut self, name: &str);
}

impl ComposerBuilder for BBFiles {
    fn create_composer_cpp(&mut self, name: &str) {
        // Create a composer file, this is used to a prover and verifier for our flavour
        let include_str = cpp_includes(name);

        let composer_cpp = format!(
        "
{include_str}

namespace proof_system::honk {{

template <typename Flavor> void {name}Composer_<Flavor>::compute_witness(CircuitConstructor& circuit)
{{
    if (computed_witness) {{
        return;
    }}

    auto polynomials = circuit.compute_polynomials();

    auto key_wires = proving_key->get_wires();
    auto poly_wires = polynomials.get_wires();

    for (size_t i = 0; i < key_wires.size(); ++i) {{
        std::copy(poly_wires[i].begin(), poly_wires[i].end(), key_wires[i].begin());
    }}

    computed_witness = true;
}}

template <typename Flavor>
{name}Prover_<Flavor> {name}Composer_<Flavor>::create_prover(CircuitConstructor& circuit_constructor)
{{
    compute_proving_key(circuit_constructor);
    compute_witness(circuit_constructor);
    compute_commitment_key(circuit_constructor.get_circuit_subgroup_size());

    {name}Prover_<Flavor> output_state(proving_key, commitment_key);

    return output_state;
}}

template <typename Flavor>
{name}Verifier_<Flavor> {name}Composer_<Flavor>::create_verifier(
    CircuitConstructor& circuit_constructor)
{{
    auto verification_key = compute_verification_key(circuit_constructor);

    {name}Verifier_<Flavor> output_state(verification_key);

    auto pcs_verification_key = std::make_unique<VerifierCommitmentKey>(verification_key->circuit_size, crs_factory_);

    output_state.pcs_verification_key = std::move(pcs_verification_key);

    return output_state;
}}

template <typename Flavor>
std::shared_ptr<typename Flavor::ProvingKey> {name}Composer_<Flavor>::compute_proving_key(
    CircuitConstructor& circuit_constructor)
{{
    if (proving_key) {{
        return proving_key;
    }}

    // Initialize proving_key
    {{
        const size_t subgroup_size = circuit_constructor.get_circuit_subgroup_size();
        proving_key = std::make_shared<typename Flavor::ProvingKey>(subgroup_size, 0);
    }}

    proving_key->contains_recursive_proof = false;

    return proving_key;
}}

template <typename Flavor>
std::shared_ptr<typename Flavor::VerificationKey> {name}Composer_<Flavor>::compute_verification_key(
    CircuitConstructor& circuit_constructor)
{{
    if (verification_key) {{
        return verification_key;
    }}

    if (!proving_key) {{
        compute_proving_key(circuit_constructor);
    }}

    verification_key =
        std::make_shared<typename Flavor::VerificationKey>(proving_key->circuit_size, proving_key->num_public_inputs);

    return verification_key;
}}

template class {name}Composer_<honk::flavor::{name}Flavor>;

}}    
");
        self.composer_cpp = Some(composer_cpp);
    }

    fn create_composer_hpp(&mut self, name: &str) {
        let include_str = hpp_includes(name);

        let composer_hpp = format!(
        "
{include_str}

namespace proof_system::honk {{
template <typename Flavor> class {name}Composer_ {{
    public:
        using CircuitConstructor = {name}TraceBuilder;
        using ProvingKey = typename Flavor::ProvingKey;
        using VerificationKey = typename Flavor::VerificationKey;
        using PCS = typename Flavor::PCS;
        using CommitmentKey = typename Flavor::CommitmentKey;
        using VerifierCommitmentKey = typename Flavor::VerifierCommitmentKey;

        // TODO: which of these will we really need
        static constexpr std::string_view NAME_STRING = \"{name}\";
        static constexpr size_t NUM_RESERVED_GATES = 0; 
        static constexpr size_t NUM_WIRES = Flavor::NUM_WIRES;

        std::shared_ptr<ProvingKey> proving_key;
        std::shared_ptr<VerificationKey> verification_key;

        // The crs_factory holds the path to the srs and exposes methods to extract the srs elements
        std::shared_ptr<barretenberg::srs::factories::CrsFactory<typename Flavor::Curve>> crs_factory_;

        // The commitment key is passed to the prover but also used herein to compute the verfication key commitments
        std::shared_ptr<CommitmentKey> commitment_key;

        std::vector<uint32_t> recursive_proof_public_input_indices;
        bool contains_recursive_proof = false;
        bool computed_witness = false;

        {name}Composer_() requires(std::same_as<Flavor, honk::flavor::{name}Flavor>)
        {{
            crs_factory_ = barretenberg::srs::get_crs_factory();
        }}

        {name}Composer_(std::shared_ptr<ProvingKey> p_key, std::shared_ptr<VerificationKey> v_key)
            : proving_key(std::move(p_key))
            , verification_key(std::move(v_key))
        {{}}

        {name}Composer_({name}Composer_&& other) noexcept = default;
        {name}Composer_({name}Composer_ const& other) noexcept = default;
        {name}Composer_& operator=({name}Composer_&& other) noexcept = default;
        {name}Composer_& operator=({name}Composer_ const& other) noexcept = default;
        ~{name}Composer_() = default;

        std::shared_ptr<ProvingKey> compute_proving_key(CircuitConstructor& circuit_constructor);
        std::shared_ptr<VerificationKey> compute_verification_key(CircuitConstructor& circuit_constructor);

        void compute_witness(CircuitConstructor& circuit_constructor);

        {name}Prover_<Flavor> create_prover(CircuitConstructor& circuit_constructor);
        {name}Verifier_<Flavor> create_verifier(CircuitConstructor& circuit_constructor);

        void add_table_column_selector_poly_to_proving_key(barretenberg::polynomial& small, const std::string& tag);

        void compute_commitment_key(size_t circuit_size)
        {{
            commitment_key = std::make_shared<CommitmentKey>(circuit_size, crs_factory_);
        }};
}};

extern template class {name}Composer_<honk::flavor::{name}Flavor>;
using {name}Composer = {name}Composer_<honk::flavor::{name}Flavor>;

}} // namespace proof_system::honk
"
    );
        self.composer_hpp = Some(composer_hpp);
    }
}

fn cpp_includes(name: &str) -> String {
    format!(
        "
#include \"./{name}_composer.hpp\"
#include \"barretenberg/honk/proof_system/generated/{name}_verifier.hpp\"
#include \"barretenberg/honk/proof_system/grand_product_library.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_trace.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/proof_system/composer/permutation_lib.hpp\"
"
    )
}

pub fn hpp_includes(name: &str) -> String {
    format!(
        "
#pragma once

#include \"barretenberg/honk/proof_system/generated/{name}_prover.hpp\"
#include \"barretenberg/honk/proof_system/generated/{name}_verifier.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_trace.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/srs/global_crs.hpp\"
    "
    )
}
