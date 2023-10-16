fn cpp_includes(name: &str) -> String {
    format!(
        "
#include \"barretenberg/honk/composer/generated/{name}_composer.hpp\"
#include \"barretenberg/honk/proof_system/grand_product_library.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_builder.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/proof_system/composer/permutation_lib.hpp\"
"
    )
}

pub fn composer_builder_cpp(name: &str) -> String {
    // Create a composer file, this is used to a prover and verifier for our flavour
    let include_str = cpp_includes(name);

    format!(
        "
{include_str}

namespace proof_system::honk {{

template <typename Flavor>
std::shared_ptr<ProverInstance_<Flavor>> {name}Composer_<Flavor>::create_instance(CircuitBuilder& circuit)
{{
    circuit.build_circuit();
    auto instance = std::make_shared<Instance>(circuit);
    instance->commitment_key = compute_commitment_key(instance->proving_key->circuit_size);
    return instance;
}}

template <typename Flavor>
{name}Prover_<Flavor> {name}Composer_<Flavor>::create_prover(const std::shared_ptr<Instance>& instance)
{{
    return {name}Prover_<Flavor>(instance);
}}

template <typename Flavor>
{name}Verifier_<Flavor> {name}Composer_<Flavor>::create_verifier(const std::shared_ptr<Instance>& instance)
{{
    auto vk = instance->compute_verification_key();
    {name}Verifier_<Flavor> output_state(vk);
    auto pcs_vk = std::make_unique<VerifierCommitmentKey>(verification_key->circuit_size, crs_factory_);
    output_state.pcs_verification_key = std::move(pcs_vk);

    return output_state;
}}

template class {name}Composer_<honk::flavor::{name}Flavor>;

}}    
")
}

pub fn hpp_includes(name: &str) -> String {
    format!(
        "
#pragma once

#include \"barretenberg/honk/proof_system/generated/{name}_prover.hpp\"
#include \"barretenberg/honk/proof_system/generated/{name}_verifier.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_builder.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/srs/factories/file_crs_factory.hpp\"
    "
    )
}

pub fn composer_builder_hpp(name: &str) -> String {
    let include_str = hpp_includes(name);

    format!(
        "
{include_str}

namespace proof_system::honk {{
template <typename Flavor> class {name}Composer_ {{
    public:
        // using CircuitConstructor = ECCVMCircuitBuilder<Flavor>; // TODO what should this be?
        using ProvingKey = typename Flavor::ProvingKey;
        using VerificationKey = typename Flavor::VerificationKey;
        using PCS = typename Flavor::PCS;
        using CommitmentKey = typename Flavor::CommitmentKey;
        using VerifierCommitmentKey = typename Flavor::VerifierCommitmentKey;

        // TODO: which of these will we really need
        static constexpr std::string_view NAME_STRING = \"{name}\";
        static constexpr size_t NUM_RESERVED_GATES = 0; 
        static constexpr size_t NUM_WIRES = CircuitConstructor::NUM_WIRES;

        std::shared_ptr<ProvingKey> proving_key;
        std::shared_ptr<VerificationKey> verification_key;

        // The crs_factory holds the path to the srs and exposes methods to extract the srs elements
        std::shared_ptr<barretenberg::srs::factories::CrsFactory<typename Flavor::Curve>> crs_factory_;

        // The commitment key is passed to the prover but also used herein to compute the verfication key commitments
        std::shared_ptr<CommitmentKey> commitment_key;

        std::vector<uint32_t> recursive_proof_public_input_indices;
        bool contains_recursive_proof = false;
        bool computer_witness = false;

        {name}Composer_() requires(std::same_as<Flavor, honk::flavor::{name}Flavor)
        {{
            crs_factory_ = barretenberg::srs::get_crs_factory();
        }}

        {name}Composer_(std::shared_ptr<ProvingKey> p_key, std::shared_ptr<VerificationKey> v_key)
            : proving_key(std::move(p_key))
            , verification_key(std::move(v_key))
        {{}}

        {name}Composer_(ECCVMComposer_&& other) noexcept = default;
        {name}Composer_(ECCVMComposer_ const& other) noexcept = default;
        {name}Composer_& operator=(ECCVMComposer_&& other) noexcept = default;
        {name}Composer_& operator=(ECCVMComposer_ const& other) noexcept = default;
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
    )
}
