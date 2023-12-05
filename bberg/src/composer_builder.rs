use crate::{file_writer::BBFiles, utils::map_with_newline};

pub trait ComposerBuilder {
    fn create_composer_cpp(&mut self, name: &str, all_cols: &[String]);
    fn create_composer_hpp(&mut self, name: &str);
}

impl ComposerBuilder for BBFiles {
    fn create_composer_cpp(&mut self, name: &str, all_cols: &[String]) {
        // Create a composer file, this is used to a prover and verifier for our flavour
        let include_str = cpp_includes(name);

        let map_polys_to_key = create_map_polys_to_key(all_cols);

        let composer_cpp = format!(
        "
{include_str}

namespace proof_system::honk {{

using Flavor = honk::flavor::{name}Flavor;
void {name}Composer::compute_witness(CircuitConstructor& circuit)
{{
    if (computed_witness) {{
        return;
    }}

    auto polynomials = circuit.compute_polynomials();

    {map_polys_to_key}

    computed_witness = true;
}}

{name}Prover {name}Composer::create_prover(CircuitConstructor& circuit_constructor)
{{
    compute_proving_key(circuit_constructor);
    compute_witness(circuit_constructor);
    compute_commitment_key(circuit_constructor.get_circuit_subgroup_size());

    {name}Prover output_state(proving_key, commitment_key);

    return output_state;
}}

{name}Verifier {name}Composer::create_verifier(
    CircuitConstructor& circuit_constructor)
{{
    auto verification_key = compute_verification_key(circuit_constructor);

    {name}Verifier output_state(verification_key);

    auto pcs_verification_key = std::make_unique<VerifierCommitmentKey>(verification_key->circuit_size, crs_factory_);

    output_state.pcs_verification_key = std::move(pcs_verification_key);

    return output_state;
}}

std::shared_ptr<Flavor::ProvingKey> {name}Composer::compute_proving_key(
    CircuitConstructor& circuit_constructor)
{{
    if (proving_key) {{
        return proving_key;
    }}

    // Initialize proving_key
    {{
        const size_t subgroup_size = circuit_constructor.get_circuit_subgroup_size();
        proving_key = std::make_shared<Flavor::ProvingKey>(subgroup_size, 0);
    }}

    proving_key->contains_recursive_proof = false;

    return proving_key;
}}

std::shared_ptr<Flavor::VerificationKey> {name}Composer::compute_verification_key(
    CircuitConstructor& circuit_constructor)
{{
    if (verification_key) {{
        return verification_key;
    }}

    if (!proving_key) {{
        compute_proving_key(circuit_constructor);
    }}

    verification_key =
        std::make_shared<Flavor::VerificationKey>(proving_key->circuit_size, proving_key->num_public_inputs);

    return verification_key;
}}

}}    
");
        self.write_file(
            &self.composer,
            &format!("{}_composer.cpp", name),
            &composer_cpp,
        );
    }

    fn create_composer_hpp(&mut self, name: &str) {
        let include_str = hpp_includes(name);

        let composer_hpp = format!(
        "
{include_str}

namespace proof_system::honk {{
class {name}Composer {{
    public:
        using Flavor = honk::flavor::{name}Flavor;
        using CircuitConstructor = {name}CircuitBuilder;
        using ProvingKey = Flavor::ProvingKey;
        using VerificationKey = Flavor::VerificationKey;
        using PCS = Flavor::PCS;
        using CommitmentKey = Flavor::CommitmentKey;
        using VerifierCommitmentKey = Flavor::VerifierCommitmentKey;

        // TODO: which of these will we really need
        static constexpr std::string_view NAME_STRING = \"{name}\";
        static constexpr size_t NUM_RESERVED_GATES = 0; 
        static constexpr size_t NUM_WIRES = Flavor::NUM_WIRES;

        std::shared_ptr<ProvingKey> proving_key;
        std::shared_ptr<VerificationKey> verification_key;

        // The crs_factory holds the path to the srs and exposes methods to extract the srs elements
        std::shared_ptr<barretenberg::srs::factories::CrsFactory<Flavor::Curve>> crs_factory_;

        // The commitment key is passed to the prover but also used herein to compute the verfication key commitments
        std::shared_ptr<CommitmentKey> commitment_key;

        std::vector<uint32_t> recursive_proof_public_input_indices;
        bool contains_recursive_proof = false;
        bool computed_witness = false;

        {name}Composer() 
        {{
            crs_factory_ = barretenberg::srs::get_crs_factory();
        }}

        {name}Composer(std::shared_ptr<ProvingKey> p_key, std::shared_ptr<VerificationKey> v_key)
            : proving_key(std::move(p_key))
            , verification_key(std::move(v_key))
        {{}}

        {name}Composer({name}Composer&& other) noexcept = default;
        {name}Composer({name}Composer const& other) noexcept = default;
        {name}Composer& operator=({name}Composer&& other) noexcept = default;
        {name}Composer& operator=({name}Composer const& other) noexcept = default;
        ~{name}Composer() = default;

        std::shared_ptr<ProvingKey> compute_proving_key(CircuitConstructor& circuit_constructor);
        std::shared_ptr<VerificationKey> compute_verification_key(CircuitConstructor& circuit_constructor);

        void compute_witness(CircuitConstructor& circuit_constructor);

        {name}Prover create_prover(CircuitConstructor& circuit_constructor);
        {name}Verifier create_verifier(CircuitConstructor& circuit_constructor);

        void add_table_column_selector_poly_to_proving_key(barretenberg::polynomial& small, const std::string& tag);

        void compute_commitment_key(size_t circuit_size)
        {{
            commitment_key = std::make_shared<CommitmentKey>(circuit_size, crs_factory_);
        }};
}};

}} // namespace proof_system::honk
"
    );

        self.write_file(
            &self.composer,
            &format!("{}_composer.hpp", name),
            &composer_hpp,
        );
    }
}

fn cpp_includes(name: &str) -> String {
    format!(
        "
#include \"./{name}_composer.hpp\"
#include \"barretenberg/vm/generated/{name}_verifier.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_circuit_builder.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/proof_system/composer/permutation_lib.hpp\"
"
    )
}

pub fn hpp_includes(name: &str) -> String {
    format!(
        "
#pragma once

#include \"barretenberg/vm/generated/{name}_prover.hpp\"
#include \"barretenberg/vm/generated/{name}_verifier.hpp\"
#include \"barretenberg/proof_system/circuit_builder/generated/{name}_circuit_builder.hpp\"
#include \"barretenberg/proof_system/composer/composer_lib.hpp\"
#include \"barretenberg/srs/global_crs.hpp\"
    "
    )
}

fn create_map_polys_to_key(all_cols: &[String]) -> String {
    let map_transformation =
        |name: &String| format!("proving_key->{name} = polynomials.{name};", name = name);

    map_with_newline(all_cols, map_transformation)
}
