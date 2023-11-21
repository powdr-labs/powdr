use crate::file_writer::BBFiles;

pub trait FlavorBuilder {
    fn create_flavor_hpp(
        &mut self,
        name: &str,
        relations: &[String],
        fixed: &[String],
        witness: &[String],
        all_cols: &[String],
        to_be_shifted: &[String],
        shifted: &[String],
    );
}

/// Build the boilerplate for the flavor file
impl FlavorBuilder for BBFiles {
    fn create_flavor_hpp(
        &mut self,
        name: &str,
        relations: &[String],
        fixed: &[String],
        witness: &[String],
        all_cols: &[String],
        to_be_shifted: &[String],
        shifted: &[String],
        // shifted: &[String],
    ) {
        let first_poly = &fixed[0];
        let includes = flavor_includes(name, relations);
        let num_precomputed = &fixed.len();
        let num_witness = witness.len();
        let num_all = num_witness + shifted.len() + to_be_shifted.len();
        // TODO: for now we include a shift OF ALL witness wires, however this is not necessarily true

        let precomputed = witness_get(fixed, false);

        let witnesses = witness_get(witness, false);
        let precomputed_str = create_precomputed_entities(&fixed);
        let witness_str = create_witness_entities(&witness);
        let all_shift = witness_get(shifted, false);

        let all_cols_and_shifts = &[all_cols.to_vec(), shifted.to_vec()]
            .into_iter()
            .flatten()
            .collect::<Vec<String>>();
        let all_entities_get_wires = make_wires_set(&all_cols_and_shifts);
        let all_entities_pointer_view =
            create_pointer_view("NUM_ALL_ENTITIES", &all_cols_and_shifts);

        let all_entities_get_unshifted = make_wires_set(all_cols);
        let all_entities_get_to_be_shifted = make_wires_set(to_be_shifted);
        let all_entities_get_shifted = make_wires_set(shifted);

        let commitment_labels_class = create_commitment_labels(all_cols);

        let verification_commitments = create_verifier_commitments(fixed);

        let transcript = generate_transcript(witness);

        // TODO: make this work when we have multiple relation files, for now we just have the one
        let relations_tuple = format!("{name}_vm::{name}<FF>");
        // let relations_tuple = relations
        //     .iter()
        //     .map(|r| format!("{}Relation", r))
        //     .collect::<Vec<_>>()
        //     .join(", ");

        let flavor_hpp = format!(
        "
{includes}

namespace proof_system::honk {{
namespace flavor {{

class {name}Flavor {{
    public: 
        using Curve = curve::BN254;
        using G1 = Curve::Group;
        using PCS = pcs::kzg::KZG<Curve>;

        using FF = G1::subgroup_field;
        using Polynomial = barretenberg::Polynomial<FF>;
        using PolynomialHandle = std::span<FF>;
        using GroupElement = G1::element;
        using Commitment = G1::affine_element;
        using CommitmentHandle = G1::affine_element;
        using CommitmentKey = pcs::CommitmentKey<Curve>;
        using VerifierCommitmentKey = pcs::VerifierCommitmentKey<Curve>;

        static constexpr size_t NUM_PRECOMPUTED_ENTITIES = {num_precomputed}; 
        static constexpr size_t NUM_WITNESS_ENTITIES = {num_witness};
        static constexpr size_t NUM_WIRES = NUM_WITNESS_ENTITIES + NUM_PRECOMPUTED_ENTITIES;
        // We have two copies of the witness entities, so we subtract the number of fixed ones (they have no shift), one for the unshifted and one for the shifted
        static constexpr size_t NUM_ALL_ENTITIES = {num_all};


        using Relations = std::tuple<{relations_tuple}>;

        static constexpr size_t MAX_PARTIAL_RELATION_LENGTH = compute_max_partial_relation_length<Relations>();

        // BATCHED_RELATION_PARTIAL_LENGTH = algebraic degree of sumcheck relation *after* multiplying by the `pow_zeta`
        // random polynomial e.g. For \\sum(x) [A(x) * B(x) + C(x)] * PowZeta(X), relation length = 2 and random relation
        // length = 3
        static constexpr size_t BATCHED_RELATION_PARTIAL_LENGTH = MAX_PARTIAL_RELATION_LENGTH + 1;
        static constexpr size_t NUM_RELATIONS = std::tuple_size<Relations>::value;

        template <size_t NUM_INSTANCES>
        using ProtogalaxyTupleOfTuplesOfUnivariates =
            decltype(create_protogalaxy_tuple_of_tuples_of_univariates<Relations, NUM_INSTANCES>());
        using SumcheckTupleOfTuplesOfUnivariates = decltype(create_sumcheck_tuple_of_tuples_of_univariates<Relations>());
        using TupleOfArraysOfValues = decltype(create_tuple_of_arrays_of_values<Relations>());
    

        static constexpr bool has_zero_row = true;

    private:
        template<typename DataType, typename HandleType>
        class PrecomputedEntities : public PrecomputedEntities_<DataType, HandleType, NUM_PRECOMPUTED_ENTITIES> {{
            public:
              {precomputed_str}

              std::vector<HandleType> get_sigma_polynomials() override {{ return {{}}; }};
              std::vector<HandleType> get_id_polynomials() override {{ return {{}}; }};
              std::vector<HandleType> get_table_polynomials() {{ return {{}}; }};
          }};
          
        template <typename DataType, typename HandleType>
        class WitnessEntities : public WitnessEntities_<DataType, HandleType, NUM_WITNESS_ENTITIES> {{
            public:

            {witness_str} 
        }};

        template <typename DataType, typename HandleType>
        class AllEntities : public AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES> {{
            public:

            {precomputed} 
            {witnesses}
            {all_shift}

            {all_entities_pointer_view}


            std::vector<HandleType> get_wires() override {{
                return {{
{all_entities_get_wires}
                }};
            }};

            std::vector<HandleType> get_unshifted() override {{
                return {{
                    {all_entities_get_unshifted}
                }};
            }};

            std::vector<HandleType> get_to_be_shifted() override {{
                return {{
                    {all_entities_get_to_be_shifted}
                }};
            }};

            std::vector<HandleType> get_shifted() override {{
                return {{
                    {all_entities_get_shifted}
                }};
            }};

        }};
    
    public:
    class ProvingKey : public ProvingKey_<PrecomputedEntities<Polynomial, PolynomialHandle>,
    WitnessEntities<Polynomial, PolynomialHandle>> {{
        public:
        // Expose constructors on the base class
        using Base = ProvingKey_<PrecomputedEntities<Polynomial, PolynomialHandle>,
        WitnessEntities<Polynomial, PolynomialHandle>>;
        using Base::Base;

        // The plookup wires that store plookup read data.
        std::array<PolynomialHandle, 0> get_table_column_wires() {{ return {{}}; }};
    }};

    using VerificationKey = VerificationKey_<PrecomputedEntities<Commitment, CommitmentHandle>>;

    using ProverPolynomials = AllEntities<PolynomialHandle, PolynomialHandle>;

    using FoldedPolynomials = AllEntities<std::vector<FF>, PolynomialHandle>;

    class AllValues : public AllEntities<FF, FF> {{
        public:
          using Base = AllEntities<FF, FF>;
          using Base::Base;
      }};
  
    class AllPolynomials : public AllEntities<Polynomial, PolynomialHandle> {{
      public:
        [[nodiscard]] size_t get_polynomial_size() const {{ return this->{first_poly}.size(); }}
        [[nodiscard]] AllValues get_row(const size_t row_idx) const
        {{
            AllValues result;
            for (auto [result_field, polynomial] : zip_view(result.pointer_view(), pointer_view())) {{
                *result_field = (*polynomial)[row_idx];
            }}
            return result;
        }}
    }};


    using RowPolynomials = AllEntities<FF, FF>;

    class PartiallyEvaluatedMultivariates : public AllEntities<Polynomial, PolynomialHandle> {{
      public:
        PartiallyEvaluatedMultivariates() = default;
        PartiallyEvaluatedMultivariates(const size_t circuit_size)
        {{
            // Storage is only needed after the first partial evaluation, hence polynomials of size (n / 2)
            for (auto* poly : pointer_view()) {{
                *poly = Polynomial(circuit_size / 2);
            }}
        }}
    }};

    /**
     * @brief A container for univariates used during Protogalaxy folding and sumcheck.
     * @details During folding and sumcheck, the prover evaluates the relations on these univariates.
     */
    template <size_t LENGTH>
    using ProverUnivariates = AllEntities<barretenberg::Univariate<FF, LENGTH>, barretenberg::Univariate<FF, LENGTH>>;

    /**
     * @brief A container for univariates produced during the hot loop in sumcheck.
     */
    using ExtendedEdges = ProverUnivariates<MAX_PARTIAL_RELATION_LENGTH>;

    {commitment_labels_class}

    {verification_commitments}

    {transcript}
}};

}} // namespace proof_system::honk::flavor
}} // namespace proof_system::honk
    
    
    "
    );
        self.flavor_hpp = Some(flavor_hpp);
    }
}

fn flavor_includes(name: &str, _relations: &[String]) -> String {
    // TODO: when there are multiple relations generated, they will need to be known in this file

    // TODO: Get the path for generated / other relations from self
    format!(
        "
#pragma once
#include \"../relation_definitions_fwd.hpp\"
#include \"barretenberg/ecc/curves/bn254/g1.hpp\"
#include \"barretenberg/commitment_schemes/kzg/kzg.hpp\"
#include \"barretenberg/polynomials/barycentric.hpp\"
#include \"barretenberg/polynomials/univariate.hpp\"

#include \"barretenberg/transcript/transcript.hpp\"
#include \"barretenberg/polynomials/evaluation_domain.hpp\"
#include \"barretenberg/polynomials/polynomial.hpp\"
#include \"barretenberg/flavor/flavor.hpp\"
#include \"barretenberg/relations/generated/{name}.hpp\"
"
    )
}

fn create_precomputed_entities(fixed: &[String]) -> String {
    let data_types = witness_get(fixed, false);
    let mut name_set = String::new();
    for name in fixed {
        let n = name.replace('.', "_");
        name_set.push_str(&format!("{n}, ", n = n));
    }

    let pointer_view = create_pointer_view("NUM_PRECOMPUTED_ENTITIES", fixed);

    format!(
        "
        {data_types}
        {pointer_view}

        std::vector<HandleType> get_selectors() override {{
            return {{ {name_set} }};
        }};
        ",
        name_set = name_set
    )
}

fn create_pointer_view(label: &str, entities: &[String]) -> String {
    let pointer_list = entities
        .iter()
        .map(|e| format!("&{}", e.replace('.', "_")))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "DEFINE_POINTER_VIEW({label}, {pointer_list})",
        label = label,
        pointer_list = pointer_list
    )
}

fn witness_get(witness: &[String], shift: bool) -> String {
    let mut return_string = String::new();
    for name in witness.iter() {
        let n = name.replace('.', "_");
        let n = if shift { format!("{}_shift", n) } else { n };
        return_string.push_str(&format!(
            "
            DataType {n};",
            n = n
        ));
    }

    format!(
        "
        {return_string}
        "
    )
}

// fn wi

// Takes in a set of wire names and outputs wrapped get_wires function
fn make_wires_set(set: &[String]) -> String {
    let mut wires = String::new();

    for name in set.iter() {
        let n = name.replace('.', "_");
        wires.push_str(&format!(
            "{n}, 
            ",
            n = n
        ));
    }
    wires
}

fn create_witness_entities(witness: &[String]) -> String {
    let data_types = witness_get(witness, false);
    let get_wires = make_wires_set(witness);
    let pointer_view = create_pointer_view("NUM_WITNESS_ENTITIES", witness);

    format!(
        "
        {data_types}

        {pointer_view}

        std::vector<HandleType> get_wires() override {{
            return {{ 
{get_wires}
            }};
        }};

        std::vector<HandleType> get_sorted_polynomials()  {{ return {{}}; }};
        "
    )
}

fn create_labels(all_ents: &[String]) -> String {
    let mut labels = String::new();
    for name in all_ents {
        let n = name.replace('.', "_");
        labels.push_str(&format!(
            "Base::{n} = \"{n}\"; 
            ",
            n = n
        ));
    }
    labels
}

fn create_commitment_labels(all_ents: &[String]) -> String {
    let labels = create_labels(all_ents);

    format!(
        "
        class CommitmentLabels: public AllEntities<std::string, std::string> {{
            private:
                using Base = AllEntities<std::string, std::string>;


            public:
                CommitmentLabels() : AllEntities<std::string, std::string>()
            {{
                {labels}
            }};
        }};
        "
    )
}

fn create_key_dereference(fixed: &[String]) -> String {
    fixed
        .iter()
        .map(|f| {
            let name = f.replace('.', "_");
            format!("{name} = verification_key->{name};")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn create_verifier_commitments(fixed: &[String]) -> String {
    let key_dereference = create_key_dereference(fixed);

    format!(
        "
    class VerifierCommitments : public AllEntities<Commitment, CommitmentHandle> {{
      private:
        using Base = AllEntities<Commitment, CommitmentHandle>;

      public:
        VerifierCommitments(const std::shared_ptr<VerificationKey>& verification_key,
                            const BaseTranscript<FF>& transcript)
        {{
            static_cast<void>(transcript);
            {key_dereference}
        }}
    }};
"
    )
}

fn generate_transcript(witness: &[String]) -> String {
    let declarations = witness
        .iter()
        .map(|f| format!("Commitment {};", f.replace('.', "_")))
        .collect::<Vec<_>>()
        .join("\n");
    let deserialize_wires = witness
        .iter()
        .map(|f| {
            format!(
                "{} = deserialize_from_buffer<Commitment>(BaseTranscript<FF>::proof_data, num_bytes_read);",
                f.replace('.', "_")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let serialize_wires = witness
        .iter()
        .map(|f| {
            format!(
                "serialize_to_buffer<Commitment>({}, BaseTranscript<FF>::proof_data);",
                f.replace('.', "_")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!("
    class Transcript : public BaseTranscript<FF> {{
      public:
        uint32_t circuit_size;

        {declarations}

        std::vector<barretenberg::Univariate<FF, BATCHED_RELATION_PARTIAL_LENGTH>> sumcheck_univariates;
        std::array<FF, NUM_ALL_ENTITIES> sumcheck_evaluations;
        std::vector<Commitment> zm_cq_comms;
        Commitment zm_cq_comm;
        Commitment zm_pi_comm;

        Transcript() = default;

        Transcript(const std::vector<uint8_t>& proof)
            : BaseTranscript<FF>(proof)
        {{}}

        void deserialize_full_transcript() override
        {{
            size_t num_bytes_read = 0;
            circuit_size = deserialize_from_buffer<uint32_t>(proof_data, num_bytes_read);
            size_t log_n = numeric::get_msb(circuit_size);

            {deserialize_wires}

            for (size_t i = 0; i < log_n; ++i) {{
                sumcheck_univariates.emplace_back(
                    deserialize_from_buffer<barretenberg::Univariate<FF, BATCHED_RELATION_PARTIAL_LENGTH>>(
                        BaseTranscript<FF>::proof_data, num_bytes_read));
            }}
            sumcheck_evaluations = deserialize_from_buffer<std::array<FF, NUM_ALL_ENTITIES>>(
                BaseTranscript<FF>::proof_data, num_bytes_read);
            for (size_t i = 0; i < log_n; ++i) {{
                zm_cq_comms.push_back(deserialize_from_buffer<Commitment>(proof_data, num_bytes_read));
            }}
            zm_cq_comm = deserialize_from_buffer<Commitment>(proof_data, num_bytes_read);
            zm_pi_comm = deserialize_from_buffer<Commitment>(proof_data, num_bytes_read);
        }}

        void serialize_full_transcript() override
        {{
            size_t old_proof_length = proof_data.size();
            BaseTranscript<FF>::proof_data.clear();
            size_t log_n = numeric::get_msb(circuit_size);

            serialize_to_buffer(circuit_size, BaseTranscript<FF>::proof_data);

            {serialize_wires}

            for (size_t i = 0; i < log_n; ++i) {{
                serialize_to_buffer(sumcheck_univariates[i], BaseTranscript<FF>::proof_data);
            }}
            serialize_to_buffer(sumcheck_evaluations, BaseTranscript<FF>::proof_data);
            for (size_t i = 0; i < log_n; ++i) {{
                serialize_to_buffer(zm_cq_comms[i], proof_data);
            }}
            serialize_to_buffer(zm_cq_comm, proof_data);
            serialize_to_buffer(zm_pi_comm, proof_data);

            // sanity check to make sure we generate the same length of proof as before.
            ASSERT(proof_data.size() == old_proof_length);
        }}
    }};
    ")
}
