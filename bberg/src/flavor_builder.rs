use crate::file_writer::BBFiles;

pub trait FlavorBuilder {
    fn create_flavor_hpp(
        &mut self,
        name: &str,
        relations: &[String],
        all_cols: &[String],
        shifted: &[String],
    );
}

/// Build the boilerplate for the flavor file
impl FlavorBuilder for BBFiles {
    fn create_flavor_hpp(
        &mut self,
        name: &str,
        relations: &[String],
        all_cols: &[String],
        shifted: &[String],
        // shifted: &[String],
    ) {
        let includes = flavor_includes(name, relations);
        let num_witness = all_cols.len();
        let num_all = num_witness + shifted.len();
        // Note: includes all witness shifts
        // TODO: for now we include a shift OF ALL witness wires, however this is not necessarily true

        let precomputed = witness_get(all_cols, 0, false);
        let witness_str = create_witness_entities(all_cols);
        let all_shift = witness_get(shifted, num_witness, true);

        let all_entities_get_wires = make_wires_set(
            &[all_cols.to_vec(), shifted.to_vec()]
                .into_iter()
                .flatten()
                .collect::<Vec<String>>(),
        );
        let all_entities_get_unshifted = make_wires_set(all_cols);
        let all_entities_get_to_be_shifted = make_wires_set(shifted);
        let all_entities_get_shifted = make_wires_set(
            &shifted
                .iter()
                .map(|w| format!("{}_shift", w))
                .collect::<Vec<String>>(),
        );

        let commitment_labels_class = create_commitment_labels(all_cols);

        let verification_commitments = create_verifier_commitments();

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

template <typename CycleGroup_T, typename Curve_T, typename PCS_T> class {name}FlavorBase {{
    public: 
        // forward template params into the ECCVMBase namespace
        using CycleGroup = CycleGroup_T;
        using Curve = Curve_T;
        using G1 = typename Curve::Group;
        using PCS = PCS_T;

        using FF = typename G1::subgroup_field;
        using Polynomial = barretenberg::Polynomial<FF>;
        using PolynomialHandle = std::span<FF>;
        using GroupElement = typename G1::element;
        using Commitment = typename G1::affine_element;
        using CommitmentHandle = typename G1::affine_element;
        using CommitmentKey = pcs::CommitmentKey<Curve>;
        using VerifierCommitmentKey = pcs::VerifierCommitmentKey<Curve>;

        static constexpr size_t NUM_WIRES = {num_witness};
        static constexpr size_t NUM_PRECOMPUTED_ENTITIES = 0; // This is zero for now
        static constexpr size_t NUM_WITNESS_ENTITIES = {num_witness};
        // We have two copies of the witness entities, so we subtract the number of fixed ones (they have no shift), one for the unshifted and one for the shifted
        static constexpr size_t NUM_ALL_ENTITIES = {num_all};


        // using GrandProductRelations = std::tuple<>;
        using Relations = std::tuple<{relations_tuple}>;
        // using LookupRelation = sumcheck::LookupRelation<FF>;

        static constexpr size_t MAX_RELATION_LENGTH  = get_max_relation_length<Relations>();
        static constexpr size_t MAX_RANDOM_RELATION_LENGTH = MAX_RELATION_LENGTH + 1;
        static constexpr size_t NUM_RELATIONS = std::tuple_size<Relations>::value;

        // define the containers for storing the contributions from each relation in Sumcheck
        using TupleOfTuplesOfUnivariates = decltype(create_relation_univariates_container<FF, Relations>());
        using TupleOfArraysOfValues = decltype(create_relation_values_container<FF, Relations>());

    private:
        template<typename DataType, typename HandleType>
        class PrecomputedEntities : public PrecomputedEntities_<DataType, HandleType, NUM_PRECOMPUTED_ENTITIES> {{
            public:

              std::vector<HandleType> get_selectors() override {{ return {{}}; }};
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
            {all_shift}


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

            AllEntities() = default;

            AllEntities(const AllEntities& other)
                : AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>(other){{}};
    
            AllEntities(AllEntities&& other) noexcept
                : AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>(other){{}};
    
            AllEntities& operator=(const AllEntities& other)
            {{
                if (this == &other) {{
                    return *this;
                }}
                AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>::operator=(other);
                return *this;
            }}
    
            AllEntities& operator=(AllEntities&& other) noexcept
            {{
                AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>::operator=(other);
                return *this;
            }}
    
            ~AllEntities() override = default;
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
          AllValues(std::array<FF, NUM_ALL_ENTITIES> _data_in) {{ this->_data = _data_in; }}
      }};
  
    class AllPolynomials : public AllEntities<Polynomial, PolynomialHandle> {{
      public:
        AllValues get_row(const size_t row_idx) const
        {{
            AllValues result;
            size_t column_idx = 0; // // TODO(https://github.com/AztecProtocol/barretenberg/issues/391) zip
            for (auto& column : this->_data) {{
                result[column_idx] = column[row_idx];
                column_idx++;
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
            for (auto& poly : this->_data) {{
                poly = Polynomial(circuit_size / 2);
            }}
        }}
    }};

    template <size_t MAX_RELATION_LENGTH>
    using ExtendedEdges = AllEntities<barretenberg::Univariate<FF, MAX_RELATION_LENGTH>,
    barretenberg::Univariate<FF, MAX_RELATION_LENGTH>>;

    class ClaimedEvaluations : public AllEntities<FF, FF> {{
        public:
            using Base = AllEntities<FF, FF>;
            using Base::Base;
            ClaimedEvaluations(std::array<FF, NUM_ALL_ENTITIES> _data_in) {{ this->_data = _data_in; }}
    }};

    {commitment_labels_class}

    {verification_commitments}
}};

class {name}Flavor : public {name}FlavorBase<grumpkin::g1, curve::BN254, pcs::kzg::KZG<curve::BN254>> {{}};

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
#include \"barretenberg/ecc/curves/bn254/g1.hpp\"
#include \"barretenberg/honk/pcs/kzg/kzg.hpp\"
#include \"barretenberg/polynomials/barycentric.hpp\"
#include \"barretenberg/polynomials/univariate.hpp\"

#include \"barretenberg/honk/transcript/transcript.hpp\"
#include \"barretenberg/polynomials/evaluation_domain.hpp\"
#include \"barretenberg/polynomials/polynomial.hpp\"
// #include \"barretenberg/proof_system/circuit_builder/ultra_circuit_builder.hpp\"
#include \"barretenberg/proof_system/flavor/flavor.hpp\"
#include \"barretenberg/proof_system/relations/generated/{name}.hpp\"
"
    )
}

fn create_precomputed_entities(fixed: &[String]) -> String {
    let mut name_set = String::new();
    for name in fixed {
        let n = name.replace('.', "_");
        name_set.push_str(&format!("{n}, ", n = n));
    }

    let get_selectors = format!(
        "
        std::vector<HandleType> get_selectors() override {{
            return {{ {name_set} }};
        }};
        ",
        name_set = name_set
    );

    get_selectors
}

fn witness_get(witness: &[String], offset: usize, shift: bool) -> String {
    let mut return_string = String::new();
    for (i, name) in witness.iter().enumerate() {
        let n = name.replace('.', "_");
        let n = if shift { format!("{}_shift", n) } else { n };
        let index = i + offset;
        return_string.push_str(&format!(
            "
            DataType& {n} = std::get<{index}>(this->_data);",
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
    let data_types = witness_get(witness, 0, false);
    let get_wires = make_wires_set(witness);

    format!(
        "
        {data_types}

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

fn create_verifier_commitments() -> &'static str {
    r#"
    class VerifierCommitments : public AllEntities<Commitment, CommitmentHandle> {
      private:
        using Base = AllEntities<Commitment, CommitmentHandle>;

      public:
        VerifierCommitments(const std::shared_ptr<VerificationKey>& verification_key,
                            const VerifierTranscript<FF>& transcript)
        {
            static_cast<void>(transcript);
            static_cast<void>(verification_key);
        }
    };
"#
}
