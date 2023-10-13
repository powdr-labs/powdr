

#pragma once
#include "barretenberg/ecc/curves/bn254/g1.hpp"
#include "barretenberg/honk/pcs/kzg/kzg.hpp"
#include "barretenberg/polynomials/barycentric.hpp"
#include "barretenberg/polynomials/univariate.hpp"

#include "barretenberg/honk/transcript/transcript.hpp"
#include "barretenberg/polynomials/evaluation_domain.hpp"
#include "barretenberg/polynomials/polynomial.hpp"
// #include "barretenberg/proof_system/circuit_builder/ultra_circuit_builder.hpp"
#include "barretenberg/proof_system/flavor/flavor.hpp"
#include "barretenberg/proof_system/relations/ExampleRelation_relation.hpp"


namespace proof_system::honk {
namespace flavor {

template <typename CycleGroup_T, typename Curve_T, typename PCS_T> class ExampleRelationFlavorBase {
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

        static constexpr size_t NUM_WIRES = 3;
        static constexpr size_t NUM_ALL_ENTITIES = 5;
        static constexpr size_t NUM_PRECOMPUTED_ENTITIES = 1;
        static constexpr size_t NUM_WITNESS_ENTITIES = 2;

        // using GrandProductRelations = std::tuple<>;
        using Relations = std::tuple<ExampleRelation>;
        // using LookupRelation = sumcheck::LookupRelation<FF>;

        static constexpr size_t NUM_RELATION_LENGTH  = get_max_relation_length<Relations>();
        static constexpr size_t NUM_RANDOM_RELATION_LENGTH = MAX_RELATION_LENGTH + 1;
        static constexpr size_t NUM_RELATIONS = std::tuple_size_v<Relations>::value;

        // define the containers for storing the contributions from each relation in Sumcheck
        using RelationUnivariates = decltype(create_relation_univariates_container<FF, Relations>());
        using RelationValues = decltype(create_relation_values_container<FF, Relations>());

    private:
        template<typename DataType, typename HandleType>
        class PrecomputedEntities : public PrecomputedEntities_<DataType, HandleType, NUM_PRECOMPUTED_ENTITIES> {
            public:

            
        
            DataType& Fibonacci_ISLAST = std::get<0>(this->_data);
        
            
        std::vector<HandleType> get_selectors() override {
            return { Fibonacci_ISLAST,  };
        };
        
      
              std::vector<HandleType> get_sigma_polynomials() override { return {}; };
              std::vector<HandleType> get_id_polynomials() override { return {}; };
              std::vector<HandleType> get_table_polynomials() { return {}; };
          };
          
        template <typename DataType, typename HandleType>
        class WitnessEntities : public WitnessEntities_<DataType, HandleType, NUM_WITNESS_ENTITIES> {
            public:

            
        
        
            DataType& Fibonacci_x = std::get<0>(this->_data);
            DataType& Fibonacci_y = std::get<1>(this->_data);
        

        std::vector<HandleType> get_wires() override {
            return { 
Fibonacci_x, 
            Fibonacci_y, 
            
            };
        };

        std::vector<HandleType> get_sorted_polynomials()  { return {}; };
         
        }

        template <typename DataType, typename HandleType>
        class AllEntities : public AllEntities_<DataType, HandleType, NUM_WITNESS_ENTITIES> {
            public:

            
        
            DataType& Fibonacci_ISLAST = std::get<0>(this->_data);
         
            
        
            DataType& Fibonacci_x = std::get<1>(this->_data);
            DataType& Fibonacci_y = std::get<2>(this->_data);
        
            
        
            DataType& Fibonacci_x_shift = std::get<3>(this->_data);
            DataType& Fibonacci_y_shift = std::get<4>(this->_data);
        


            std::vector<HandleType> get_wires() override {
                return {
Fibonacci_ISLAST, 
            Fibonacci_x, 
            Fibonacci_y, 
            
                };
            };

            std::vector<HandleType> get_unshifted() override {
                return {
                    Fibonacci_ISLAST, 
            
                };
            };

            std::vector<HandleType> get_to_be_shifted() override {
                return {
                    Fibonacci_x, 
            Fibonacci_y, 
            
                };
            };

            std::vector<HandleType> get_shifted() override {
                return {
                    Fibonacci_x_shift, 
            Fibonacci_y_shift, 
            
                };
            };

            AllEntities() = default;

            AllEntities(const AllEntities& other)
                : AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>(other){};
    
            AllEntities(AllEntities&& other) noexcept
                : AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>(other){};
    
            AllEntities& operator=(const AllEntities& other)
            {
                if (this == &other) {
                    return *this;
                }
                AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>::operator=(other);
                return *this;
            }
    
            AllEntities& operator=(AllEntities&& other) noexcept
            {
                AllEntities_<DataType, HandleType, NUM_ALL_ENTITIES>::operator=(other);
                return *this;
            }
    
            ~AllEntities() override = default;
        };
    
    public:
    class ProvingKey : public ProvingKey_<PrecomputedEntities<Polynomial, PolynomialHandle>,
    WitnessEntities<Polynomial, PolynomialHandle>> {
        public:
        // Expose constructors on the base class
        using Base = ProvingKey_<PrecomputedEntities<Polynomial, PolynomialHandle>,
        WitnessEntities<Polynomial, PolynomialHandle>>;
        using Base::Base;

        // The plookup wires that store plookup read data.
        std::array<PolynomialHandle, 0> get_table_column_wires() { return {}; };
    };

    using VerificationKey = VerificationKey_<PrecomputedEntities<Commitment, CommitmentHandle>>;

    using ProverPolynomials = AllEntities<PolynomialHandle, PolynomialHandle>;

    using FoldedPolynomials = AllEntities<std::vector<FF>, PolynomialHandle>;

    using RawPolynomials = AllEntities<Polynomial, PolynomialHandle>;

    using RowPolynomials = AllEntities<FF, FF>;

    class PartiallyEvaluatedMultivariates : public AllEntities<Polynomial, PolynomialHandle> {
        public:
        PartiallyEvaluatedMultivariates() = default;
        PartiallyEvaluatedMultivariates(const size_t circuit_size)
        {
            // Storage is only needed after the first partial evaluation, hence polynomials of size (n / 2)
            for (auto& poly : this->_data) {
                poly = Polynomial(circuit_size / 2);
            }
        }
    };

    template <size_t MAX_RELATION_LENGTH>
    using ExtendedEdges = AllEntities<barretenberg::Univariate<FF, MAX_RELATION_LENGTH>,
    barretenberg::Univariate<FF, MAX_RELATION_LENGTH>>;

    class ClaimedEvaluations : public AllEntities<FF, FF> {
        public:
            using Base = AllEntities<FF, FF>;
            using Base::Base;
            ClaimedEvaluations(std::array<FF, NUM_ALL_ENTITIES> _data_in) { this->_data = _data_in; }
    };

    
        class CommitmentLabels: public AllEntities<std::string, std::string> {
            private:
                using Base = AllEntities<std::string, std::string>;


            public:
                CommitmentLabels() : AllEntities<std::string, std::string>()
            {
                Base::Fibonacci_ISLAST = "Fibonacci_ISLAST", 
            Base::Fibonacci_x = "Fibonacci_x", 
            Base::Fibonacci_y = "Fibonacci_y", 
            
            };
        };
        

    
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

};

class ExampleRelationFlavor : public ExampleRelationFlavorBase<grumpkin::g1, curve::BN254, pcs::kzg::KZG<curve::BN254>> {};

} // namespace proof_system::honk::flavor
} // namespace proof_system::honk
    
    
    