use std::collections::BTreeMap;
use std::{collections::HashSet, sync::Arc};

use crate::air_builder::AirKeygenBuilder;
use crate::{BabyBearSC, IntoOpenVm};
use eyre::Result;
use itertools::Itertools;
use openvm_algebra_circuit::{Fp2ExtensionPeriphery, ModularExtensionPeriphery};
use openvm_circuit::arch::{VmChipComplex, VmConfig};
use openvm_circuit::system::phantom::PhantomChip;
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_ecc_circuit::WeierstrassExtensionPeriphery;
use openvm_instructions::VmOpcode;
use openvm_keccak256_circuit::Keccak256Periphery;
use openvm_native_circuit::CastFExtensionPeriphery;
use openvm_pairing_circuit::PairingExtensionPeriphery;
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery};
use openvm_stark_backend::{
    air_builders::symbolic::SymbolicConstraints, config::StarkGenericConfig, rap::AnyRap, Chip,
};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{config_from_perm, default_perm},
    fri_params::SecurityParameters,
};
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear;
use powdr_autoprecompiles::bus_map::{BusMap, BusType};
use powdr_autoprecompiles::SymbolicMachine;

use crate::utils::get_pil;

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;

fn get_bus_map<F: PrimeField32>(
    chip_complex: &VmChipComplex<F, SdkVmConfigExecutor<F>, SdkVmConfigPeriphery<F>>,
) -> BusMap {
    #[derive(Debug)]
    enum ExtractionError {
        NoBus,
        #[allow(dead_code)]
        Unimplemented(&'static str),
    }

    // Define how to extract bus information from the chips we know
    fn extract_bitwise_lookup_bus(
        chip: &SharedBitwiseOperationLookupChip<8>,
    ) -> Result<(u64, BusType), ExtractionError> {
        Ok((chip.bus().inner.index as u64, BusType::BitwiseLookup))
    }
    fn extract_range_tuple_checker_bus(
        chip: &SharedRangeTupleCheckerChip<2>,
    ) -> Result<(u64, BusType), ExtractionError> {
        Ok((chip.bus().inner.index as u64, BusType::TupleRangeChecker))
    }
    fn extract_phantom_bus<G>(_: &PhantomChip<G>) -> Result<(u64, BusType), ExtractionError> {
        Err(ExtractionError::NoBus)
    }
    fn extract_native_poseidon2_bus<G: PrimeField32>(
        _: &openvm_native_circuit::chip::NativePoseidon2Chip<G, 1>,
    ) -> Result<(u64, BusType), ExtractionError> {
        Err(ExtractionError::Unimplemented(
            "NativePoseidon2Chip does not expose its PermutationBus",
        ))
    }
    fn extract_sha256_bus<G: PrimeField32>(
        _: &openvm_sha256_circuit::Sha256VmChip<G>,
    ) -> Result<(u64, BusType), ExtractionError> {
        Err(ExtractionError::Unimplemented(
            "Sha256VmChip does not expose its PermutationBus",
        ))
    }
    fn extract_rv32_hint_store_bus<G: PrimeField32>(
        _: &openvm_rv32im_circuit::Rv32HintStoreChip<G>,
    ) -> Result<(u64, BusType), ExtractionError> {
        Err(ExtractionError::Unimplemented(
            "Rv32HintStoreChip does not expose its PermutationBus",
        ))
    }
    fn extract_keccak256_bus<G: PrimeField32>(
        c: &openvm_keccak256_circuit::KeccakVmChip<G>,
    ) -> Result<(u64, BusType), ExtractionError> {
        extract_bitwise_lookup_bus(&c.bitwise_lookup_chip)
    }

    BusMap::from_id_type_pairs(
        {
            let base = &chip_complex.base;
            [
                (
                    base.execution_bus().inner.index as u64,
                    BusType::ExecutionBridge,
                ),
                (base.memory_bus().inner.index as u64, BusType::Memory),
                (base.program_bus().inner.index as u64, BusType::PcLookup),
                (
                    base.range_checker_bus().inner.index as u64,
                    BusType::VariableRangeChecker,
                ),
            ]
        }
        .into_iter()
        .chain(
            chip_complex
                .inventory
                .periphery()
                .iter()
                .map(|chip| match chip {
                    SdkVmConfigPeriphery::System(system_periphery) => match system_periphery {
                        openvm_circuit::arch::SystemPeriphery::Poseidon2(_) => {
                            Err(ExtractionError::Unimplemented(
                                "Poseidon2PeripheryAir does not expose its LookupBus",
                            ))
                        }
                    },
                    SdkVmConfigPeriphery::Rv32i(rv32_i_periphery) => match rv32_i_periphery {
                        openvm_rv32im_circuit::Rv32IPeriphery::BitwiseOperationLookup(
                            shared_bitwise_operation_lookup_chip,
                        ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                        openvm_rv32im_circuit::Rv32IPeriphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::Io(rv32_io_periphery) => match rv32_io_periphery {
                        openvm_rv32im_circuit::Rv32IoPeriphery::BitwiseOperationLookup(
                            shared_bitwise_operation_lookup_chip,
                        ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                        openvm_rv32im_circuit::Rv32IoPeriphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::Keccak(keccak256_periphery) => {
                        match keccak256_periphery {
                            Keccak256Periphery::BitwiseOperationLookup(
                                shared_bitwise_operation_lookup_chip,
                            ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                            Keccak256Periphery::Phantom(phantom_chip) => {
                                extract_phantom_bus(phantom_chip)
                            }
                        }
                    }
                    SdkVmConfigPeriphery::Sha256(sha256_periphery) => match sha256_periphery {
                        openvm_sha256_circuit::Sha256Periphery::BitwiseOperationLookup(
                            shared_bitwise_operation_lookup_chip,
                        ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                        openvm_sha256_circuit::Sha256Periphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::Native(native_periphery) => match native_periphery {
                        openvm_native_circuit::NativePeriphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::Rv32m(rv32_m_periphery) => match rv32_m_periphery {
                        openvm_rv32im_circuit::Rv32MPeriphery::BitwiseOperationLookup(
                            shared_bitwise_operation_lookup_chip,
                        ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                        openvm_rv32im_circuit::Rv32MPeriphery::RangeTupleChecker(
                            shared_range_tuple_checker_chip,
                        ) => extract_range_tuple_checker_bus(shared_range_tuple_checker_chip),
                        openvm_rv32im_circuit::Rv32MPeriphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::BigInt(int256_periphery) => match int256_periphery {
                        openvm_bigint_circuit::Int256Periphery::BitwiseOperationLookup(
                            shared_bitwise_operation_lookup_chip,
                        ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                        openvm_bigint_circuit::Int256Periphery::RangeTupleChecker(
                            range_tuple_checker_chip,
                        ) => extract_range_tuple_checker_bus(range_tuple_checker_chip),
                        openvm_bigint_circuit::Int256Periphery::Phantom(phantom_chip) => {
                            extract_phantom_bus(phantom_chip)
                        }
                    },
                    SdkVmConfigPeriphery::Modular(modular_extension_periphery) => {
                        match modular_extension_periphery {
                            ModularExtensionPeriphery::BitwiseOperationLookup(
                                shared_bitwise_operation_lookup_chip,
                            ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                            ModularExtensionPeriphery::Phantom(phantom_chip) => {
                                extract_phantom_bus(phantom_chip)
                            }
                        }
                    }
                    SdkVmConfigPeriphery::Fp2(fp2_extension_periphery) => {
                        match fp2_extension_periphery {
                            Fp2ExtensionPeriphery::BitwiseOperationLookup(
                                shared_bitwise_operation_lookup_chip,
                            ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                            Fp2ExtensionPeriphery::Phantom(phantom_chip) => {
                                extract_phantom_bus(phantom_chip)
                            }
                        }
                    }
                    SdkVmConfigPeriphery::Pairing(pairing_extension_periphery) => {
                        match pairing_extension_periphery {
                            PairingExtensionPeriphery::BitwiseOperationLookup(
                                shared_bitwise_operation_lookup_chip,
                            ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                            PairingExtensionPeriphery::Phantom(phantom_chip) => {
                                extract_phantom_bus(phantom_chip)
                            }
                        }
                    }
                    SdkVmConfigPeriphery::Ecc(weierstrass_extension_periphery) => {
                        match weierstrass_extension_periphery {
                            WeierstrassExtensionPeriphery::BitwiseOperationLookup(
                                shared_bitwise_operation_lookup_chip,
                            ) => extract_bitwise_lookup_bus(shared_bitwise_operation_lookup_chip),
                            WeierstrassExtensionPeriphery::Phantom(phantom_chip) => {
                                extract_phantom_bus(phantom_chip)
                            }
                        }
                    }
                    SdkVmConfigPeriphery::CastF(cast_fextension_periphery) => {
                        match cast_fextension_periphery {
                            CastFExtensionPeriphery::Placeholder(_) => {
                                Err(ExtractionError::Unimplemented(
                                    "Unsure how to handle Placeholder CastFExtensionPeriphery",
                                ))
                            }
                        }
                    }
                }).chain(
                    chip_complex
                        .inventory
                        .executors()
                        .iter()
                        .map(|executor| match executor{
                            SdkVmConfigExecutor::System(system_executor) => match system_executor {
                                openvm_circuit::arch::SystemExecutor::PublicValues(_) => Err(ExtractionError::NoBus),
                                openvm_circuit::arch::SystemExecutor::Phantom(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Rv32i(rv32_iexecutor) => match rv32_iexecutor {
                                openvm_rv32im_circuit::Rv32IExecutor::BaseAlu(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::LessThan(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::Shift(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::LoadStore(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::LoadSignExtend(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::BranchEqual(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::BranchLessThan(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::JalLui(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::Jalr(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32IExecutor::Auipc(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Io(rv32_io_executor) => match rv32_io_executor {
                                openvm_rv32im_circuit::Rv32IoExecutor::HintStore(rv32_hint_store_chip) => extract_rv32_hint_store_bus(rv32_hint_store_chip),
                            },
                            SdkVmConfigExecutor::Keccak(keccak256_executor) => match keccak256_executor {
                                openvm_keccak256_circuit::Keccak256Executor::Keccak256(keccak_vm_chip) => extract_keccak256_bus(keccak_vm_chip),
                            },
                            SdkVmConfigExecutor::Sha256(sha256_executor) => match sha256_executor {
                                openvm_sha256_circuit::Sha256Executor::Sha256(sha256_vm_chip) => extract_sha256_bus(sha256_vm_chip),
                            },
                            SdkVmConfigExecutor::Native(native_executor) => match native_executor {
                                openvm_native_circuit::NativeExecutor::LoadStore(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::BlockLoadStore(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::BranchEqual(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::Jal(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::FieldArithmetic(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::FieldExtension(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::FriReducedOpening(_) => Err(ExtractionError::NoBus),
                                openvm_native_circuit::NativeExecutor::VerifyBatch(native_poseidon2_chip) => extract_native_poseidon2_bus(native_poseidon2_chip),
                            },
                            SdkVmConfigExecutor::Rv32m(rv32_mexecutor) => match rv32_mexecutor {
                                openvm_rv32im_circuit::Rv32MExecutor::Multiplication(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32MExecutor::MultiplicationHigh(_) => Err(ExtractionError::NoBus),
                                openvm_rv32im_circuit::Rv32MExecutor::DivRem(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::BigInt(int256_executor) => match int256_executor {
                                openvm_bigint_circuit::Int256Executor::BaseAlu256(_) => Err(ExtractionError::NoBus),
                                openvm_bigint_circuit::Int256Executor::LessThan256(_) => Err(ExtractionError::NoBus),
                                openvm_bigint_circuit::Int256Executor::BranchEqual256(_) => Err(ExtractionError::NoBus),
                                openvm_bigint_circuit::Int256Executor::BranchLessThan256(_) => Err(ExtractionError::NoBus),
                                openvm_bigint_circuit::Int256Executor::Multiplication256(_) => Err(ExtractionError::NoBus),
                                openvm_bigint_circuit::Int256Executor::Shift256(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Modular(modular_extension_executor) => match modular_extension_executor {
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularAddSubRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularMulDivRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularIsEqualRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularAddSubRv32_48(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularMulDivRv32_48(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::ModularExtensionExecutor::ModularIsEqualRv32_48(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Fp2(fp2_extension_executor) => match fp2_extension_executor {
                                openvm_algebra_circuit::Fp2ExtensionExecutor::Fp2AddSubRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::Fp2ExtensionExecutor::Fp2MulDivRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::Fp2ExtensionExecutor::Fp2AddSubRv32_48(_) => Err(ExtractionError::NoBus),
                                openvm_algebra_circuit::Fp2ExtensionExecutor::Fp2MulDivRv32_48(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Pairing(pairing_extension_executor) => match pairing_extension_executor {
                                openvm_pairing_circuit::PairingExtensionExecutor::MillerDoubleAndAddStepRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_pairing_circuit::PairingExtensionExecutor::EvaluateLineRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_pairing_circuit::PairingExtensionExecutor::MillerDoubleAndAddStepRv32_48(_) => Err(ExtractionError::NoBus),
                                openvm_pairing_circuit::PairingExtensionExecutor::EvaluateLineRv32_48(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::Ecc(weierstrass_extension_executor) => match weierstrass_extension_executor {
                                openvm_ecc_circuit::WeierstrassExtensionExecutor::EcAddNeRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_ecc_circuit::WeierstrassExtensionExecutor::EcDoubleRv32_32(_) => Err(ExtractionError::NoBus),
                                openvm_ecc_circuit::WeierstrassExtensionExecutor::EcAddNeRv32_48(_) => Err(ExtractionError::NoBus),
                                openvm_ecc_circuit::WeierstrassExtensionExecutor::EcDoubleRv32_48(_) => Err(ExtractionError::NoBus),
                            },
                            SdkVmConfigExecutor::CastF(cast_fextension_executor) => match cast_fextension_executor {
                                openvm_native_circuit::CastFExtensionExecutor::CastF(_) => Err(ExtractionError::NoBus),
                            },
                        }),
                )
                .inspect(|r| println!("{r:?}"))
                // silently ignore all extraction errors
                .filter_map(Result::ok),
        ),
    )
}

pub fn get_airs_and_bus_map<P: IntoOpenVm>(
    vm_config: SdkVmConfig,
    used_instructions: &HashSet<VmOpcode>,
) -> (BTreeMap<usize, SymbolicMachine<P>>, BusMap) {
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    let bus_map = get_bus_map(&chip_complex);

    // Note that we could use chip_complex.inventory.available_opcodes() instead of used_instructions,
    // which depends on the program being executed. But this turns out to be heavy on memory, because
    // it includes large precompiles like Keccak.
    (
        used_instructions
            .iter()
            .filter_map(|op| {
                chip_complex.inventory.get_executor(*op).map(|executor| {
                    let air = executor.air();

                    let columns = get_columns(air.clone());

                    let constraints = get_constraints(air);

                    let powdr_exprs = constraints
                        .constraints
                        .iter()
                        .map(|expr| symbolic_to_algebraic(expr, &columns).into())
                        .collect::<Vec<_>>();

                    let powdr_bus_interactions = constraints
                        .interactions
                        .iter()
                        .map(|expr| openvm_bus_interaction_to_powdr(expr, &columns))
                        .collect();

                    let symb_machine = SymbolicMachine {
                        constraints: powdr_exprs,
                        bus_interactions: powdr_bus_interactions,
                    };

                    (op.as_usize(), symb_machine)
                })
            })
            .collect(),
        bus_map,
    )
}

pub fn export_pil<VC: VmConfig<p3_baby_bear::BabyBear>>(
    vm_config: VC,
    path: &str,
    max_width: usize,
    bus_map: &BusMap,
) where
    VC::Executor: Chip<BabyBearSC>,
    VC::Periphery: Chip<BabyBearSC>,
{
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    let pil = chip_complex
        .inventory
        .executors()
        .iter()
        .filter_map(|executor| {
            let air = executor.air();
            let width = air.width();
            let name = air.name();

            if width > max_width {
                log::warn!("Skipping {name} (width: {width})");
                return None;
            }

            let columns = get_columns(air.clone());

            let constraints = get_constraints(air);

            Some(get_pil(&name, &constraints, &columns, vec![], bus_map))
        })
        .join("\n\n\n");

    println!("Writing PIL...");
    std::fs::write(path, pil).unwrap();
    println!("Exported PIL to {path}");
}

fn get_columns(air: Arc<dyn AnyRap<BabyBearSC>>) -> Vec<String> {
    let width = air.width();
    air.columns()
        .inspect(|columns| {
            assert_eq!(columns.len(), width);
        })
        .unwrap_or_else(|| (0..width).map(|i| format!("unknown_{i}")).collect())
}

pub fn get_constraints(
    air: Arc<dyn AnyRap<BabyBearSC>>,
) -> SymbolicConstraints<p3_baby_bear::BabyBear> {
    let perm = default_perm();
    let security_params = SecurityParameters::standard_fast();
    let config = config_from_perm(&perm, security_params);
    let air_keygen_builder = AirKeygenBuilder::new(config.pcs(), air);
    let builder = air_keygen_builder.get_symbolic_builder(None);
    builder.constraints()
}

#[cfg(test)]
mod tests {
    use crate::OpenVmField;

    use super::*;
    use openvm_algebra_circuit::{Fp2Extension, ModularExtension};
    use openvm_bigint_circuit::Int256;
    use openvm_circuit::arch::SystemConfig;
    use openvm_ecc_circuit::{WeierstrassExtension, SECP256K1_CONFIG};
    use openvm_pairing_circuit::{PairingCurve, PairingExtension};
    use openvm_rv32im_circuit::Rv32M;
    use powdr_number::BabyBearField;

    #[test]
    fn test_get_bus_map() {
        // Adapted from openvm-reth-benchmark for a config which has a lot of extensions

        let app_log_blowup = 2;
        let use_kzg_intrinsics = true;

        let system_config = SystemConfig::default()
            .with_continuations()
            .with_max_constraint_degree((1 << app_log_blowup) + 1)
            .with_public_values(32);
        let int256 = Int256::default();
        let bn_config = PairingCurve::Bn254.curve_config();
        let bls_config = PairingCurve::Bls12_381.curve_config();
        // The builder will do this automatically, but we set it just in case.
        let rv32m = Rv32M {
            range_tuple_checker_sizes: int256.range_tuple_checker_sizes,
        };
        let mut supported_moduli = vec![
            bn_config.modulus.clone(),
            bn_config.scalar.clone(),
            SECP256K1_CONFIG.modulus.clone(),
            SECP256K1_CONFIG.scalar.clone(),
        ];
        let mut supported_complex_moduli = vec![bn_config.modulus.clone()];
        let mut supported_curves = vec![bn_config.clone(), SECP256K1_CONFIG.clone()];
        let mut supported_pairing_curves = vec![PairingCurve::Bn254];
        if use_kzg_intrinsics {
            supported_moduli.push(bls_config.modulus.clone());
            supported_moduli.push(bls_config.scalar.clone());
            supported_complex_moduli.push(bls_config.modulus.clone());
            supported_curves.push(bls_config.clone());
            supported_pairing_curves.push(PairingCurve::Bls12_381);
        }
        let vm_config = SdkVmConfig::builder()
            .system(system_config.into())
            .rv32i(Default::default())
            .rv32m(rv32m)
            .io(Default::default())
            .keccak(Default::default())
            .sha256(Default::default())
            .bigint(int256)
            .modular(ModularExtension::new(supported_moduli))
            .fp2(Fp2Extension::new(supported_complex_moduli))
            .ecc(WeierstrassExtension::new(supported_curves))
            .pairing(PairingExtension::new(supported_pairing_curves))
            .build();

        let chip_complex: VmChipComplex<OpenVmField<BabyBearField>, _, _> =
            vm_config.create_chip_complex().unwrap();
        // This will panic if the same id is used for multiple bus types
        let _ = get_bus_map(&chip_complex);
    }
}
