use std::collections::BTreeMap;
use std::{collections::HashSet, sync::Arc};

use crate::air_builder::AirKeygenBuilder;
use crate::{BabyBearSC, IntoOpenVm};
use itertools::Itertools;
use openvm_circuit::arch::{VmChipComplex, VmConfig};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_instructions::VmOpcode;
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
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::SymbolicMachine;

use crate::utils::{get_pil, UnsupportedOpenVmReferenceError};

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;

fn to_option<T>(mut v: Vec<T>) -> Option<T> {
    match v.len() {
        0 => None,
        1 => Some(v.pop().unwrap()),
        _ => panic!("Expected at most one element, got multiple"),
    }
}

fn get_bus_map<F: PrimeField32>(
    chip_complex: &VmChipComplex<F, SdkVmConfigExecutor<F>, SdkVmConfigPeriphery<F>>,
) -> BusMap {
    let builder = chip_complex.inventory_builder();

    let shared_bitwise_lookup =
        to_option(builder.find_chip::<SharedBitwiseOperationLookupChip<8>>());
    let shared_range_tuple_checker =
        to_option(builder.find_chip::<SharedRangeTupleCheckerChip<2>>());

    BusMap::from_id_type_pairs(
        {
            let base = &chip_complex.base;
            [
                (base.execution_bus().inner.index, BusType::ExecutionBridge),
                (base.memory_bus().inner.index, BusType::Memory),
                (base.program_bus().inner.index, BusType::PcLookup),
                (
                    base.range_checker_bus().inner.index,
                    BusType::VariableRangeChecker,
                ),
            ]
            .into_iter()
        }
        .chain(
            shared_bitwise_lookup
                .into_iter()
                .map(|chip| (chip.bus().inner.index, BusType::BitwiseLookup)),
        )
        .chain(
            shared_range_tuple_checker
                .into_iter()
                .map(|chip| (chip.bus().inner.index, BusType::TupleRangeChecker)),
        )
        .map(|(id, bus_type)| (id as u64, bus_type)),
    )
}

/// Given a VM configuration and a set of used instructions, computes:
/// - The opcode -> AIR map
/// - The bus map
///
/// Returns an error if the conversion from the OpenVM expression type fails.
pub fn get_airs_and_bus_map<P: IntoOpenVm>(
    vm_config: SdkVmConfig,
    used_instructions: &HashSet<VmOpcode>,
) -> Result<(BTreeMap<usize, SymbolicMachine<P>>, BusMap), UnsupportedOpenVmReferenceError> {
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    let bus_map = get_bus_map(&chip_complex);

    // Note that we could use chip_complex.inventory.available_opcodes() instead of used_instructions,
    // which depends on the program being executed. But this turns out to be heavy on memory, because
    // it includes large precompiles like Keccak.
    Ok((
        used_instructions
            .iter()
            .filter_map(|op| Some((op, chip_complex.inventory.get_executor(*op)?)))
            .map(|(op, executor)| {
                let air = executor.air();
                let columns = get_columns(air.clone());
                let constraints = get_constraints(air);

                let powdr_exprs = constraints
                    .constraints
                    .iter()
                    .map(|expr| try_convert(symbolic_to_algebraic(expr, &columns)))
                    .collect::<Result<Vec<_>, _>>()?;

                let powdr_bus_interactions = constraints
                    .interactions
                    .iter()
                    .map(|expr| openvm_bus_interaction_to_powdr(expr, &columns))
                    .collect::<Result<_, _>>()?;

                let symb_machine = SymbolicMachine {
                    constraints: powdr_exprs.into_iter().map(Into::into).collect(),
                    bus_interactions: powdr_bus_interactions,
                };

                Ok((op.as_usize(), symb_machine))
            })
            .collect::<Result<_, _>>()?,
        bus_map,
    ))
}

pub fn export_pil<VC: VmConfig<p3_baby_bear::BabyBear>>(
    vm_config: VC,
    path: &str,
    blacklist: &[&str],
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
            let name = air.name();

            if blacklist.contains(&name.as_str()) {
                log::warn!("Skipping blacklisted AIR: {name}");
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

pub fn get_columns(air: Arc<dyn AnyRap<BabyBearSC>>) -> Vec<Arc<String>> {
    let width = air.width();
    air.columns()
        .inspect(|columns| {
            assert_eq!(columns.len(), width);
        })
        .unwrap_or_else(|| (0..width).map(|i| format!("unknown_{i}")).collect())
        .into_iter()
        .map(Arc::new)
        .collect()
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
