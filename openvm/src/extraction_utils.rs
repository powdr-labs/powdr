use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::{Arc, Mutex};

use crate::air_builder::AirKeygenBuilder;
use crate::{opcode::instruction_allowlist, BabyBearSC, SpecializedConfig};
use crate::{AirMetrics, Instr, SpecializedExecutor, APP_LOG_BLOWUP};
use openvm_circuit::arch::{VmChipComplex, VmConfig, VmInventoryError};
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_instructions::VmOpcode;
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor, SdkVmConfigPeriphery};
use openvm_stark_backend::air_builders::symbolic::SymbolicRapBuilder;
use openvm_stark_backend::interaction::fri_log_up::find_interaction_chunks;
use openvm_stark_backend::{
    air_builders::symbolic::SymbolicConstraints, config::StarkGenericConfig, rap::AnyRap, Chip,
};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{config_from_perm, default_perm},
    fri_params::SecurityParameters,
};
use openvm_stark_sdk::p3_baby_bear::{self, BabyBear};
use powdr_autoprecompiles::bus_map::{BusMap, BusType};
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::{InstructionMachineHandler, SymbolicMachine};
use serde::{Deserialize, Serialize};
use std::iter::Sum;
use std::ops::Deref;
use std::ops::{Add, Sub};
use std::sync::MutexGuard;

use crate::utils::{get_pil, UnsupportedOpenVmReferenceError};

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;

// TODO: Use `<PackedChallenge<BabyBearSC> as FieldExtensionAlgebra<Val<BabyBearSC>>>::D` instead after fixing p3 dependency
const EXT_DEGREE: usize = 4;

#[derive(Clone, Serialize, Deserialize, Default)]
pub struct OriginalAirs<F> {
    opcode_to_air: HashMap<VmOpcode, String>,
    air_name_to_machine: BTreeMap<String, (SymbolicMachine<F>, AirMetrics)>,
}

impl<F> InstructionMachineHandler<F, Instr<F>> for OriginalAirs<F> {
    fn get_instruction_air(&self, instruction: &Instr<F>) -> Option<&SymbolicMachine<F>> {
        self.opcode_to_air
            .get(&instruction.0.opcode)
            .and_then(|air_name| {
                self.air_name_to_machine
                    .get(air_name)
                    .map(|(machine, _)| machine)
            })
    }
}

impl<F> OriginalAirs<F> {
    /// Insert a new opcode, generating the air if it does not exist
    /// Panics if the opcode already exists
    pub fn insert_opcode(
        &mut self,
        opcode: VmOpcode,
        air_name: String,
        machine: impl Fn() -> Result<(SymbolicMachine<F>, AirMetrics), UnsupportedOpenVmReferenceError>,
    ) -> Result<(), UnsupportedOpenVmReferenceError> {
        if self.opcode_to_air.contains_key(&opcode) {
            panic!("Opcode {opcode} already exists");
        }
        // Insert the machine only if `air_name` isn't already present
        if !self.air_name_to_machine.contains_key(&air_name) {
            let machine_instance = machine()?;
            self.air_name_to_machine
                .insert(air_name.clone(), machine_instance);
        }

        self.opcode_to_air.insert(opcode, air_name);
        Ok(())
    }

    pub fn get_instruction_metrics(&self, opcode: usize) -> Option<&AirMetrics> {
        self.opcode_to_air
            .get(&VmOpcode::from_usize(opcode))
            .and_then(|air_name| {
                self.air_name_to_machine
                    .get(air_name)
                    .map(|(_, metrics)| metrics)
            })
    }

    pub fn allow_list(&self) -> BTreeSet<usize> {
        self.opcode_to_air
            .keys()
            .map(|opcode| opcode.as_usize())
            .collect()
    }
}

fn to_option<T>(mut v: Vec<T>) -> Option<T> {
    match v.len() {
        0 => None,
        1 => Some(v.pop().unwrap()),
        _ => panic!("Expected at most one element, got multiple"),
    }
}

/// A lazy chip complex that is initialized on the first access
type LazyChipComplex =
    Option<VmChipComplex<BabyBear, SdkVmConfigExecutor<BabyBear>, SdkVmConfigPeriphery<BabyBear>>>;

/// A shared and mutable reference to a `LazyChipComplex`.
type CachedChipComplex = Arc<Mutex<LazyChipComplex>>;

/// A guard that provides access to the chip complex, ensuring it is initialized.
pub struct ChipComplexGuard<'a> {
    guard: MutexGuard<'a, LazyChipComplex>,
}

impl<'a> Deref for ChipComplexGuard<'a> {
    type Target =
        VmChipComplex<BabyBear, SdkVmConfigExecutor<BabyBear>, SdkVmConfigPeriphery<BabyBear>>;

    fn deref(&self) -> &Self::Target {
        // Unwrap is safe here because we ensure that the chip complex is initialized
        self.guard
            .as_ref()
            .expect("Chip complex should be initialized")
    }
}

/// A wrapper around the `SdkVmConfig` that caches a chip complex.
#[derive(Serialize, Deserialize, Clone)]
pub struct OriginalVmConfig {
    sdk_config: SdkVmConfig,
    #[serde(skip)]
    chip_complex: CachedChipComplex,
}

impl OriginalVmConfig {
    pub fn new(sdk_config: SdkVmConfig) -> Self {
        Self {
            sdk_config,
            chip_complex: Default::default(),
        }
    }

    pub fn config(&self) -> &SdkVmConfig {
        &self.sdk_config
    }

    pub fn config_mut(&mut self) -> &mut SdkVmConfig {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");
        *guard = None; // Invalidate cache
        &mut self.sdk_config
    }

    /// Returns a guard that provides access to the chip complex, initializing it if necessary.
    fn chip_complex(&self) -> ChipComplexGuard {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");

        if guard.is_none() {
            // This is the expensive part that we want to run a single time: create the chip complex
            let complex = self
                .sdk_config
                .create_chip_complex()
                .expect("Failed to create chip complex");
            // Store the complex in the guard
            *guard = Some(complex);
        }

        ChipComplexGuard { guard }
    }

    /// Given a VM configuration and a set of used instructions, computes:
    /// - The opcode -> AIR map
    /// - The bus map
    ///
    /// Returns an error if the conversion from the OpenVM expression type fails.
    pub fn airs(&self) -> Result<OriginalAirs<BabyBear>, UnsupportedOpenVmReferenceError> {
        let chip_complex = self.chip_complex();

        let instruction_allowlist = instruction_allowlist();

        let res = chip_complex
            .inventory
            .available_opcodes()
            .filter(|op| {
                // Filter out the opcode that we are not interested in
                instruction_allowlist.contains(&op.as_usize())
            })
            .filter_map(|op| Some((op, chip_complex.inventory.get_executor(op)?)))
            .try_fold(OriginalAirs::default(), |mut airs, (op, executor)| {
                airs.insert_opcode(op, get_name(executor.air()), || {
                    let air = executor.air();
                    let columns = get_columns(air.clone());
                    let constraints = get_constraints(air.clone());
                    let metrics = get_air_metrics(air);

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

                    Ok((
                        SymbolicMachine {
                            constraints: powdr_exprs.into_iter().map(Into::into).collect(),
                            bus_interactions: powdr_bus_interactions,
                        },
                        metrics,
                    ))
                })?;

                Ok(airs)
            });

        res
    }

    pub fn bus_map(&self) -> BusMap {
        let chip_complex = self.chip_complex();
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

    pub fn create_chip_complex(
        &self,
    ) -> Result<
        VmChipComplex<BabyBear, SdkVmConfigExecutor<BabyBear>, SdkVmConfigPeriphery<BabyBear>>,
        VmInventoryError,
    > {
        // Clear the cache
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");
        *guard = None; // Invalidate cache
                       // Create a new chip complex
        self.sdk_config.create_chip_complex()
    }

    pub fn chip_inventory_air_metrics(&self) -> HashMap<String, AirMetrics> {
        let inventory = &self.chip_complex().inventory;

        inventory
            .executors()
            .iter()
            .map(|executor| executor.air())
            .chain(
                inventory
                    .periphery()
                    .iter()
                    .map(|periphery| periphery.air()),
            )
            .map(|air| {
                // both executors and periphery implement the same `air()` API
                (air.name(), get_air_metrics(air))
            })
            .collect()
    }
}

pub fn export_pil(writer: &mut impl std::io::Write, vm_config: &SpecializedConfig) {
    let blacklist = ["KeccakVmAir"];
    let bus_map = vm_config.sdk_config.bus_map();
    let chip_complex: VmChipComplex<_, _, _> = vm_config.create_chip_complex().unwrap();

    for executor in chip_complex.inventory.executors().iter() {
        let air = executor.air();
        let name = match executor {
            SpecializedExecutor::PowdrExecutor(powdr_executor) => {
                powdr_executor.air_name() // name with opcode
            }
            _ => air.name(),
        };

        if blacklist.contains(&name.as_str()) {
            log::warn!("Skipping blacklisted AIR: {name}");
            continue;
        }

        let columns = get_columns(air.clone());

        let constraints = get_constraints(air);

        let pil = get_pil(&name, &constraints, &columns, vec![], &bus_map);
        writeln!(writer, "{pil}\n").unwrap();
    }
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

pub fn get_name(air: Arc<dyn AnyRap<BabyBearSC>>) -> String {
    air.name()
}

pub fn get_constraints(
    air: Arc<dyn AnyRap<BabyBearSC>>,
) -> SymbolicConstraints<p3_baby_bear::BabyBear> {
    let builder = symbolic_builder_with_degree(air, None);
    builder.constraints()
}

pub fn get_air_metrics(air: Arc<dyn AnyRap<BabyBearSC>>) -> AirMetrics {
    let max_degree = (1 << APP_LOG_BLOWUP) + 1;

    let main = air.width();

    let symbolic_rap_builder = symbolic_builder_with_degree(air, Some(max_degree));
    let preprocessed = symbolic_rap_builder.width().preprocessed.unwrap_or(0);

    let SymbolicConstraints {
        constraints,
        interactions,
    } = symbolic_rap_builder.constraints();

    let log_up = (find_interaction_chunks(&interactions, max_degree)
        .interaction_partitions()
        .len()
        + 1)
        * EXT_DEGREE;

    AirMetrics {
        widths: AirWidths {
            preprocessed,
            main,
            log_up,
        },
        constraints: constraints.len(),
        bus_interactions: interactions.len(),
    }
}

pub fn symbolic_builder_with_degree(
    air: Arc<dyn AnyRap<BabyBearSC>>,
    max_constraint_degree: Option<usize>,
) -> SymbolicRapBuilder<p3_baby_bear::BabyBear> {
    let perm = default_perm();
    let security_params = SecurityParameters::standard_fast();
    let config = config_from_perm(&perm, security_params);
    let air_keygen_builder = AirKeygenBuilder::new(config.pcs(), air);
    air_keygen_builder.get_symbolic_builder(max_constraint_degree)
}

#[derive(Clone, Copy, Serialize, Deserialize, Default, PartialEq, Eq, Debug)]
pub struct AirWidths {
    pub preprocessed: usize,
    pub main: usize,
    pub log_up: usize,
}

impl Add for AirWidths {
    type Output = AirWidths;
    fn add(self, rhs: AirWidths) -> AirWidths {
        AirWidths {
            preprocessed: self.preprocessed + rhs.preprocessed,
            main: self.main + rhs.main,
            log_up: self.log_up + rhs.log_up,
        }
    }
}

impl Sub for AirWidths {
    type Output = AirWidths;
    fn sub(self, rhs: AirWidths) -> AirWidths {
        AirWidths {
            preprocessed: self.preprocessed - rhs.preprocessed,
            main: self.main - rhs.main,
            log_up: self.log_up - rhs.log_up,
        }
    }
}

impl Sum<AirWidths> for AirWidths {
    fn sum<I: Iterator<Item = AirWidths>>(iter: I) -> AirWidths {
        iter.fold(AirWidths::default(), Add::add)
    }
}

impl AirWidths {
    pub fn total(&self) -> usize {
        self.preprocessed + self.main + self.log_up
    }
}

impl std::fmt::Display for AirWidths {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Total Width: {} (Preprocessed: {} Main: {}, Log Up: {})",
            self.preprocessed + self.main + self.log_up,
            self.preprocessed,
            self.main,
            self.log_up
        )
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Default, PartialEq, Eq, Debug)]
pub struct AirWidthsDiff {
    pub before: AirWidths,
    pub after: AirWidths,
}

impl AirWidthsDiff {
    pub fn new(before: AirWidths, after: AirWidths) -> Self {
        Self { before, after }
    }

    pub fn columns_saved(&self) -> AirWidths {
        self.before - self.after
    }
}

impl Add for AirWidthsDiff {
    type Output = AirWidthsDiff;

    fn add(self, rhs: AirWidthsDiff) -> AirWidthsDiff {
        AirWidthsDiff {
            before: self.before + rhs.before,
            after: self.after + rhs.after,
        }
    }
}

impl Sum<AirWidthsDiff> for AirWidthsDiff {
    fn sum<I: Iterator<Item = AirWidthsDiff>>(iter: I) -> AirWidthsDiff {
        let zero = AirWidthsDiff::new(AirWidths::default(), AirWidths::default());
        iter.fold(zero, Add::add)
    }
}

#[cfg(test)]
mod tests {
    use crate::APP_LOG_BLOWUP;

    use super::*;
    use openvm_algebra_circuit::{Fp2Extension, ModularExtension};
    use openvm_bigint_circuit::Int256;
    use openvm_circuit::arch::SystemConfig;
    use openvm_ecc_circuit::{WeierstrassExtension, SECP256K1_CONFIG};
    use openvm_pairing_circuit::{PairingCurve, PairingExtension};
    use openvm_rv32im_circuit::Rv32M;
    use openvm_sdk::config::SdkSystemConfig;

    #[test]
    fn test_get_bus_map() {
        // Adapted from openvm-reth-benchmark for a config which has a lot of extensions
        let use_kzg_intrinsics = true;

        let system_config = SystemConfig::default()
            .with_continuations()
            .with_max_constraint_degree((1 << APP_LOG_BLOWUP) + 1)
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
        let mut supported_complex_moduli =
            vec![("Bn254Fp2".to_string(), bn_config.modulus.clone())];
        let mut supported_curves = vec![bn_config.clone(), SECP256K1_CONFIG.clone()];
        let mut supported_pairing_curves = vec![PairingCurve::Bn254];
        if use_kzg_intrinsics {
            supported_moduli.push(bls_config.modulus.clone());
            supported_moduli.push(bls_config.scalar.clone());
            supported_complex_moduli.push(("Bls12_381Fp2".to_string(), bls_config.modulus.clone()));
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

        let _ = OriginalVmConfig::new(vm_config).bus_map();
    }

    #[test]
    fn test_export_pil() {
        let writer = &mut Vec::new();
        let base_config = OriginalVmConfig::new(
            SdkVmConfig::builder()
                .system(SdkSystemConfig::default())
                .build(),
        );
        let specialized_config = SpecializedConfig::new(
            base_config,
            vec![],
            crate::PrecompileImplementation::SingleRowChip,
        );
        export_pil(writer, &specialized_config);
        let output = String::from_utf8(writer.clone()).unwrap();
        assert!(!output.is_empty(), "PIL output should not be empty");
    }
}
