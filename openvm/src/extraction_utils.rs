use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use crate::air_builder::AirKeygenBuilder;
use crate::bus_map::{BusMap, OpenVmBusType};
use crate::opcode::branch_opcodes_set;
use crate::powdr_extension::executor::RecordArenaDimension;
use crate::{opcode::instruction_allowlist, BabyBearSC, SpecializedConfig};
use crate::{AirMetrics, ExtendedVmConfig, ExtendedVmConfigExecutor, Instr};
use crate::{BabyBearPoseidon2Engine, ExtendedVmConfigCpuBuilder};
use itertools::Itertools;
use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ExecutorInventory, ExecutorInventoryError, MatrixRecordArena,
    SystemConfig, VmBuilder, VmChipComplex, VmCircuitConfig, VmExecutionConfig,
};
use openvm_circuit::system::memory::interface::MemoryInterfaceAirs;
use openvm_circuit::system::SystemChipInventory;
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_instructions::VmOpcode;

use crate::utils::get_pil;
use openvm_stark_backend::air_builders::symbolic::SymbolicRapBuilder;
use openvm_stark_backend::config::Val;
use openvm_stark_backend::interaction::fri_log_up::find_interaction_chunks;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_backend::prover::cpu::CpuBackend;
use openvm_stark_backend::{
    air_builders::symbolic::SymbolicConstraints, config::StarkGenericConfig, rap::AnyRap,
};
use openvm_stark_sdk::config::{
    baby_bear_poseidon2::{config_from_perm, default_perm},
    fri_params::SecurityParameters,
};
use openvm_stark_sdk::p3_baby_bear::{self, BabyBear};
use powdr_autoprecompiles::bus_map::BusType;
use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::expression::try_convert;
use powdr_autoprecompiles::{Apc, InstructionHandler, SymbolicMachine};
use serde::{Deserialize, Serialize};
use std::iter::Sum;
use std::ops::Deref;
use std::ops::{Add, Sub};
use std::sync::MutexGuard;

use crate::utils::UnsupportedOpenVmReferenceError;

use crate::customize_exe::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;

// TODO: Use `<PackedChallenge<BabyBearSC> as FieldExtensionAlgebra<Val<BabyBearSC>>>::D` instead after fixing p3 dependency
const EXT_DEGREE: usize = 4;

#[derive(Clone, Serialize, Deserialize, Default)]
pub struct OriginalAirs<F> {
    pub(crate) opcode_to_air: HashMap<VmOpcode, String>,
    pub(crate) air_name_to_machine: BTreeMap<String, (SymbolicMachine<F>, AirMetrics)>,
}

impl<F> InstructionHandler for OriginalAirs<F> {
    type Field = F;
    type Instruction = Instr<F>;
    type AirId = String;

    fn get_instruction_air_and_id(
        &self,
        instruction: &Self::Instruction,
    ) -> (Self::AirId, &SymbolicMachine<Self::Field>) {
        let id = self
            .opcode_to_air
            .get(&instruction.0.opcode)
            .unwrap()
            .clone();
        let air = &self.air_name_to_machine.get(&id).unwrap().0;
        (id, air)
    }

    fn is_allowed(&self, instruction: &Self::Instruction) -> bool {
        self.opcode_to_air.contains_key(&instruction.0.opcode)
    }

    fn is_branching(&self, instruction: &Self::Instruction) -> bool {
        branch_opcodes_set().contains(&instruction.0.opcode)
    }

    fn get_instruction_air_stats(&self, instruction: &Self::Instruction) -> AirStats {
        self.get_instruction_metrics(instruction.0.opcode)
            .map(|metrics| metrics.clone().into())
            .unwrap()
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

    pub fn get_instruction_metrics(&self, opcode: VmOpcode) -> Option<&AirMetrics> {
        self.opcode_to_air.get(&opcode).and_then(|air_name| {
            self.air_name_to_machine
                .get(air_name)
                .map(|(_, metrics)| metrics)
        })
    }

    pub fn allow_list(&self) -> Vec<VmOpcode> {
        self.opcode_to_air.keys().cloned().collect()
    }

    pub fn airs_by_name(&self) -> impl Iterator<Item = (&String, &SymbolicMachine<F>)> {
        self.air_name_to_machine
            .iter()
            .map(|(name, (machine, _))| (name, machine))
    }
}

/// For each air name, the dimension of a record arena needed to store the
/// records for a single APC call.
pub fn record_arena_dimension_by_air_name_per_apc_call<F>(
    apc: &Apc<F, Instr<F>>,
    air_by_opcode_id: &OriginalAirs<F>,
) -> BTreeMap<String, RecordArenaDimension> {
    apc.instructions()
        .iter()
        .map(|instr| &instr.0.opcode)
        .zip_eq(apc.subs.iter())
        .fold(BTreeMap::new(), |mut acc, (opcode, sub)| {
            // Get the air name for this opcode
            let air_name = air_by_opcode_id.opcode_to_air.get(opcode).unwrap();

            // Increment the height for this air name, initializing if necessary
            let entry = acc.entry(air_name.clone()).or_insert_with(|| {
                let (_, air_metrics) = air_by_opcode_id.air_name_to_machine.get(air_name).unwrap();

                RecordArenaDimension {
                    real_height: 0,
                    width: air_metrics.widths.main,
                    dummy_height: 0,
                }
            });
            entry.real_height += 1;
            (sub.is_empty()).then(|| entry.dummy_height += 1);
            acc
        })
}

type ChipComplex = VmChipComplex<
    BabyBearSC,
    MatrixRecordArena<Val<BabyBearSC>>,
    CpuBackend<BabyBearSC>,
    SystemChipInventory<BabyBearSC>,
>;

/// A lazy chip complex that is initialized on the first access
type LazyChipComplex = Option<ChipComplex>;

/// A shared and mutable reference to a `LazyChipComplex`.
type CachedChipComplex = Arc<Mutex<LazyChipComplex>>;

/// A guard that provides access to the chip complex, ensuring it is initialized.
pub struct ChipComplexGuard<'a> {
    guard: MutexGuard<'a, LazyChipComplex>,
}

impl<'a> Deref for ChipComplexGuard<'a> {
    type Target = ChipComplex;

    fn deref(&self) -> &Self::Target {
        // Unwrap is safe here because we ensure that the chip complex is initialized
        self.guard
            .as_ref()
            .expect("Chip complex should be initialized")
    }
}

/// A wrapper around the `ExtendedVmConfig` that caches a chip complex.
#[derive(Serialize, Deserialize, Clone)]
pub struct OriginalVmConfig {
    pub sdk_config: ExtendedVmConfig,
    #[serde(skip)]
    pub chip_complex: CachedChipComplex,
}

// TODO: derive `VmCircuitConfig`, currently not possible because we don't have SC/F everywhere
impl<SC: StarkGenericConfig> VmCircuitConfig<SC> for OriginalVmConfig
where
    Val<SC>: PrimeField32,
{
    fn create_airs(&self) -> Result<AirInventory<SC>, AirInventoryError> {
        self.sdk_config.create_airs()
    }
}

impl<F: PrimeField32> VmExecutionConfig<F> for OriginalVmConfig {
    type Executor = ExtendedVmConfigExecutor<F>;

    fn create_executors(
        &self,
    ) -> Result<ExecutorInventory<Self::Executor>, ExecutorInventoryError> {
        self.sdk_config.create_executors()
    }
}

impl AsRef<SystemConfig> for OriginalVmConfig {
    fn as_ref(&self) -> &SystemConfig {
        self.sdk_config.as_ref()
    }
}

impl AsMut<SystemConfig> for OriginalVmConfig {
    fn as_mut(&mut self) -> &mut SystemConfig {
        self.sdk_config.as_mut()
    }
}

impl OriginalVmConfig {
    pub fn new(sdk_config: ExtendedVmConfig) -> Self {
        Self {
            sdk_config,
            chip_complex: Default::default(),
        }
    }

    pub fn config(&self) -> &ExtendedVmConfig {
        &self.sdk_config
    }

    pub fn config_mut(&mut self) -> &mut ExtendedVmConfig {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");
        *guard = None; // Invalidate cache
        &mut self.sdk_config
    }

    /// Returns a guard that provides access to the chip complex, initializing it if necessary.
    fn chip_complex(&self) -> ChipComplexGuard<'_> {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");

        if guard.is_none() {
            // This is the expensive part that we want to run a single time: create the chip complex
            let airs = self
                .sdk_config
                .sdk
                .create_airs()
                .expect("Failed to create air inventory");
            let complex =
                <ExtendedVmConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(
                    &ExtendedVmConfigCpuBuilder,
                    &self.sdk_config,
                    airs,
                )
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
    pub fn airs(
        &self,
        max_degree: usize,
    ) -> Result<OriginalAirs<BabyBear>, UnsupportedOpenVmReferenceError> {
        let chip_complex = &self.chip_complex();

        let chip_inventory = &chip_complex.inventory;

        let executor_inventory: ExecutorInventory<ExtendedVmConfigExecutor<Val<BabyBearSC>>> =
            self.create_executors().unwrap();

        let instruction_allowlist = instruction_allowlist();

        instruction_allowlist
            .into_iter()
            .filter_map(|op| {
                executor_inventory
                    .instruction_lookup
                    .get(&op)
                    .map(|id| (op, *id as usize))
            })
            .map(|(op, executor_id)| {
                let insertion_index = chip_inventory.executor_idx_to_insertion_idx[executor_id];
                let air_ref = &chip_inventory.airs().ext_airs()[insertion_index];
                (op, air_ref)
            }) // find executor for opcode
            .try_fold(OriginalAirs::default(), |mut airs, (op, air_ref)| {
                airs.insert_opcode(op, air_ref.name(), || {
                    let columns = get_columns(air_ref.clone());
                    let constraints = get_constraints(air_ref.clone());
                    let metrics = get_air_metrics(air_ref.clone(), max_degree);

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
                            derived_columns: vec![],
                        },
                        metrics,
                    ))
                })?;

                Ok(airs)
            })
    }

    pub fn bus_map(&self) -> BusMap {
        let chip_complex = self.chip_complex();
        let inventory = &chip_complex.inventory;

        let shared_bitwise_lookup = inventory
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .next();
        let shared_range_tuple_checker = inventory
            .find_chip::<SharedRangeTupleCheckerChip<2>>()
            .next();

        let system_air_inventory = inventory.airs().system();
        let connector_air = system_air_inventory.connector;
        let memory_air = &system_air_inventory.memory;

        BusMap::from_id_type_pairs(
            {
                [
                    (
                        connector_air.execution_bus.index(),
                        BusType::ExecutionBridge,
                    ),
                    (
                        // TODO: make getting memory bus index a helper function
                        match &memory_air.interface {
                            MemoryInterfaceAirs::Volatile { boundary } => {
                                boundary.memory_bus.inner.index
                            }
                            MemoryInterfaceAirs::Persistent { boundary, .. } => {
                                boundary.memory_bus.inner.index
                            }
                        },
                        BusType::Memory,
                    ),
                    (connector_air.program_bus.index(), BusType::PcLookup),
                    (
                        connector_air.range_bus.index(),
                        BusType::Other(OpenVmBusType::VariableRangeChecker),
                    ),
                ]
                .into_iter()
            }
            .chain(shared_bitwise_lookup.into_iter().map(|chip| {
                (
                    chip.bus().inner.index,
                    BusType::Other(OpenVmBusType::BitwiseLookup),
                )
            }))
            .chain(shared_range_tuple_checker.into_iter().map(|chip| {
                (
                    chip.bus().inner.index,
                    BusType::Other(OpenVmBusType::TupleRangeChecker),
                )
            }))
            .map(|(id, bus_type)| (id as u64, bus_type)),
        )
    }

    pub fn chip_inventory_air_metrics(&self, max_degree: usize) -> HashMap<String, AirMetrics> {
        let inventory = &self.chip_complex().inventory;

        inventory
            .airs()
            .ext_airs()
            .iter()
            .map(|air| {
                let name = air.name();
                let metrics = get_air_metrics(air.clone(), max_degree);
                (name, metrics)
            })
            .collect()
    }
}

pub fn export_pil(writer: &mut impl std::io::Write, vm_config: &SpecializedConfig) {
    let blacklist = ["KeccakVmAir"];
    let bus_map = vm_config.sdk.bus_map();
    let chip_complex = vm_config.sdk.chip_complex();

    for air in chip_complex
        .inventory
        .executor_idx_to_insertion_idx
        .iter()
        .map(|insertion_idx| &chip_complex.inventory.airs().ext_airs()[*insertion_idx])
    {
        let name = get_name(air.clone());

        if blacklist.contains(&name.as_str()) {
            log::warn!("Skipping blacklisted AIR: {name}");
            continue;
        }

        let columns = get_columns(air.clone());

        let constraints = get_constraints(air.clone());

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

pub fn get_name<SC: StarkGenericConfig>(air: Arc<dyn AnyRap<SC>>) -> String {
    air.name()
}

pub fn get_constraints(
    air: Arc<dyn AnyRap<BabyBearSC>>,
) -> SymbolicConstraints<p3_baby_bear::BabyBear> {
    let builder = symbolic_builder_with_degree(air, None);
    builder.constraints()
}

pub fn get_air_metrics(air: Arc<dyn AnyRap<BabyBearSC>>, max_degree: usize) -> AirMetrics {
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
    use crate::DEFAULT_OPENVM_DEGREE_BOUND;

    use super::*;
    use openvm_algebra_circuit::{Fp2Extension, ModularExtension};
    use openvm_bigint_circuit::Int256;
    use openvm_circuit::arch::SystemConfig;
    use openvm_ecc_circuit::{WeierstrassExtension, SECP256K1_CONFIG};
    use openvm_pairing_circuit::{PairingCurve, PairingExtension};
    use openvm_rv32im_circuit::Rv32M;
    use openvm_sdk::config::SdkVmConfig;
    use powdr_openvm_hints_circuit::HintsExtension;

    #[test]
    fn test_get_bus_map() {
        // Adapted from openvm-reth-benchmark for a config which has a lot of extensions
        let use_kzg_intrinsics = true;

        let system_config = SystemConfig::default()
            .with_continuations()
            .with_max_constraint_degree(DEFAULT_OPENVM_DEGREE_BOUND)
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
        let sdk_vm_config = SdkVmConfig::builder()
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

        let _ = OriginalVmConfig::new(ExtendedVmConfig {
            sdk: sdk_vm_config,
            hints: HintsExtension,
        })
        .bus_map();
    }

    #[test]
    fn test_export_pil() {
        let writer = &mut Vec::new();
        let ext_config = ExtendedVmConfig {
            sdk: SdkVmConfig::riscv32(),
            hints: HintsExtension,
        };
        let base_config = OriginalVmConfig::new(ext_config);
        let specialized_config =
            SpecializedConfig::new(base_config, vec![], DEFAULT_OPENVM_DEGREE_BOUND);
        export_pil(writer, &specialized_config);
        let output = String::from_utf8(writer.clone()).unwrap();
        assert!(!output.is_empty(), "PIL output should not be empty");
    }
}
