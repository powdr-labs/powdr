use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};

use itertools::Itertools;
use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ExecutorInventory, ExecutorInventoryError, SystemConfig,
    VmCircuitConfig, VmExecutionConfig,
};
use openvm_circuit::system::memory::interface::MemoryInterfaceAirs;
use openvm_circuit_primitives::bitwise_op_lookup::SharedBitwiseOperationLookupChip;
use openvm_circuit_primitives::range_tuple::SharedRangeTupleCheckerChip;
use openvm_instructions::VmOpcode;
use openvm_stark_backend::air_builders::symbolic::SymbolicRapBuilder;
use openvm_stark_backend::interaction::fri_log_up::find_interaction_chunks;
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
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::{Apc, DegreeBound, InstructionHandler};
use powdr_openvm_bus_interaction_handler::bus_map::{BusMap, OpenVmBusType};
use serde::{Deserialize, Serialize};
use std::iter::Sum;
use std::ops::Deref;
use std::ops::{Add, Sub};
use std::sync::MutexGuard;

use crate::customize_exe::Instr;
use crate::isa::{OpenVmISA, OriginalCpuChipComplex};
use crate::powdr_extension::executor::RecordArenaDimension;
use crate::utils::openvm_bus_interaction_to_powdr;
use crate::utils::symbolic_to_algebraic;
use crate::utils::UnsupportedOpenVmReferenceError;
use crate::AirMetrics;
use crate::{air_builder::AirKeygenBuilder, BabyBearSC};

// TODO: Use `<PackedChallenge<BabyBearSC> as FieldExtensionAlgebra<Val<BabyBearSC>>>::D` instead after fixing p3 dependency
const EXT_DEGREE: usize = 4;

#[derive(Clone, Serialize, Deserialize)]
pub struct OriginalAirs<F, ISA> {
    /// The degree bound used when building the airs
    pub(crate) degree_bound: DegreeBound,
    /// Maps a VM opcode to the name of the (unique) AIR that implements it.
    pub(crate) opcode_to_air: HashMap<VmOpcode, String>,
    /// Maps an AIR name to its symbolic machine and metrics.
    /// Note that this map only contains AIRs that implement instructions.
    pub(crate) air_name_to_machine: BTreeMap<String, (SymbolicMachine<F>, AirMetrics)>,
    _marker: PhantomData<ISA>,
}

impl<F, ISA> InstructionHandler for OriginalAirs<F, ISA> {
    type Field = F;
    type Instruction = Instr<F, ISA>;
    type AirId = String;

    fn get_instruction_air_and_id(
        &self,
        instruction: &Self::Instruction,
    ) -> (Self::AirId, &SymbolicMachine<Self::Field>) {
        let id = self
            .opcode_to_air
            .get(&instruction.inner.opcode)
            .unwrap()
            .clone();
        let air = &self.air_name_to_machine.get(&id).unwrap().0;
        (id, air)
    }

    fn get_instruction_air_stats(&self, instruction: &Self::Instruction) -> AirStats {
        self.get_instruction_metrics(instruction.inner.opcode)
            .map(|metrics| metrics.clone().into())
            .unwrap()
    }

    fn degree_bound(&self) -> DegreeBound {
        self.degree_bound
    }
}

impl<F, ISA> OriginalAirs<F, ISA> {
    pub fn insert_opcode(
        &mut self,
        opcode: VmOpcode,
        air_name: String,
        machine: impl Fn(
            DegreeBound,
        )
            -> Result<(SymbolicMachine<F>, AirMetrics), UnsupportedOpenVmReferenceError>,
    ) -> Result<(), UnsupportedOpenVmReferenceError> {
        if self.opcode_to_air.contains_key(&opcode) {
            panic!("Opcode {opcode} already exists");
        }
        if !self.air_name_to_machine.contains_key(&air_name) {
            let machine_instance = machine(self.degree_bound)?;
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

    fn with_degree_bound(degree_bound: DegreeBound) -> Self {
        Self {
            degree_bound,
            opcode_to_air: Default::default(),
            air_name_to_machine: Default::default(),
            _marker: PhantomData,
        }
    }

    pub fn get_air_machine(&self, air_name: &str) -> Option<&SymbolicMachine<F>> {
        self.air_name_to_machine
            .get(air_name)
            .map(|(machine, _)| machine)
    }
}

pub fn record_arena_dimension_by_air_name_per_apc_call<F, ISA>(
    apc: &Apc<F, Instr<F, ISA>, (), ()>,
    air_by_opcode_id: &OriginalAirs<F, ISA>,
) -> BTreeMap<String, RecordArenaDimension> {
    apc.instructions()
        .map(|instr| &instr.inner.opcode)
        .zip_eq(apc.subs.iter().map(|sub| sub.is_empty()))
        .fold(
            BTreeMap::new(),
            |mut acc, (opcode, should_use_dummy_arena)| {
                let air_name = air_by_opcode_id.opcode_to_air.get(opcode).unwrap();

                let entry = acc.entry(air_name.clone()).or_insert_with(|| {
                    let (_, air_metrics) =
                        air_by_opcode_id.air_name_to_machine.get(air_name).unwrap();

                    RecordArenaDimension {
                        real_height: 0,
                        width: air_metrics.widths.main,
                        dummy_height: 0,
                    }
                });
                if should_use_dummy_arena {
                    entry.dummy_height += 1;
                } else {
                    entry.real_height += 1;
                }
                acc
            },
        )
}

type ChipComplex = OriginalCpuChipComplex;

type LazyChipComplex = Option<ChipComplex>;
type CachedChipComplex = Arc<Mutex<LazyChipComplex>>;

pub struct ChipComplexGuard<'a> {
    guard: MutexGuard<'a, LazyChipComplex>,
}

impl<'a> Deref for ChipComplexGuard<'a> {
    type Target = ChipComplex;

    fn deref(&self) -> &Self::Target {
        self.guard
            .as_ref()
            .expect("Chip complex should be initialized")
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct OriginalVmConfig<ISA: OpenVmISA> {
    pub config: ISA::Config,
    #[serde(skip)]
    pub chip_complex: CachedChipComplex,
}

impl<ISA: OpenVmISA> VmCircuitConfig<BabyBearSC> for OriginalVmConfig<ISA> {
    fn create_airs(&self) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
        self.config.create_airs()
    }
}

impl<ISA: OpenVmISA> VmExecutionConfig<BabyBear> for OriginalVmConfig<ISA> {
    type Executor = <ISA::Config as VmExecutionConfig<BabyBear>>::Executor;

    fn create_executors(
        &self,
    ) -> Result<ExecutorInventory<Self::Executor>, ExecutorInventoryError> {
        self.config.create_executors()
    }
}

impl<ISA: OpenVmISA> AsRef<SystemConfig> for OriginalVmConfig<ISA> {
    fn as_ref(&self) -> &SystemConfig {
        self.config.as_ref()
    }
}

impl<ISA: OpenVmISA> AsMut<SystemConfig> for OriginalVmConfig<ISA> {
    fn as_mut(&mut self) -> &mut SystemConfig {
        self.config.as_mut()
    }
}

impl<ISA: OpenVmISA> OriginalVmConfig<ISA> {
    pub fn new(config: ISA::Config) -> Self {
        Self {
            config,
            chip_complex: Default::default(),
        }
    }

    pub fn config(&self) -> &ISA::Config {
        &self.config
    }

    pub fn config_mut(&mut self) -> &mut ISA::Config {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");
        *guard = None;
        &mut self.config
    }

    pub fn chip_complex(&self) -> ChipComplexGuard<'_> {
        let mut guard = self.chip_complex.lock().expect("Mutex poisoned");

        if guard.is_none() {
            let airs = self
                .config
                .create_airs()
                .expect("Failed to create air inventory");
            let complex = ISA::create_original_chip_complex(&self.config, airs)
                .expect("Failed to create chip complex");
            *guard = Some(complex);
        }

        ChipComplexGuard { guard }
    }

    pub fn airs(
        &self,
        degree_bound: DegreeBound,
    ) -> Result<OriginalAirs<BabyBear, ISA>, UnsupportedOpenVmReferenceError> {
        let chip_complex = &self.chip_complex();
        let chip_inventory = &chip_complex.inventory;

        let executor_inventory = self.create_executors().unwrap();
        let instruction_allowlist = ISA::allowed_opcodes();

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
            })
            .try_fold(
                OriginalAirs::with_degree_bound(degree_bound),
                |mut airs, (op, air_ref)| {
                    airs.insert_opcode(op, air_ref.name(), |degree_bound| {
                        let columns = get_columns(air_ref.clone());
                        let constraints = get_constraints(air_ref.clone());
                        let metrics = get_air_metrics(air_ref.clone(), degree_bound.identities);

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
                },
            )
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
                    BusType::Other(OpenVmBusType::TupleRangeChecker(chip.bus().sizes)),
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
