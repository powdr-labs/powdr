use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::Itertools;

use super::{ConnectionKind, Machine, MachineParts};
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{EvalError, EvalResult, FixedData, MutableState, QueryCallback};
use crate::witgen::{EvalValue, IncompleteCause};
use powdr_number::{DegreeType, FieldElement, LargeInt};

use powdr_ast::analyzed::{DegreeRange, PolyID};

/// If all witnesses of a machine have a name in this list (disregarding the namespace),
/// we'll consider it to be a double-sorted machine.
/// This does not include the selectors, which are dynamically added to this list.
const ALLOWED_WITNESSES: [&str; 11] = [
    "m_value1",
    "m_value2",
    "m_addr_high",
    "m_addr_low",
    "m_step_high",
    "m_step_low",
    "m_change",
    "m_is_write",
    "m_is_bootloader_write",
    "m_high_limb_equal",
    "m_diff_minus_one",
];

const DIFF_COLUMNS: [&str; 2] = ["m_high_limb_equal", "m_diff_minus_one"];
const BOOTLOADER_WRITE_COLUMN: &str = "m_is_bootloader_write";

// The operation ID is decomposed into m_is_write + 2 * m_is_bootloader_write
const OPERATION_ID_WRITE: u64 = 1;
const OPERATION_ID_BOOTLOADER_WRITE: u64 = 2;

/// A 32-bit word, represented as two 16-bit limbs (big endian).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Word32<T>(T, T);

impl<T: FieldElement> From<Word32<T>> for u64 {
    fn from(value: Word32<T>) -> Self {
        [value.0, value.1]
            .iter()
            .map(|a| {
                let value = a.to_integer().try_into_u64().unwrap();
                // We expect 16-Bit limbs
                TryInto::<u16>::try_into(value).expect("Expected 16-Bit limbs")
            })
            .fold(0u64, |acc, x| (acc << 16) | (x as u64))
    }
}

fn unzip_word_column<T: FieldElement>(column: Vec<Word32<T>>) -> (Vec<T>, Vec<T>) {
    column.into_iter().map(|w| (w.0, w.1)).unzip()
}

impl<T: FieldElement> From<u64> for Word32<T> {
    fn from(value: u64) -> Self {
        assert!(value <= 0xffff_ffff);
        let high = value >> 16;
        let low = value & 0xffff;
        Word32(T::from(high), T::from(low))
    }
}

fn split_column_name(name: &str) -> (&str, &str) {
    let mut limbs = name.split("::");
    let namespace = limbs.next().unwrap();
    let col = limbs.next().unwrap();
    (namespace, col)
}

/// TODO make this generic

pub struct DoubleSortedWitnesses16<'a, T: FieldElement> {
    degree_range: DegreeRange,
    degree: DegreeType,
    //key_col: String,
    /// Position of the witness columns in the data.
    /// The key column has a position of usize::max
    //witness_positions: HashMap<String, usize>,
    /// (addr, step) -> (value1, value2)
    trace: BTreeMap<(Word32<T>, Word32<T>), Operation<T>>,
    data: BTreeMap<u64, u64>,
    is_initialized: BTreeMap<Word32<T>, bool>,
    namespace: String,
    name: String,
    parts: MachineParts<'a, T>,
    /// If the machine has the `m_high_limbs_equal` and `m_diff_minus_one` columns, this is the base of the latter.
    diff_columns_base: Option<u64>,
    /// Whether this machine has a `m_is_bootloader_write` column.
    has_bootloader_write_column: bool,
    /// All selector IDs that are used on the right-hand side connecting identities.
    selector_ids: BTreeMap<u64, PolyID>,
}

struct Operation<T> {
    pub is_normal_write: bool,
    pub is_bootloader_write: bool,
    pub value: Word32<T>,
    pub selector_id: PolyID,
}

impl<'a, T: FieldElement> DoubleSortedWitnesses16<'a, T> {
    fn namespaced(&self, name: &str) -> String {
        format!("{}::{}", self.namespace, name)
    }

    pub fn try_new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: &MachineParts<'a, T>,
    ) -> Option<Self> {
        let degree_range = parts.common_degree_range();

        let degree = degree_range.max;

        // get the namespaces and column names
        let (mut namespaces, columns): (HashSet<_>, HashSet<_>) = parts
            .witnesses
            .iter()
            .map(|r| split_column_name(parts.column_name(r)))
            .unzip();

        if namespaces.len() > 1 {
            // columns are not in the same namespace, fail
            return None;
        }

        if !parts
            .connections
            .values()
            .all(|i| i.kind == ConnectionKind::Permutation)
        {
            return None;
        }

        let selector_ids = parts
            .connections
            .iter()
            .map(|(id, i)| try_to_simple_poly(&i.right.selector).map(|p| (*id, p.poly_id)))
            .collect::<Option<BTreeMap<_, _>>>()?;

        let namespace = namespaces.drain().next().unwrap().into();

        // TODO check the identities.
        let selector_names = selector_ids
            .values()
            .map(|s| split_column_name(parts.column_name(s)).1);
        let allowed_witnesses: HashSet<_> = ALLOWED_WITNESSES
            .into_iter()
            .chain(selector_names)
            .collect();
        if !columns.iter().all(|c| allowed_witnesses.contains(c)) {
            return None;
        }

        let has_diff_columns = DIFF_COLUMNS.iter().all(|c| columns.contains(c));
        let has_bootloader_write_column = columns.contains(&BOOTLOADER_WRITE_COLUMN);

        let diff_columns_base = if has_diff_columns {
            let diff_poly_id =
                fixed_data.try_column_by_name(&format!("{namespace}::{}", DIFF_COLUMNS[1]))?;
            let diff_constraint = fixed_data.global_range_constraints().witness_constraints
                [&diff_poly_id]
                .as_ref()?;

            let (min, max) = diff_constraint.range();

            if min == T::zero() {
                assert!(
                    max == T::from((1 << 16) - 1),
                    "Currently, only 16-Bit limbs are supported"
                );
                Some(max.to_degree() + 1)
            } else {
                return None;
            }
        } else {
            None
        };

        if !parts.prover_functions.is_empty() {
            log::warn!(
                "DoubleSortedWitness16 machine does not support prover functions.\
                The following prover functions are ignored:\n{}",
                parts.prover_functions.iter().format("\n")
            );
        }

        Some(Self {
            name,
            degree_range,
            namespace,
            parts: parts.clone(), // TODO is this really unused?
            degree,
            diff_columns_base,
            has_bootloader_write_column,
            trace: Default::default(),
            data: Default::default(),
            is_initialized: Default::default(),
            selector_ids,
        })
    }
}

impl<'a, T: FieldElement> Machine<'a, T> for DoubleSortedWitnesses16<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.selector_ids.keys().cloned().collect()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &mut MutableState<'a, '_, T, Q>,
        identity_id: u64,
        caller_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        self.process_plookup_internal(identity_id, caller_rows)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        let mut addr: Vec<Word32<T>> = vec![];
        let mut step: Vec<Word32<T>> = vec![];
        let mut value: Vec<Word32<T>> = vec![];
        let mut is_normal_write = vec![];
        let mut is_bootloader_write = vec![];
        let mut selectors = self
            .selector_ids
            .values()
            .map(|id| (id, Vec::new()))
            .collect::<BTreeMap<_, _>>();
        let mut set_selector = |selector_id: Option<PolyID>| {
            for (id, v) in selectors.iter_mut() {
                v.push(if Some(**id) == selector_id {
                    T::one()
                } else {
                    T::zero()
                })
            }
        };

        for ((current_address, current_step), operation) in std::mem::take(&mut self.trace) {
            let address_int: u64 = current_address.into();
            let step_int: u64 = current_step.into();

            if let Some(prev_address) = addr.last() {
                let prev_address_int: u64 = (*prev_address).into();
                assert!(
                    current_address >= *prev_address,
                    "Expected addresses to be sorted"
                );
                if self.diff_columns_base.is_none() && address_int - prev_address_int >= self.degree
                {
                    log::error!("Jump in memory accesses between {prev_address_int:x} and {address_int:x} is larger than or equal to the degree {}! This will violate the constraints.", self.degree);
                }

                let current_diff = if current_address != *prev_address {
                    address_int - prev_address_int
                } else {
                    let prev_step_int: u64 = (*step.last().unwrap()).into();
                    step_int - prev_step_int
                };
                assert!(current_diff > 0);
            }

            addr.push(current_address);
            step.push(current_step);
            value.push(operation.value);

            is_normal_write.push(operation.is_normal_write.into());
            is_bootloader_write.push(operation.is_bootloader_write.into());
            set_selector(Some(operation.selector_id));
        }
        if addr.is_empty() {
            // No memory access at all - fill a first row with something.
            addr.push(Word32(-T::one(), -T::one()));
            step.push(0.into());
            value.push(Word32(0.into(), 0.into()));
            is_normal_write.push(0.into());
            is_bootloader_write.push(0.into());
            set_selector(None);
        }

        let current_size = addr.len();
        let new_size = current_size.next_power_of_two() as DegreeType;
        let new_size = self.degree_range.fit(new_size);
        log::info!(
            "Resizing variable length machine '{}': {} -> {} (rounded up from {})",
            self.name,
            self.degree,
            new_size,
            current_size
        );
        self.degree = new_size;

        while addr.len() < self.degree as usize {
            addr.push(*addr.last().unwrap());
            step.push(Word32::from(u64::from(*step.last().unwrap()) + 1));
            value.push(*value.last().unwrap());
            is_normal_write.push(0.into());
            is_bootloader_write.push(0.into());
            set_selector(None);
        }

        let last_row_change_value = match self.has_bootloader_write_column {
            true => (&addr[0] != addr.last().unwrap()).into(),
            // In the machine without the bootloader write column, m_change is constrained
            // to be 1 in the last row.
            false => 1.into(),
        };

        let change = addr
            .iter()
            .tuple_windows()
            .map(|(a, a_next)| if a == a_next { 0.into() } else { 1.into() })
            .chain(once(last_row_change_value))
            .collect::<Vec<_>>();
        assert_eq!(change.len(), addr.len());

        let diff_columns = if self.diff_columns_base.is_some() {
            let (diff_col1, diff_col2): (Vec<T>, Vec<T>) = change
                .iter()
                .enumerate()
                .map(|(i, address_change)| {
                    // The first value should store whether the high 16-Bit limbs are equal;
                    // the second value should store the diff - 1 of the limb being compared

                    let values = if address_change == &T::one() {
                        &addr
                    } else {
                        &step
                    };

                    // Get the current and next value. The next value is None for the last row.
                    let current = &values[i];
                    let next = values.get(i + 1);

                    next.map(|next| {
                        if current.0 == next.0 {
                            assert!(current.1 < next.1);
                            (T::one(), next.1 - current.1 - T::one())
                        } else {
                            assert!(current.0 < next.0);
                            (T::zero(), next.0 - current.0 - T::one())
                        }
                    })
                    // On the last row, the diff columns are constrained to equal (0, 0).
                    .unwrap_or((T::zero(), T::zero()))
                })
                .unzip();
            vec![
                (self.namespaced(DIFF_COLUMNS[0]), diff_col1),
                (self.namespaced(DIFF_COLUMNS[1]), diff_col2),
            ]
        } else {
            vec![]
        };

        let bootloader_columns = if self.has_bootloader_write_column {
            vec![(
                self.namespaced(BOOTLOADER_WRITE_COLUMN),
                is_bootloader_write.clone(),
            )]
        } else {
            vec![]
        };

        let (addr_high, addr_low) = unzip_word_column(addr);
        let (value_high, value_low) = unzip_word_column(value);
        let (step_high, step_low) = unzip_word_column(step);

        let selector_columns = selectors
            .into_iter()
            .map(|(id, v)| (self.parts.column_name(id).to_string(), v))
            .collect::<Vec<_>>();

        [
            (self.namespaced("m_value1"), value_high),
            (self.namespaced("m_value2"), value_low),
            (self.namespaced("m_addr_high"), addr_high),
            (self.namespaced("m_addr_low"), addr_low),
            (self.namespaced("m_step_high"), step_high),
            (self.namespaced("m_step_low"), step_low),
            (self.namespaced("m_change"), change),
            (self.namespaced("m_is_write"), is_normal_write.clone()),
        ]
        .into_iter()
        .chain(diff_columns)
        .chain(bootloader_columns)
        .chain(selector_columns)
        .collect()
    }
}

impl<'a, T: FieldElement> DoubleSortedWitnesses16<'a, T> {
    pub fn process_plookup_internal(
        &mut self,
        identity_id: u64,
        caller_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        // We blindly assume the lookup is of the form
        // OP { operation_id, ADDR_high, ADDR_low, STEP, X_high, X_low } is
        // <selector> { operation_id, m_addr_high, m_addr_low, m_step_high * 2**16 + m_step_low, m_value_high, m_value_low }
        // Where:
        // - operation_id == 0: Read
        // - operation_id == 1: Write
        // - operation_id == 2: Bootloader write

        let args = self.parts.connections[&identity_id]
            .left
            .expressions
            .iter()
            .map(|e| caller_rows.evaluate(e).unwrap())
            .collect::<Vec<_>>();

        let operation_id = match args[0].constant_value() {
            Some(v) => v,
            None => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::NonConstantRequiredArgument("operation_id"),
                ))
            }
        };

        let selector_id = *self.selector_ids.get(&identity_id).unwrap();

        let is_normal_write = operation_id == T::from(OPERATION_ID_WRITE);
        let is_bootloader_write = operation_id == T::from(OPERATION_ID_BOOTLOADER_WRITE);
        let is_write = is_bootloader_write || is_normal_write;
        let addr = match (args[1].constant_value(), args[2].constant_value()) {
            (Some(high), Some(low)) => Word32(high, low),
            _ => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::NonConstantRequiredArgument("m_addr"),
                ))
            }
        };

        let addr_int: u64 = addr.into();

        if self.has_bootloader_write_column {
            let is_initialized = self.is_initialized.get(&addr).cloned().unwrap_or_default();
            if !is_initialized && !is_bootloader_write {
                panic!("Memory address {addr_int:x} must be initialized with a bootloader write",);
            }
            self.is_initialized.insert(addr, true);
        }

        let step = args[3]
            .constant_value()
            .ok_or_else(|| format!("Step must be known but is: {}", args[3]))?;
        let step_word = Word32::from(step.to_degree());

        let value1_expr = &args[4];
        let value2_expr = &args[5];

        log::trace!(
            "Query addr=0x{:x}, step={step}, write: {is_write}, value: ({} {})",
            addr_int,
            value1_expr,
            value2_expr
        );

        // TODO this does not check any of the failure modes
        let mut assignments = EvalValue::complete(vec![]);
        let has_side_effect = if is_write {
            let value = match (value1_expr.constant_value(), value2_expr.constant_value()) {
                (Some(high), Some(low)) => Word32(high, low),
                _ => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantRequiredArgument("m_value"),
                    ))
                }
            };

            let value_int: u64 = value.into();

            log::trace!(
                "Memory write: addr=0x{:x}, step={step}, value=0x{:x}",
                addr_int,
                value_int
            );
            self.data.insert(addr_int, value_int);
            self.trace
                .insert(
                    (addr, step_word),
                    Operation {
                        is_normal_write,
                        is_bootloader_write,
                        value,
                        selector_id,
                    },
                )
                .is_none()
        } else {
            let value = self.data.entry(addr_int).or_default();
            log::trace!(
                "Memory read: addr=0x{:x}, step={step}, value=0x{:x}",
                addr_int,
                value
            );

            let value_int: u64 = *value;
            let value_low = value_int & 0xffff;
            let value_high = value_int >> 16;
            let value_low_fe: T = value_low.into();
            let value_high_fe: T = value_high.into();

            let ass = (value1_expr.clone() - value_high_fe.into())
                .solve_with_range_constraints(caller_rows)?;
            assignments.combine(ass);
            let ass2 = (value2_expr.clone() - value_low_fe.into())
                .solve_with_range_constraints(caller_rows)?;
            assignments.combine(ass2);
            self.trace
                .insert(
                    (addr, step_word),
                    Operation {
                        is_normal_write,
                        is_bootloader_write,
                        value: Word32(value_high_fe, value_low_fe),
                        selector_id,
                    },
                )
                .is_none()
        };
        if has_side_effect {
            assignments = assignments.report_side_effect();
        }

        if self.trace.len() >= (self.degree as usize) {
            return Err(EvalError::RowsExhausted(self.name.clone()));
        }

        Ok(assignments)
    }
}
