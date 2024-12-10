use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;

use itertools::Itertools;

use super::{Machine, MachineParts};
use crate::witgen::data_structures::mutable_state::MutableState;
use crate::witgen::machines::compute_size_and_log;
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{EvalError, EvalResult, FixedData, QueryCallback};
use crate::witgen::{EvalValue, IncompleteCause};

use powdr_number::{DegreeType, FieldElement, LargeInt};

use powdr_ast::analyzed::{DegreeRange, PolyID};

/// If all witnesses of a machine have a name in this list (disregarding the namespace),
/// we'll consider it to be a double-sorted machine.
/// This does not include the selectors, which are dynamically added to this list.
const ALLOWED_WITNESSES: [&str; 8] = [
    "m_value",
    "m_addr",
    "m_step",
    "m_change",
    "m_is_write",
    "m_is_bootloader_write",
    "m_diff_upper",
    "m_diff_lower",
];

const DIFF_COLUMNS: [&str; 2] = ["m_diff_upper", "m_diff_lower"];
const BOOTLOADER_WRITE_COLUMN: &str = "m_is_bootloader_write";

// The operation ID is decomposed into m_is_write + 2 * m_is_bootloader_write
const OPERATION_ID_WRITE: u64 = 1;
const OPERATION_ID_BOOTLOADER_WRITE: u64 = 2;

fn split_column_name(name: &str) -> (&str, &str) {
    let mut limbs = name.split("::");
    let namespace = limbs.next().unwrap();
    let col = limbs.next().unwrap();
    (namespace, col)
}

/// TODO make this generic

pub struct DoubleSortedWitnesses32<'a, T: FieldElement> {
    degree_range: DegreeRange,
    degree: DegreeType,
    //key_col: String,
    /// Position of the witness columns in the data.
    /// The key column has a position of usize::max
    //witness_positions: HashMap<String, usize>,
    /// (addr, step) -> value
    trace: BTreeMap<(T, T), Operation<T>>,
    /// The current contents of memory.
    data: PagedData<T>,
    is_initialized: BTreeMap<T, bool>,
    namespace: String,
    name: String,
    parts: MachineParts<'a, T>,
    /// If the machine has the `m_diff_upper` and `m_diff_lower` columns, this is the base of the
    /// two digits.
    diff_columns_base: Option<u64>,
    /// Whether this machine has a `m_is_bootloader_write` column.
    has_bootloader_write_column: bool,
    /// All selector IDs that are used on the right-hand side connecting identities.
    selector_ids: BTreeMap<u64, PolyID>,
}

struct Operation<T> {
    pub is_normal_write: bool,
    pub is_bootloader_write: bool,
    pub value: T,
    pub selector_id: PolyID,
}

impl<'a, T: FieldElement> DoubleSortedWitnesses32<'a, T> {
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

        if parts.connections.is_empty() {
            return None;
        }

        if !parts.connections.values().all(|i| i.is_permutation()) {
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
            // We have the `m_diff_upper` and `m_diff_lower` columns.
            // Now, we check that they both have the same range constraint and use it to determine
            // the base of the two digits.
            let upper_poly_id =
                fixed_data.try_column_by_name(&format!("{namespace}::{}", DIFF_COLUMNS[0]))?;
            let upper_range_constraint = fixed_data.global_range_constraints().witness_constraints
                [&upper_poly_id]
                .as_ref()?;
            let lower_poly_id =
                fixed_data.try_column_by_name(&format!("{namespace}::{}", DIFF_COLUMNS[1]))?;
            let lower_range_constraint = fixed_data.global_range_constraints().witness_constraints
                [&lower_poly_id]
                .as_ref()?;

            let (min, max) = upper_range_constraint.range();

            if upper_range_constraint == lower_range_constraint && min == T::zero() {
                Some(max.to_degree() + 1)
            } else {
                return None;
            }
        } else {
            None
        };

        if !parts.prover_functions.is_empty() {
            log::warn!(
                "DoubleSortedWitness32 machine does not support prover functions.\
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

impl<'a, T: FieldElement> Machine<'a, T> for DoubleSortedWitnesses32<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.selector_ids.keys().cloned().collect()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn process_plookup<Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &MutableState<'a, T, Q>,
        identity_id: u64,
        caller_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        self.process_plookup_internal(identity_id, caller_rows)
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        _mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        let mut addr = vec![];
        let mut step = vec![];
        let mut value = vec![];
        let mut is_normal_write = vec![];
        let mut is_bootloader_write = vec![];
        let mut diff = vec![];
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

        for ((a, s), o) in std::mem::take(&mut self.trace) {
            if let Some(prev_address) = addr.last() {
                assert!(a >= *prev_address, "Expected addresses to be sorted");
                if self.diff_columns_base.is_none()
                    && (a - *prev_address).to_degree() >= self.degree
                {
                    log::error!("Jump in memory accesses between {prev_address:x} and {a:x} is larger than or equal to the degree {}! This will violate the constraints.", self.degree);
                }

                let current_diff = if a != *prev_address {
                    a - *prev_address
                } else {
                    s - *step.last().unwrap()
                };
                assert!(current_diff > T::zero());
                diff.push(current_diff.to_degree() - 1);
            }

            addr.push(a);
            step.push(s);
            value.push(o.value);

            is_normal_write.push(o.is_normal_write.into());
            is_bootloader_write.push(o.is_bootloader_write.into());
            set_selector(Some(o.selector_id));
        }
        if addr.is_empty() {
            // No memory access at all - fill a first row with something.
            addr.push(-T::one());
            step.push(0.into());
            value.push(0.into());
            is_normal_write.push(0.into());
            is_bootloader_write.push(0.into());
            set_selector(None);
        }

        self.degree = compute_size_and_log(&self.name, addr.len(), self.degree_range);

        while addr.len() < self.degree as usize {
            addr.push(*addr.last().unwrap());
            step.push(*step.last().unwrap() + T::from(1));
            diff.push(0);
            value.push(*value.last().unwrap());
            is_normal_write.push(0.into());
            is_bootloader_write.push(0.into());
            set_selector(None);
        }

        // We have all diffs, except from the last to the first element, which is unconstrained.
        assert_eq!(diff.len(), self.degree as usize - 1);
        diff.push(0);

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

        let diff_columns = if let Some(diff_columns_base) = self.diff_columns_base {
            let diff_upper = diff
                .iter()
                .map(|d| T::from(*d / diff_columns_base))
                .collect::<Vec<_>>();
            let diff_lower = diff
                .iter()
                .map(|d| T::from(*d % diff_columns_base))
                .collect::<Vec<_>>();
            vec![
                (self.namespaced("m_diff_upper"), diff_upper),
                (self.namespaced("m_diff_lower"), diff_lower),
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

        let selector_columns = selectors
            .into_iter()
            .map(|(id, v)| (self.parts.column_name(id).to_string(), v))
            .collect::<Vec<_>>();

        [
            (self.namespaced("m_value"), value),
            (self.namespaced("m_addr"), addr),
            (self.namespaced("m_step"), step),
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

impl<'a, T: FieldElement> DoubleSortedWitnesses32<'a, T> {
    fn process_plookup_internal(
        &mut self,
        identity_id: u64,
        caller_rows: &RowPair<'_, 'a, T>,
    ) -> EvalResult<'a, T> {
        // We blindly assume the lookup is of the form
        // OP { operation_id, ADDR, STEP, X } is <selector> { operation_id, m_addr, m_step, m_value }
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
        let addr = match args[1].constant_value() {
            Some(v) => v,
            None => {
                return Ok(EvalValue::incomplete(
                    IncompleteCause::NonConstantRequiredArgument("m_addr"),
                ))
            }
        };

        if self.has_bootloader_write_column {
            let is_initialized = self.is_initialized.get(&addr).cloned().unwrap_or_default();
            if !is_initialized && !is_bootloader_write {
                panic!("Memory address {addr:x} must be initialized with a bootloader write",);
            }
            self.is_initialized.insert(addr, true);
        }

        let step = args[2]
            .constant_value()
            .ok_or_else(|| format!("Step must be known but is: {}", args[2]))?;

        let value_expr = &args[3];

        log::trace!(
            "Query addr={:x}, step={step}, write: {is_write}, value: {}",
            addr.to_arbitrary_integer(),
            value_expr
        );

        // TODO this does not check any of the failure modes
        let mut assignments = EvalValue::complete(vec![]);
        let has_side_effect = if is_write {
            let value = match value_expr.constant_value() {
                Some(v) => v,
                None => {
                    return Ok(EvalValue::incomplete(
                        IncompleteCause::NonConstantRequiredArgument("m_value"),
                    ))
                }
            };

            log::trace!(
                "Memory write: addr={:x}, step={step}, value={:x}",
                addr,
                value
            );
            self.data.write(addr, value);
            self.trace
                .insert(
                    (addr, step),
                    Operation {
                        is_normal_write,
                        is_bootloader_write,
                        value,
                        selector_id,
                    },
                )
                .is_none()
        } else {
            let value = self.data.read(addr);
            log::trace!(
                "Memory read: addr={:x}, step={step}, value={:x}",
                addr,
                value
            );
            let ass =
                (value_expr.clone() - value.into()).solve_with_range_constraints(caller_rows)?;
            assignments.combine(ass);
            self.trace
                .insert(
                    (addr, step),
                    Operation {
                        is_normal_write,
                        is_bootloader_write,
                        value,
                        selector_id,
                    },
                )
                .is_none()
        };
        assert!(
            has_side_effect,
            "Already had a memory access for address 0x{addr:x} and time step {step}!"
        );
        assignments = assignments.report_side_effect();

        if self.trace.len() > (self.degree as usize) {
            return Err(EvalError::RowsExhausted(self.name.clone()));
        }

        Ok(assignments)
    }
}

/// A paged key-value store. Addresses do not overlap, every address can store
/// a full field element.
struct PagedData<T> {
    /// All pages except the first.
    pages: HashMap<u64, Vec<T>>,
    /// The first page, to optimize for small memory addresses.
    first_page: Vec<T>,
}

impl<T: FieldElement> Default for PagedData<T> {
    fn default() -> Self {
        Self {
            pages: Default::default(),
            first_page: Self::fresh_page(),
        }
    }
}

impl<T: FieldElement> PagedData<T> {
    /// Tuning parameters.
    /// On the dev machine, only the combination of "PAGE_SIZE_LOG2 <= 8" and the introduction
    /// of "page zero" gives a 2x improvement in the register machine (and 20% in regular
    /// memory as well actually) relative to non-paged.
    /// This should be continuously monitored.
    const PAGE_SIZE_LOG2: u64 = 8;
    const PAGE_SIZE: u64 = (1 << Self::PAGE_SIZE_LOG2);
    const PAGE_MASK: u64 = Self::PAGE_SIZE - 1;

    fn page_offset(addr: T) -> (u64, usize) {
        let addr = addr.to_integer().try_into_u64().unwrap();
        (
            addr >> Self::PAGE_SIZE_LOG2,
            (addr & Self::PAGE_MASK) as usize,
        )
    }

    fn fresh_page() -> Vec<T> {
        vec![0.into(); Self::PAGE_SIZE as usize]
    }

    pub fn write(&mut self, addr: T, value: T) {
        let (page, offset) = Self::page_offset(addr);
        if page == 0 {
            self.first_page[offset] = value;
        } else {
            self.pages.entry(page).or_insert_with(Self::fresh_page)[offset] = value;
        }
    }

    pub fn read(&mut self, addr: T) -> T {
        let (page, offset) = Self::page_offset(addr);
        if page == 0 {
            self.first_page[offset]
        } else {
            self.pages
                .get(&page)
                .map(|page| page[offset])
                .unwrap_or_default()
        }
    }
}
