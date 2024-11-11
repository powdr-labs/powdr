use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::sync::OnceLock;

use super::{Connection, ConnectionKind, EvalResult, FixedData, MachineParts};

use crate::witgen::affine_expression::{AffineExpression, AlgebraicVariable};
use crate::witgen::machines::{LookupCell, Machine};
use crate::witgen::rows::RowPair;
use crate::witgen::util::try_to_simple_poly;
use crate::witgen::{
    Constraint, EvalError, EvalValue, IncompleteCause, MutableState, QueryCallback,
};
use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicExpression as Expression, DegreeRange, Identity, PolyID, PolynomialType,
};
use powdr_ast::parsed::visitor::AllChildren;
use powdr_number::{DegreeType, FieldElement};
use std::iter::once;

pub struct BlockMachineJIT<'a, T: FieldElement> {
    /// The degree range of all columns in this machine
    degree_range: DegreeRange,
    /// The current degree of all columns in this machine
    degree: DegreeType,
    /// Block size, the period of the selector.
    block_size: usize,
    /// The row index (within the block) of the latch row
    latch_row: usize,
    /// The number of rows the data is shifted by. This allows
    /// blocks with a max per-column reach that is larger than the block size
    /// to still be stored consecutively.
    row_shift: usize,
    // TOOD mutable data, maybe move to `data`?
    blocks_generated: usize,
    fixed_data: &'a FixedData<'a, T>,
    /// The parts of the machine (identities, witness columns, etc.)
    parts: MachineParts<'a, T>,
    /// The type of constraint used to connect this machine to its caller.
    connection_type: ConnectionKind,
    /// The data of the machine, starting with row "-1".
    data: CompactData<'a, T>,

    name: String,
}

impl<'a, T: FieldElement> Display for BlockMachineJIT<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} - JIT (block_size: {}, latch_row: {})",
            self.name, self.block_size, self.latch_row
        )
    }
}

impl<'a, T: FieldElement> BlockMachineJIT<'a, T> {
    pub fn try_new(
        name: String,
        fixed_data: &'a FixedData<'a, T>,
        parts: &MachineParts<'a, T>,
    ) -> Option<Self> {
        if !parts
            .witnesses
            .iter()
            .any(|w| fixed_data.column_name(w).contains("main_binary"))
        {
            return None;
        }

        let degree_range = parts.common_degree_range();

        // start from the max degree
        let degree = degree_range.max;

        let (connection_type, block_size, latch_row) =
            detect_connection_type_and_block_size(fixed_data, &parts.connections)?;

        for id in parts.connections.values() {
            for r in id.right.expressions.iter() {
                if let Some(poly) = try_to_simple_poly(r) {
                    if poly.poly_id.ptype == PolynomialType::Constant {
                        // It does not really make sense to have constant polynomials on the RHS
                        // of a block machine lookup, as all constant polynomials are periodic, so
                        // it would always return the same value.
                        return None;
                    }
                }
            }
        }

        assert!(block_size <= degree as usize);

        let data = CompactData::try_new(&parts.witnesses, degree as usize)?;
        // TODO document.
        let row_shift = 1;

        Some(BlockMachineJIT {
            name,
            degree_range,
            degree,
            block_size,
            latch_row,
            row_shift,
            blocks_generated: 0,
            fixed_data,
            parts: parts.clone(),
            connection_type,
            data,
        })
    }
}

// TODO duplicated from block machine.

fn detect_connection_type_and_block_size<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    connections: &BTreeMap<u64, Connection<'a, T>>,
) -> Option<(ConnectionKind, usize, usize)> {
    // TODO we should check that the other constraints/fixed columns are also periodic.

    // Connecting identities should either all be permutations or all lookups.
    let connection_type = connections
        .values()
        .map(|id| id.kind)
        .unique()
        .exactly_one()
        .ok()?;

    // Detect the block size.
    let (latch_row, block_size) = match connection_type {
        ConnectionKind::Lookup => {
            // We'd expect all RHS selectors to be fixed columns of the same period.
            connections
                .values()
                .map(|id| try_to_period(&id.right.selector, fixed_data))
                .unique()
                .exactly_one()
                .ok()??
        }
        ConnectionKind::Permutation => {
            // We check all fixed columns appearing in RHS selectors. If there is none, the block size is 1.

            let find_max_period = |latch_candidates: BTreeSet<Expression<T>>| {
                latch_candidates
                    .iter()
                    .filter_map(|e| try_to_period(e, fixed_data))
                    // If there is more than one period, the block size is the maximum period.
                    .max_by_key(|&(_, period)| period)
            };
            let mut latch_candidates = BTreeSet::new();
            for id in connections.values() {
                collect_fixed_cols(&id.right.selector, &mut latch_candidates);
            }
            if latch_candidates.is_empty() {
                (0, 1)
            } else {
                find_max_period(latch_candidates)?
            }
        }
    };
    Some((connection_type, block_size, latch_row))
}

fn collect_fixed_cols<T: FieldElement>(
    expression: &Expression<T>,
    result: &mut BTreeSet<Expression<T>>,
) {
    for e in expression.all_children() {
        if let Expression::Reference(r) = e {
            if r.is_fixed() {
                result.insert(e.clone());
            }
        }
    }
}

/// Check if `expr` is a reference to a function of the form
/// f(i) { if (i + o) % k == 0 { 1 } else { 0 } }
/// for some k < degree / 2, o.
/// If so, returns (o, k).
fn try_to_period<T: FieldElement>(
    expr: &Expression<T>,
    fixed_data: &FixedData<T>,
) -> Option<(usize, usize)> {
    if let Expression::Number(ref n) = expr {
        if *n == T::one() {
            return Some((0, 1));
        }
    }

    let poly = try_to_simple_poly(expr)?;
    if !poly.is_fixed() {
        return None;
    }

    let degree = fixed_data.common_degree_range(once(&poly.poly_id)).max;

    let values = fixed_data.fixed_cols[&poly.poly_id].values(degree);

    let offset = values.iter().position(|v| v.is_one())?;
    let period = 1 + values.iter().skip(offset + 1).position(|v| v.is_one())?;
    if period > degree as usize / 2 {
        // This filters out columns like [0]* + [1], which might appear in a block machine
        // but shouldn't be detected as the latch.
        return None;
    }
    values
        .iter()
        .enumerate()
        .all(|(i, v)| {
            let expected = if i % period == offset {
                1.into()
            } else {
                0.into()
            };
            *v == expected
        })
        .then_some((offset, period))
}

impl<'a, T: FieldElement> Machine<'a, T> for BlockMachineJIT<'a, T> {
    fn identity_ids(&self) -> Vec<u64> {
        self.parts.connections.keys().copied().collect()
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        // TODO just return the query latch result and the rest async.
        if (self.blocks_generated + 1) * self.block_size > self.degree_range.max as usize {
            // Excausted the size of the machine.
            return Err(EvalError::RowsExhausted(self.name.clone()));
        }
        let start_outer = std::time::Instant::now();
        let connection = self.parts.connections.get(&identity_id).unwrap();

        static A_COL: OnceLock<PolyID> = OnceLock::new();
        static B_COL: OnceLock<PolyID> = OnceLock::new();
        static C_COL: OnceLock<PolyID> = OnceLock::new();
        static OP_ID: OnceLock<PolyID> = OnceLock::new();
        A_COL.get_or_init(|| {
            self.fixed_data
                .try_column_by_name("main_binary::A")
                .unwrap()
        });
        B_COL.get_or_init(|| {
            self.fixed_data
                .try_column_by_name("main_binary::B")
                .unwrap()
        });
        C_COL.get_or_init(|| {
            self.fixed_data
                .try_column_by_name("main_binary::C")
                .unwrap()
        });
        OP_ID.get_or_init(|| {
            self.fixed_data
                .try_column_by_name("main_binary::operation_id")
                .unwrap()
        });

        // Process the outer query.

        // TODO Set RHS selector to 1.
        // Propagate caller values into our columns.
        let mut input_a = None;
        let mut input_b = None;
        let mut input_op_id = None;
        let mut output_c = None;
        for (l, r) in connection
            .left
            .expressions
            .iter()
            .zip(&connection.right.expressions)
        {
            // TOOD this way, we only support direct expressions like `[.., a, ...] in [..., b, ...]`
            // we could also use something like
            // (caller_rows.evaluate(l) - r).solve()
            let Expression::Reference(r) = r else {
                continue;
            };
            if &r.poly_id == C_COL.get().unwrap() {
                let Expression::Reference(l) = l else {
                    continue;
                };

                output_c = Some(l);
                continue;
            };
            let Ok(AffineExpression::Constant(l)) = caller_rows.evaluate(l) else {
                continue;
            };
            if &r.poly_id == A_COL.get().unwrap() {
                input_a = Some(l);
            } else if &r.poly_id == B_COL.get().unwrap() {
                input_b = Some(l);
            } else if &r.poly_id == OP_ID.get().unwrap() {
                input_op_id = Some(l);
            }
        }
        let (Some(a), Some(b), Some(op_id), Some(output_c)) =
            (input_a, input_b, input_op_id, output_c)
        else {
            return Ok(EvalValue::incomplete(IncompleteCause::SolvingFailed));
        };

        let latch_row = self.blocks_generated * self.block_size + self.latch_row;
        let shifted_latch_row = latch_row + self.row_shift;
        self.blocks_generated += 1;

        self.data.set(shifted_latch_row, A_COL.get().unwrap(), a);
        self.data.set(shifted_latch_row, B_COL.get().unwrap(), b);
        self.data
            .set(shifted_latch_row, OP_ID.get().unwrap(), op_id);

        hack_binary_machine(
            self.fixed_data,
            mutable_state,
            &self.parts.identities,
            &mut self.data,
            shifted_latch_row,
            self.row_shift,
            self.degree,
        );

        let c = *self.data.get(shifted_latch_row, C_COL.get().unwrap());

        Ok(EvalValue::complete(vec![(
            AlgebraicVariable::Column(output_c),
            Constraint::Assignment(c),
        )]))
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        //println!("Number of blocks: {}", self.blocks_generated);
        // TODO handle dummy block, move first row to last, etc.

        let mut columns = vec![
            Vec::with_capacity(self.blocks_generated * self.block_size);
            self.parts.witnesses.len()
        ];
        let column_count = self.parts.witnesses.len();
        let mut vec_idx = 0;
        for v in self.data.iter_row_major() {
            columns[vec_idx].push(*v);
            vec_idx += 1;
            if vec_idx >= column_count {
                vec_idx = 0;
            }
        }
        let first_col_id = self
            .parts
            .witnesses
            .iter()
            .map(|col| col.id)
            .min()
            .unwrap_or(0) as usize;
        columns
            .into_iter()
            .enumerate()
            .map(|(i, v)| {
                let poly_id = PolyID {
                    id: i as u64 + first_col_id as u64,
                    ptype: PolynomialType::Committed,
                };
                let col_name = self.fixed_data.column_name(&poly_id).to_string();
                (col_name, v)
            })
            .collect()
    }
}

struct CompactData<'a, T> {
    /// The cell values, stored in row-major order.
    data: Vec<T>,
    first_column_id: u64,
    column_count: usize,
    /// The stored public values
    publics: BTreeMap<&'a str, T>,
}

impl<'a, T: FieldElement> CompactData<'a, T> {
    pub fn try_new(witnesses: &HashSet<PolyID>, row_count: usize) -> Option<Self> {
        let first_column_id = witnesses.iter().map(|col| col.id).min().unwrap_or(0);
        let column_count = witnesses.len();
        witnesses
            .iter()
            .all(|&id| id.id < first_column_id + column_count as u64)
            .then(|| Self {
                data: vec![0.into(); row_count * column_count],
                first_column_id,
                column_count,
                publics: Default::default(),
            })
    }

    #[inline]
    fn index(&self, row: usize, column: &PolyID) -> usize {
        // TODO checks?
        row * self.column_count + (column.id - self.first_column_id) as usize
    }

    #[inline]
    pub fn set(&mut self, row: usize, column: &PolyID, value: T) {
        let index = self.index(row, column);
        self.data[index] = value;
    }

    #[inline]
    pub fn get(&self, row: usize, column: &PolyID) -> &T {
        let index = self.index(row, column);
        &self.data[index]
    }

    #[inline]
    pub fn get_mut(&mut self, row: usize, column: &PolyID) -> &mut T {
        let index = self.index(row, column);
        &mut self.data[index]
    }

    #[inline]
    pub fn iter_row_major(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }
}

struct BinCols {
    a: PolyID,
    b: PolyID,
    c: PolyID,
    a_byte: PolyID,
    b_byte: PolyID,
    c_byte: PolyID,
    op_id: PolyID,
}

fn hack_binary_machine<'a, T: FieldElement>(
    fixed_data: &'a FixedData<'a, T>,
    mutable_state: &mut MutableState<'a, '_, T, impl QueryCallback<T>>,
    identities: &[&'a Identity<T>],
    data: &mut CompactData<T>,
    row_index: usize,
    row_shift: usize,
    size: DegreeType,
) -> bool {
    let start = std::time::Instant::now();
    static BIN_COLS: OnceLock<BinCols> = OnceLock::new();
    static LOOKUP_ID: OnceLock<u64> = OnceLock::new();
    // TODO underflow?
    let row_nr = row_index - row_shift;
    if row_nr % 4 == 3 {
        let BinCols {
            a,
            b,
            c,
            a_byte,
            b_byte,
            c_byte,
            op_id,
        } = BIN_COLS.get_or_init(|| {
            let a = fixed_data.try_column_by_name("main_binary::A").unwrap();
            let b = fixed_data.try_column_by_name("main_binary::B").unwrap();
            let c = fixed_data.try_column_by_name("main_binary::C").unwrap();
            let a_byte = fixed_data
                .try_column_by_name("main_binary::A_byte")
                .unwrap();
            let b_byte = fixed_data
                .try_column_by_name("main_binary::B_byte")
                .unwrap();
            let c_byte = fixed_data
                .try_column_by_name("main_binary::C_byte")
                .unwrap();
            let op_id = fixed_data
                .try_column_by_name("main_binary::operation_id")
                .unwrap();
            BinCols {
                a,
                b,
                c,
                a_byte,
                b_byte,
                c_byte,
                op_id,
            }
        });
        let lookup_id = *LOOKUP_ID.get_or_init(|| {
            let identity = identities
                .iter()
                .find(|id| id.to_string().contains("main_byte_binary"))
                .unwrap();
            println!("Lookup identity: {identity}");
            identity.id()
        });
        let op_id_v = *data.get(row_index, op_id);
        let a_v = data.get(row_index, a).to_integer();
        let b_v = data.get(row_index, b).to_integer();

        let a_byte_v = T::from(a_v & 0xff.into());
        data.set(row_index - 4, a_byte, a_byte_v);
        let b_byte_v = T::from(b_v & 0xff.into());
        data.set(row_index - 4, b_byte, b_byte_v);
        let c_byte_v =
            call_fixed_three_ret(mutable_state, lookup_id, &op_id_v, &a_byte_v, &b_byte_v);
        data.set(row_index - 4, c_byte, c_byte_v);

        data.set(row_index - 3, c, c_byte_v);
        let mut c_v = c_byte_v.to_integer();
        let a_byte_v = T::from((a_v >> 8) & 0xff.into());
        let b_byte_v = T::from((b_v >> 8) & 0xff.into());
        let c_byte_v =
            call_fixed_three_ret(mutable_state, lookup_id, &op_id_v, &a_byte_v, &b_byte_v);
        data.set(row_index - 3, a_byte, a_byte_v);
        data.set(row_index - 3, b_byte, b_byte_v);
        data.set(row_index - 3, c_byte, c_byte_v);
        data.set(row_index - 3, a, (a_v & 0xff.into()).into());
        data.set(row_index - 3, b, (b_v & 0xff.into()).into());
        data.set(row_index - 3, op_id, op_id_v);

        let a_byte_v = T::from((a_v >> 16) & 0xff.into());
        let b_byte_v = T::from((b_v >> 16) & 0xff.into());
        let c_byte_v =
            call_fixed_three_ret(mutable_state, lookup_id, &op_id_v, &a_byte_v, &b_byte_v);
        data.set(row_index - 2, a_byte, a_byte_v);
        data.set(row_index - 2, b_byte, b_byte_v);
        data.set(row_index - 2, c_byte, c_byte_v);
        c_v = c_v | (c_byte_v.to_integer() << 16);
        data.set(row_index - 2, c, c_v.into());
        data.set(row_index - 2, a, (a_v & 0xffff.into()).into());
        data.set(row_index - 2, b, (b_v & 0xffff.into()).into());
        data.set(row_index - 2, op_id, op_id_v);

        let a_byte_v = T::from((a_v >> 24) & 0xff.into());
        let b_byte_v = T::from((b_v >> 24) & 0xff.into());
        let c_byte_v =
            call_fixed_three_ret(mutable_state, lookup_id, &op_id_v, &a_byte_v, &b_byte_v);
        data.set(row_index - 1, a_byte, a_byte_v);
        data.set(row_index - 1, b_byte, b_byte_v);
        data.set(row_index - 1, c_byte, c_byte_v);
        c_v = c_v | (c_byte_v.to_integer() << 24);
        data.set(row_index - 1, c, c_v.into());
        data.set(row_index - 1, a, (a_v & 0xffffff.into()).into());
        data.set(row_index - 1, b, (b_v & 0xffffff.into()).into());
        data.set(row_index - 1, op_id, op_id_v);

        data.set(row_index, c, c_v.into());

        let end = std::time::Instant::now();
        //println!("Time: {}", (end - start).as_nanos());
        true
    } else {
        false
    }
}

#[inline]
fn call_fixed_three<'a, T: FieldElement>(
    mutable_state: &mut MutableState<'a, '_, T, impl QueryCallback<T>>,
    identity_id: u64,
    op_id: &T,
    a: &T,
    b: &T,
    c: &mut T,
) {
    let s = mutable_state
        .machines
        .call_direct(
            identity_id,
            vec![
                LookupCell::Input(op_id),
                LookupCell::Input(a),
                LookupCell::Input(b),
                LookupCell::Output(c),
            ],
        )
        .unwrap();
    assert!(s);
}

#[inline]
fn call_fixed_three_ret<'a, T: FieldElement>(
    mutable_state: &mut MutableState<'a, '_, T, impl QueryCallback<T>>,
    identity_id: u64,
    op_id: &T,
    a: &T,
    b: &T,
) -> T {
    let mut c = T::zero();
    call_fixed_three(mutable_state, identity_id, op_id, a, b, &mut c);
    c
}
