use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;
use std::sync::OnceLock;

use super::{ConnectionKind, EvalResult, FixedData, MachineParts};

use crate::witgen::machines::Machine;
use crate::witgen::rows::RowPair;
use crate::witgen::{MutableState, QueryCallback};
use itertools::Itertools;
use powdr_ast::analyzed::{DegreeRange, PolyID};
use powdr_number::{DegreeType, FieldElement};

pub struct BlockMachineJIT<'a, T: FieldElement> {
    /// The degree range of all columns in this machine
    degree_range: DegreeRange,
    /// The current degree of all columns in this machine
    degree: DegreeType,
    /// Block size, the period of the selector.
    block_size: usize,
    /// The row index (within the block) of the latch row
    latch_row: usize,
    fixed_data: &'a FixedData<'a, T>,
    /// The parts of the machine (identities, witness columns, etc.)
    parts: MachineParts<'a, T>,
    /// The type of constraint used to connect this machine to its caller.
    connection_type: ConnectionKind,
    /// The data of the machine, starting with row "-1".
    data: CompactData<T>,
    publics: BTreeMap<&'a str, T>,
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
        for w in &parts.witnesses {
            println!("{}", fixed_data.column_name(w));
        }
        None
    }
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
        todo!()
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        todo!()
    }
}

struct CompactData<T> {
    /// The cell values, stored in row-major order.
    data: Vec<T>,
    first_column_id: u64,
    column_count: usize,
}

impl<T: FieldElement> CompactData<T> {
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
            })
    }

    fn index(&self, row: usize, column: &PolyID) -> usize {
        // TODO checks?
        row * self.column_count + (column.id - self.first_column_id) as usize
    }

    pub fn set(&mut self, row: usize, column: &PolyID, value: T) {
        let index = self.index(row, column);
        self.data[index] = value;
    }

    pub fn get(&self, row: usize, column: &PolyID) -> &T {
        let index = self.index(row, column);
        &self.data[index]
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
    data: &mut CompactData<T>,
    row_index: usize,
    size: DegreeType,
) -> bool {
    let start = std::time::Instant::now();
    static BIN_COLS: OnceLock<BinCols> = OnceLock::new();
    // TODO underflow?
    let row_nr = row_index - 1;
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
        let op_id_v = *data.get(row_index, op_id);
        let a_v = data.get(row_index, a).to_integer();
        let b_v = data.get(row_index, b).to_integer();

        let c_v = if op_id_v == 0.into() {
            a_v & b_v
        } else if op_id_v == 1.into() {
            a_v | b_v
        } else if op_id_v == 2.into() {
            a_v ^ b_v
        } else {
            return false;
        };
        data.set(row_index, c, c_v.into());
        data.set(row_index - 1, a, (a_v & 0xffffff.into()).into());
        data.set(row_index - 1, b, (b_v & 0xffffff.into()).into());
        data.set(row_index - 1, c, (c_v & 0xffffff.into()).into());
        data.set(row_index - 1, a_byte, ((a_v >> 24) & 0xff.into()).into());
        data.set(row_index - 1, b_byte, ((b_v >> 24) & 0xff.into()).into());
        data.set(row_index - 1, c_byte, ((c_v >> 24) & 0xff.into()).into());
        data.set(row_index - 1, op_id, op_id_v);

        data.set(row_index - 2, a, (a_v & 0xffff.into()).into());
        data.set(row_index - 2, b, (b_v & 0xffff.into()).into());
        data.set(row_index - 2, c, (c_v & 0xffff.into()).into());
        data.set(row_index - 2, a_byte, ((a_v >> 16) & 0xff.into()).into());
        data.set(row_index - 2, b_byte, ((b_v >> 16) & 0xff.into()).into());
        data.set(row_index - 2, c_byte, ((c_v >> 16) & 0xff.into()).into());
        data.set(row_index - 2, op_id, op_id_v);

        data.set(row_index - 3, a, (a_v & 0xff.into()).into());
        data.set(row_index - 3, b, (b_v & 0xff.into()).into());
        data.set(row_index - 3, c, (c_v & 0xff.into()).into());
        data.set(row_index - 3, a_byte, ((a_v >> 8) & 0xff.into()).into());
        data.set(row_index - 3, b_byte, ((b_v >> 8) & 0xff.into()).into());
        data.set(row_index - 3, c_byte, ((c_v >> 8) & 0xff.into()).into());
        data.set(row_index - 3, op_id, op_id_v);

        data.set(row_index - 4, a_byte, (a_v & 0xff.into()).into());
        data.set(row_index - 4, b_byte, (b_v & 0xff.into()).into());
        data.set(row_index - 4, c_byte, (c_v & 0xff.into()).into());

        true
    } else {
        false
    }
}
