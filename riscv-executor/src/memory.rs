use powdr_number::FieldElement;

use crate::Elem;

#[derive(Debug, Eq, PartialEq)]
/// Order of fields matter: will be ordered by addr then step.
struct Op<F: FieldElement> {
    addr: u32,
    step: u32,
    value: Elem<F>,
    write: bool,
    // each machine that's called via permutation has a selector array, with one entry per incoming permutation.
    // This is the idx assigned to the `link` triggering the memory operation.
    selector_idx: u32,
}

pub struct MemoryMachine<F: FieldElement> {
    pub namespace: String,
    ops: Vec<Op<F>>,
    // this is the size of the "selector array" for this machine. We deduce it
    // from the largest idx given in incoming read/write operations. Each
    // element becomes a column in the final trace.
    selector_count: usize,
}

impl<F: FieldElement> MemoryMachine<F> {
    pub fn new(namespace: &str) -> Self {
        MemoryMachine {
            namespace: namespace.to_string(),
            ops: Vec::new(),
            selector_count: 0,
        }
    }

    pub fn write(&mut self, step: u32, addr: u32, val: Elem<F>, selector_idx: u32) {
        self.selector_count = std::cmp::max(self.selector_count, selector_idx as usize + 1);
        self.ops.push(Op {
            addr,
            step,
            value: val,
            write: true,
            selector_idx,
        });
    }

    pub fn read(&mut self, step: u32, addr: u32, val: Elem<F>, selector_idx: u32) {
        self.selector_count = std::cmp::max(self.selector_count, selector_idx as usize + 1);
        self.ops.push(Op {
            addr,
            step,
            value: val,
            write: false,
            selector_idx,
        });
    }

    pub fn len(&self) -> u32 {
        self.ops.len() as u32
    }

    pub fn take_cols(mut self, len: u32) -> Vec<(String, Vec<Elem<F>>)> {
        assert!(
            len >= self.len(),
            "trying to take less rows than memory ops"
        );

        // order here matters! we use this to index into the columns
        #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
        #[repr(usize)]
        enum Cols {
            Addr = 0,
            Step,
            Change,
            Value,
            IsWrite,
            DiffLower,
            DiffUpper,
            Selectors, // this is last and will be used as a base for selectors
        }
        use Cols::*;

        let mut cols = vec![
            (
                format!("{}::m_addr", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_step", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_change", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_value", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_is_write", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_diff_lower", self.namespace),
                Vec::with_capacity(len as usize),
            ),
            (
                format!("{}::m_diff_upper", self.namespace),
                Vec::with_capacity(len as usize),
            ),
        ];
        for i in 0..self.selector_count as u32 {
            cols.push((
                format!("{}::selectors[{}]", self.namespace, i),
                Vec::with_capacity(len as usize),
            ));
        }

        // sort ops by (addr, step)
        self.ops.sort_by_key(|op| (op.addr, op.step));

        // generate rows from ops
        for (idx, op) in self.ops.iter().enumerate() {
            if let Some(next_addr) = self.ops.get(idx + 1).map(|op| op.addr) {
                assert!(next_addr >= op.addr);
                let diff = if next_addr > op.addr {
                    cols[Change as usize].1.push(1.into());
                    (next_addr - op.addr).wrapping_sub(1)
                } else {
                    cols[Change as usize].1.push(0.into());
                    let next_step = self.ops.get(idx + 1).map(|op| op.step).unwrap();
                    (next_step - op.step).wrapping_sub(1)
                };
                cols[DiffLower as usize].1.push((diff & 0xffff).into());
                cols[DiffUpper as usize].1.push((diff >> 16).into());
            } else {
                // last row
                cols[DiffUpper as usize].1.push(0.into());
                cols[DiffLower as usize].1.push(0.into());
                cols[Change as usize].1.push(0.into());
            }
            cols[IsWrite as usize]
                .1
                .push(if op.write { 1.into() } else { 0.into() });
            cols[Step as usize].1.push(op.step.into());
            cols[Addr as usize].1.push(op.addr.into());
            cols[Value as usize].1.push(op.value);

            for i in 0..self.selector_count as u32 {
                cols[Selectors as usize + i as usize]
                    .1
                    .push(if i == op.selector_idx {
                        1.into()
                    } else {
                        0.into()
                    });
            }
        }

        // extend rows if needed
        let last_step = self.ops.last().map(|op| op.step).unwrap_or(0);
        let last_addr = self.ops.last().map(|op| op.addr).unwrap_or(0);
        let last_value = self
            .ops
            .last()
            .map(|op| op.value)
            .unwrap_or(Elem::Field(0.into()));
        if self.len() < len {
            // addr and value are repeated
            cols[Addr as usize].1.resize(len as usize, last_addr.into());
            cols[Value as usize].1.resize(len as usize, last_value);
            // step increases
            cols[Step as usize].1.extend(
                (last_step + 1..)
                    .take((len - self.len()) as usize)
                    .map(|x| Elem::from_u32_as_fe(x)),
            );
            // rest are zero
            cols[Change as usize].1.resize(len as usize, 0.into());
            cols[IsWrite as usize].1.resize(len as usize, 0.into());
            cols[DiffLower as usize].1.resize(len as usize, 0.into());
            cols[DiffUpper as usize].1.resize(len as usize, 0.into());
            for i in 0..self.selector_count as u32 {
                cols[Selectors as usize + i as usize]
                    .1
                    .resize(len as usize, 0.into());
            }
        }
        // m_change is 1 in last row
        *cols[Change as usize].1.last_mut().unwrap() = 1.into();
        cols
    }
}