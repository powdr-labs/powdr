use powdr_number::{FieldElement, LargeInt};

use crate::Submachine;

#[derive(Debug, Eq, PartialEq)]
/// Order of fields matter: will be ordered by addr then step.
struct Op<F: FieldElement> {
    addr: u32,
    step: u32,
    value: F,
    write: F,
    // each machine that's called via permutation has a selector array, with one entry per incoming permutation.
    // This is the idx assigned to the `link` triggering the memory operation.
    selector_idx: u8,
}

pub struct MemoryMachine<F: FieldElement> {
    pub namespace: String,
    ops: Vec<Op<F>>,
    witness_cols: Vec<String>,
}

impl<F: FieldElement> MemoryMachine<F> {
    pub fn new(namespace: &str, witness_cols: &[String]) -> Self {
        // filter for the machine columns
        let prefix = format!("{namespace}::");
        let witness_cols = witness_cols
            .iter()
            .filter(|c| c.starts_with(&prefix))
            .cloned()
            .collect();

        MemoryMachine {
            namespace: namespace.to_string(),
            ops: Vec::new(),
            witness_cols,
        }
    }

    pub fn len(&self) -> u32 {
        self.ops.len() as u32
    }
}

impl<F: FieldElement> Submachine<F> for MemoryMachine<F> {
    fn len(&self) -> u32 {
        self.ops.len() as u32
    }

    fn namespace(&self) -> &str {
        &self.namespace
    }

    fn add_operation(&mut self, selector: Option<&str>, lookup_args: &[F; 4], _extra: &[F]) {
        let [op_id, addr, step, value] = lookup_args[..] else {
            panic!()
        };
        // get the idx from the selector
        let selector_idx = selector
            .map(|s| {
                let start = s.find('[').unwrap() + 1;
                let end = s.find(']').unwrap();
                s[start..end].parse::<u8>().unwrap()
            })
            .unwrap();
        self.ops.push(Op {
            addr: addr.to_integer().try_into_u32().unwrap(),
            step: step.to_integer().try_into_u32().unwrap(),
            value,
            write: op_id,
            selector_idx,
        });
    }

    fn finish(&mut self, degree: u32) -> Vec<(String, Vec<F>)> {
        assert!(
            degree >= self.len(),
            "trying to take less rows than memory ops"
        );

        // order here matters (pil defines the order of witness cols)! we use this to index into the columns
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

        // sort ops by (addr, step)
        self.ops.sort_by_key(|op| (op.addr, op.step));

        let mut cols: Vec<_> = std::mem::take(&mut self.witness_cols)
            .into_iter()
            .map(|n| (n, Vec::with_capacity(self.ops.len())))
            .collect();
        let selector_count = cols.len() - Cols::Selectors as usize;

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
            cols[IsWrite as usize].1.push(op.write);
            cols[Step as usize].1.push(op.step.into());
            cols[Addr as usize].1.push(op.addr.into());
            cols[Value as usize].1.push(op.value);

            for i in 0..selector_count as u8 {
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
        let last_value = self.ops.last().map(|op| op.value).unwrap_or(0.into());
        if self.len() < degree {
            // addr and value are repeated
            cols[Addr as usize]
                .1
                .resize(degree as usize, last_addr.into());
            cols[Value as usize].1.resize(degree as usize, last_value);
            // step increases
            cols[Step as usize].1.extend(
                (last_step + 1..)
                    .take((degree - self.len()) as usize)
                    .map(|x| F::from(x)),
            );
            // rest are zero
            cols[Change as usize].1.resize(degree as usize, 0.into());
            cols[IsWrite as usize].1.resize(degree as usize, 0.into());
            cols[DiffLower as usize].1.resize(degree as usize, 0.into());
            cols[DiffUpper as usize].1.resize(degree as usize, 0.into());
            for i in 0..selector_count as u32 {
                cols[Selectors as usize + i as usize]
                    .1
                    .resize(degree as usize, 0.into());
            }
            // m_change is 1 in last row
            *cols[Change as usize].1.last_mut().unwrap() = 1.into();
        }
        cols
    }
}
