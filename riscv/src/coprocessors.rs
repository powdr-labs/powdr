use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
};

use powdr_number::{FieldElement, KnownField};

type RuntimeFunctionImpl = (&'static str, fn() -> String);

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct CoProcessor {
    name: &'static str,
    ty: &'static str,
    import: &'static str,
    instructions: &'static str,
    runtime_function_impl: Option<RuntimeFunctionImpl>,
}

static BINARY_COPROCESSOR: CoProcessor = CoProcessor {
    name: "binary",
    ty: "Binary",
    import: "use std::binary::Binary;",
    instructions: r#"
    // ================= binary/bitwise instructions =================
    instr and Y, Z -> X ~ binary.and;
    instr or Y, Z -> X ~ binary.or;
    instr xor Y, Z -> X ~ binary.xor;

            "#,
    runtime_function_impl: None,
};

static SHIFT_COPROCESSOR: CoProcessor = CoProcessor {
    name: "shift",
    ty: "Shift",
    import: "use std::shift::Shift;",
    instructions: r#"
    // ================= shift instructions =================
    instr shl Y, Z -> X ~ shift.shl;
    instr shr Y, Z -> X ~ shift.shr;

            "#,
    runtime_function_impl: None,
};

static SPLIT_GL_COPROCESSOR: CoProcessor = CoProcessor {
    name: "split_gl",
    ty: "SplitGL",
    import: "use std::split::split_gl::SplitGL;",
    instructions: r#"
// ================== wrapping instructions ==============
instr split_gl Z -> X, Y ~ split_gl.split;

    "#,
    runtime_function_impl: None,
};

static POSEIDON_GL_COPROCESSOR: CoProcessor = CoProcessor {
    name: "poseidon_gl",
    ty: "PoseidonGL",
    import: "use std::hash::poseidon_gl::PoseidonGL;",
    instructions: r#"
// ================== hashing instructions ==============
instr poseidon_gl ~ poseidon_gl.poseidon_permutation P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11 -> P0', P1', P2', P3';

"#,
    runtime_function_impl: Some(("poseidon_gl_coprocessor", poseidon_gl_call)),
};

static INPUT_COPROCESSOR: CoProcessor = CoProcessor {
    name: "prover_input",
    ty: "",
    import: "",
    instructions: "",
    runtime_function_impl: Some(("input_coprocessor", prover_input_call)),
};

static ALL_COPROCESSORS: [(&str, &CoProcessor); 5] = [
    (BINARY_COPROCESSOR.name, &BINARY_COPROCESSOR),
    (SHIFT_COPROCESSOR.name, &SHIFT_COPROCESSOR),
    (SPLIT_GL_COPROCESSOR.name, &SPLIT_GL_COPROCESSOR),
    (POSEIDON_GL_COPROCESSOR.name, &POSEIDON_GL_COPROCESSOR),
    (INPUT_COPROCESSOR.name, &INPUT_COPROCESSOR),
];

/// Defines which coprocessors should be used by the RISCV machine.
/// It is important to not add unused coprocessors since they may
/// lead to many extra columns in PIL.
#[derive(Default)]
pub struct CoProcessors {
    coprocessors: BTreeMap<&'static str, &'static CoProcessor>,
}

impl TryFrom<Vec<&str>> for CoProcessors {
    type Error = String;

    fn try_from(list: Vec<&str>) -> Result<Self, Self::Error> {
        let items: BTreeSet<&str> = list.into_iter().collect();

        if !items.iter().all(|co_processor| {
            ALL_COPROCESSORS
                .iter()
                .any(|(name, _)| co_processor == name)
        }) {
            return Err("Invalid co-processor specified.".to_string());
        }

        Ok(Self {
            coprocessors: ALL_COPROCESSORS
                .iter()
                .filter_map(|(name, co_processor)| {
                    if items.contains(name) {
                        Some((*name, *co_processor))
                    } else {
                        None
                    }
                })
                .collect(),
        })
    }
}

impl CoProcessors {
    /// The base version only adds the commonly used bitwise and shift operations.
    pub fn base<T: FieldElement>() -> CoProcessors {
        let mut coprocessors = BTreeMap::from([
            (BINARY_COPROCESSOR.name, &BINARY_COPROCESSOR),
            (SHIFT_COPROCESSOR.name, &SHIFT_COPROCESSOR),
            (INPUT_COPROCESSOR.name, &INPUT_COPROCESSOR),
        ]);

        if matches!(T::known_field(), Some(KnownField::GoldilocksField)) {
            // The mul instructions needs the split machine.
            coprocessors.insert(SPLIT_GL_COPROCESSOR.name, &SPLIT_GL_COPROCESSOR);
        }

        Self { coprocessors }
    }

    /// Poseidon also uses the Split machine.
    pub fn with_poseidon(mut self) -> Self {
        self.coprocessors
            .insert(SPLIT_GL_COPROCESSOR.name, &SPLIT_GL_COPROCESSOR);
        self.coprocessors
            .insert(POSEIDON_GL_COPROCESSOR.name, &POSEIDON_GL_COPROCESSOR);
        self
    }

    pub fn has(&self, key: &str) -> bool {
        self.coprocessors.contains_key(key)
    }

    pub fn declarations(&self) -> Vec<(&'static str, &'static str)> {
        self.coprocessors
            .values()
            .filter(|c| !c.ty.is_empty())
            .map(|c| (c.name, c.ty))
            .collect()
    }

    pub fn machine_imports(&self) -> Vec<&'static str> {
        self.coprocessors.values().map(|c| c.import).collect()
    }

    pub fn instructions(&self) -> String {
        self.coprocessors
            .values()
            .map(|c| c.instructions)
            .collect::<Vec<&str>>()
            .join("")
    }

    pub fn runtime_names(&self) -> Vec<&str> {
        self.coprocessors
            .values()
            .filter_map(|c| c.runtime_function_impl)
            .map(|f| f.0)
            .collect()
    }

    pub fn runtime(&self) -> String {
        self.runtime_names()
            .iter()
            .map(|f| {
                format!(
                    r#"
                        .globl {} 
                        {}:
                            ret
                        "#,
                    f, f
                )
            })
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn substitutions(&self) -> Vec<(&'static str, String)> {
        self.coprocessors
            .values()
            .filter_map(|c| c.runtime_function_impl)
            .map(|f| (f.0, f.1()))
            .collect()
    }

    pub fn registers(&self) -> String {
        // Poseidon has 12 inputs and 4 outputs.
        // The base RISCV machine has 4 assignment registers.
        // We need 12 extra general purpose registers to store the
        // input and values directly.

        if !self.coprocessors.contains_key(POSEIDON_GL_COPROCESSOR.name) {
            return String::new();
        }

        let p_regs: Vec<String> = (0..12).map(|i| format!("reg P{};", i)).collect();

        p_regs.join("\n")
    }
}

fn poseidon_gl_call() -> String {
    // The x10 register is RISCV's a0 register, which has the first function argument in function
    // calls.  The poseidon coprocessor has a single argument, the memory address of the 12 field
    // element input array, that is, a pointer to the first element.  Since the memory offset is
    // chosen by LLVM, we assume it is properly aligned.  The accesses to all elements are computed
    // below, using the offset above as base.  Therefore these should also be aligned.
    let decoding = |i| {
        format!(
            r#"
        P{i}, tmp2 <== mload({} + x10);
        tmp1, tmp2 <== mload({} + x10);
        P{i} <=X= P{i} + tmp1 * 2**32;
    "#,
            i * 8,
            i * 8 + 4
        )
    };

    let encoding = |i| {
        format!(
            r#"
        tmp1, tmp2 <== split_gl(P{i});
        mstore {} + x10, tmp1;
        mstore {} + x10, tmp2;
    "#,
            i * 8,
            i * 8 + 4
        )
    };

    let call = "poseidon_gl;";

    (0..12)
        .map(decoding)
        .chain(std::iter::once(call.to_string()))
        .chain((0..4).map(encoding))
        .collect()
}

fn prover_input_call() -> String {
    "x10 <=X= ${ std::prover::Query::DataIdentifier(std::convert::int(std::prover::eval(x11)), std::convert::int(std::prover::eval(x10))) };".to_string()
}

// This could also potentially go in the impl of CoProcessors,
// but I purposefully left it outside because it should be removed eventually.
pub fn call_every_submachine(coprocessors: &CoProcessors) -> Vec<String> {
    // TODO This is a hacky snippet to ensure that every submachine in the RISCV machine
    // is called at least once. This is needed for witgen until it can do default blocks
    // automatically.
    // https://github.com/powdr-labs/powdr/issues/548
    let mut calls = vec![];
    if coprocessors.has(BINARY_COPROCESSOR.name) {
        calls.push("x10 <== and(x10, x10);".to_string());
    }
    if coprocessors.has(SHIFT_COPROCESSOR.name) {
        calls.push("x10 <== shl(x10, x10);".to_string());
    }
    if coprocessors.has(POSEIDON_GL_COPROCESSOR.name) {
        calls.extend(vec![
            "poseidon_gl;".to_string(),
            "P0 <=X= 0;".to_string(),
            "P1 <=X= 0;".to_string(),
            "P2 <=X= 0;".to_string(),
            "P3 <=X= 0;".to_string(),
        ]);
    }
    if coprocessors.has(SPLIT_GL_COPROCESSOR.name) {
        calls.push("x10, x11 <== split_gl(x10);".to_string());
    }

    calls.extend(vec!["x10 <=X= 0;".to_string(), "x11 <=X= 0;".to_string()]);

    calls
}
