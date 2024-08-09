use std::collections::BTreeMap;

use crate::{
    ast::{Argument, BinaryOpKind, Expression, FunctionOpKind, Register, Statement},
    utils::{alignment_size, split_at_first},
};

#[derive(Debug)]
pub enum DataValue {
    Direct(Vec<u8>),
    Zero(usize),
    // alignment size and the byte value used as padding
    Alignment(usize, u8),
    Reference(String),
    // This is needed for .word directives such as
    // .word	.Lfunc_begin0-.Lfunc_begin0
    Offset(String, String),
}

impl DataValue {
    /// Returns the size of the value in bytes.
    ///
    /// The address is necessary because the size of the alignment padding
    /// depends on what address it is defined on.
    pub fn size(&self, from_addr: usize) -> usize {
        match self {
            DataValue::Direct(data) => data.len(),
            DataValue::Zero(length) => *length,
            DataValue::Alignment(bytes, _) => alignment_size(from_addr, *bytes),
            DataValue::Reference(_) => 4,
            DataValue::Offset(..) => 4,
        }
    }
}

#[derive(Default)]
struct DataSections {
    /// This is a vector of sections, where each section is a vector of (maybe
    /// named) labels, which in turn contains a sequence of data values.
    ///
    /// I weighted against making this and a potential `struct Section` part of
    /// the public API because the users would need to know and access all the
    /// internals anyway, so it wouldn't be abstracting away any complexity.
    sections: Vec<Vec<(Option<String>, Vec<DataValue>)>>,
}

impl DataSections {
    fn new() -> Self {
        Default::default()
    }

    fn current_entry(&mut self) -> &mut Vec<DataValue> {
        let last_section = self.sections.last_mut().unwrap();
        if last_section.is_empty() {
            last_section.push((None, Vec::new()))
        }
        &mut last_section.last_mut().unwrap().1
    }

    fn append_label_to_curr_section(&mut self, label: &str) {
        let last_section = self.sections.last_mut().unwrap();
        last_section.push((Some(label.to_owned()), Vec::new()));
    }

    fn append_section(&mut self) {
        self.sections.push(Vec::new())
    }

    fn add_empty_section(&mut self, label: String) {
        self.sections.push(vec![(Some(label), Vec::new())]);

        // If there are other sections, the previous one is the active one, so we swap.
        let len = self.sections.len();
        if len > 1 {
            self.sections.swap(len - 1, len - 2);
        }
    }
}

pub struct DataObjects {
    pub sections: Vec<Vec<(Option<String>, Vec<DataValue>)>>,
    pub adhoc_symbols: BTreeMap<String, u32>,
}

/// Extract all data objects from the list of statements.
/// Returns the named data objects themselves and a vector of the names
/// in the order in which they occur in the statements.
pub fn extract_data_objects<R: Register, F: FunctionOpKind>(
    statements: &[Statement<R, F>],
) -> DataObjects {
    let mut adhoc_symbols = BTreeMap::new();
    let mut data = DataSections::new();

    let mut is_in_data_section = false;

    for s in statements {
        match s {
            Statement::Label(l) => {
                if is_in_data_section {
                    data.append_label_to_curr_section(l);
                }
            }
            Statement::Directive(dir, args) => match (dir.as_str(), &args[..]) {
                (".text", args) => {
                    assert!(args.is_empty());
                    is_in_data_section = false;
                }
                (".data", args) => {
                    assert!(args.is_empty());
                    is_in_data_section = true;
                    data.append_section();
                }
                (".section", args) => {
                    is_in_data_section = is_data_section(&args[0]);
                    if is_in_data_section {
                        data.append_section();
                    }
                }
                (
                    ".zero" | ".ascii" | ".asciz" | ".dword" | ".word" | ".half" | ".hword"
                    | ".short" | ".byte",
                    args,
                ) => {
                    if is_in_data_section {
                        data.current_entry()
                            .extend(extract_data_value(dir.as_str(), args));
                    } else {
                        // This is most likely debug data.
                    }
                }
                (".balign", [Argument::Expression(Expression::Number(byte_size))]) => {
                    if is_in_data_section {
                        data.current_entry()
                            .push(DataValue::Alignment(*byte_size as usize, 0));
                    }
                }
                (
                    ".balign",
                    [Argument::Expression(Expression::Number(byte_size)), Argument::Expression(Expression::Number(pad_value))],
                ) => {
                    if is_in_data_section {
                        data.current_entry()
                            .push(DataValue::Alignment(*byte_size as usize, *pad_value as u8));
                    }
                }
                (".p2align", [Argument::Expression(Expression::Number(pow_of_2))]) => {
                    if is_in_data_section {
                        data.current_entry()
                            .push(DataValue::Alignment((1 << pow_of_2) as usize, 0));
                    }
                }
                (
                    ".p2align",
                    [Argument::Expression(Expression::Number(pow_of_2)), Argument::Expression(Expression::Number(pad_value))],
                ) => {
                    if is_in_data_section {
                        data.current_entry().push(DataValue::Alignment(
                            (1 << pow_of_2) as usize,
                            *pad_value as u8,
                        ));
                    }
                }
                (
                    ".set",
                    [Argument::Expression(Expression::Symbol(label)), Argument::Expression(Expression::Number(value))],
                ) => {
                    // This is a directive that sets a symbol to a value. We
                    // create a phantom empty data section so reachability is
                    // happy, but we also save it so we can replace the symbol
                    // with the value when needed.
                    data.add_empty_section(label.clone());
                    adhoc_symbols.insert(label.clone(), *value as u32);
                }

                (n @ ".balign" | n @ ".p2align", arg) => {
                    // TODO: implement last optional argument of .balign and .p2align
                    unimplemented!("{n} {arg:?}");
                }
                _ => {}
            },
            _ => {}
        }
    }
    DataObjects {
        sections: data.sections,
        adhoc_symbols,
    }
}

fn is_data_section<R: Register, F: FunctionOpKind>(arg: &Argument<R, F>) -> bool {
    let full_name = match arg {
        Argument::StringLiteral(name) => name.as_slice(),
        Argument::Expression(Expression::Symbol(name)) => name.as_bytes(),
        _ => return false,
    };

    // split out the part before the initial '.'
    let name = split_at_first(full_name, &b'.').1.unwrap();

    // isolate name until next '.'
    let name = split_at_first(name, &b'.').0;

    matches!(
        name,
        b"sbss" | b"tbss" | b"bss" | b"sdata" | b"tdata" | b"rodata" | b"data" | b"data1"
    )
}

fn extract_data_value<R: Register, F: FunctionOpKind>(
    directive: &str,
    arguments: &[Argument<R, F>],
) -> Vec<DataValue> {
    match (directive, arguments) {
        (".zero", [Argument::Expression(Expression::Number(n))]) => {
            vec![DataValue::Zero(*n as usize)]
        }
        (
            ".zero",
            [Argument::Expression(Expression::Number(n)), Argument::Expression(Expression::Number(value))],
        ) => {
            assert!(0 <= *value && *value <= 0xff);
            vec![DataValue::Direct(vec![*value as u8; *n as usize])]
        }
        (".ascii", [Argument::StringLiteral(data)]) => {
            vec![DataValue::Direct(data.clone())]
        }
        (".asciz", [Argument::StringLiteral(data)]) => {
            let mut data = data.clone();
            data.push(0);
            vec![DataValue::Direct(data)]
        }
        (".dword" | ".half" | ".hword" | ".short" | ".byte", data) => {
            let len = match directive {
                ".dword" => 8,
                ".byte" => 1,
                _ => 2,
            };

            let mut bytes = Vec::with_capacity(data.len() * len);
            for arg in data {
                let Argument::Expression(Expression::Number(n)) = arg else {
                    panic!("only literals are supported for .{directive}");
                };
                for byte in 0..len {
                    bytes.push((n >> (byte * 8) & 0xff) as u8);
                }
            }

            vec![DataValue::Direct(bytes)]
        }
        (".word", data) => data
            .iter()
            .map(|x| match x {
                Argument::Expression(Expression::Number(n)) => {
                    let n = *n as u32;
                    DataValue::Direct(vec![
                        (n & 0xff) as u8,
                        (n >> 8 & 0xff) as u8,
                        (n >> 16 & 0xff) as u8,
                        (n >> 24 & 0xff) as u8,
                    ])
                }
                Argument::Expression(Expression::Symbol(sym)) => DataValue::Reference(sym.clone()),
                Argument::Expression(Expression::BinaryOp(BinaryOpKind::Sub, args)) => {
                    match args.as_slice() {
                        [Expression::Symbol(a), Expression::Symbol(b)] => {
                            DataValue::Offset(a.to_string(), b.to_string())
                        }
                        _ => panic!("Invalid .word directive"),
                    }
                }
                _ => panic!("Invalid .word directive"),
            })
            .collect::<Vec<DataValue>>(),
        _ => panic!(),
    }
}
