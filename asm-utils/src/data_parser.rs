use std::collections::BTreeMap;

use crate::ast::{Argument, BinaryOpKind, Expression, FunctionOpKind, Register, Statement};

#[derive(Debug)]
pub enum DataValue {
    Direct(Vec<u8>),
    Zero(usize),
    Reference(String),
    // This is needed for .word diretives such as
    // .word	.Lfunc_begin0-.Lfunc_begin0
    Offset(String, String),
}

impl DataValue {
    /// Returns the size of the value in bytes.
    pub fn size(&self) -> usize {
        match self {
            DataValue::Direct(data) => data.len(),
            DataValue::Zero(length) => *length,
            DataValue::Reference(_) => 4,
            DataValue::Offset(..) => 4,
        }
    }
}

/// Extract all data objects from the list of statements.
/// Returns the named data objects themselves and a vector of the names
/// in the order in which they occur in the statements.
pub fn extract_data_objects<R: Register, F: FunctionOpKind>(
    statements: &[Statement<R, F>],
) -> (BTreeMap<String, Vec<DataValue>>, Vec<String>) {
    let mut current_label = None;
    // TODO the way these collections are used here looks hacky.
    // It might need a more function reimpl.
    let mut object_order = vec![];
    let mut objects = BTreeMap::new();
    for s in statements {
        match s {
            Statement::Label(l) => {
                current_label = Some(l.as_str());
            }
            Statement::Directive(dir, args) => match (dir.as_str(), &args[..]) {
                (
                    ".type",
                    [Argument::Expression(Expression::Symbol(name)), Argument::Expression(Expression::Symbol(kind))],
                ) if kind.as_str() == "@object" => {
                    object_order.push(name.clone());
                    assert!(objects.insert(name.clone(), vec![]).is_none());
                }
                (".zero" | ".ascii" | ".asciz" | ".word" | ".byte", args) => {
                    let label = current_label.unwrap().to_string();
                    objects
                        .entry(label.clone())
                        .or_insert_with(|| {
                            object_order.push(label);
                            Default::default()
                        })
                        .extend(extract_data_value(dir.as_str(), args));
                }
                (
                    ".size",
                    [Argument::Expression(Expression::Symbol(name)), Argument::Expression(Expression::Number(n))],
                ) if Some(name.as_str()) == current_label => {
                    let label = current_label.unwrap().to_string();
                    objects
                        .entry(current_label.unwrap().into())
                        .and_modify(|entry| {
                            let size: usize = entry.iter().map(|v| v.size()).sum();
                            assert!(
                                size as i64 == *n,
                                "Invalid size for data object {name}: computed: {size} vs. specified: {n}"
                            );
                        })
                        .or_insert_with(|| {
                            object_order.push(label);
                            assert!(*n == 0, "Nonzero size for object without elements: {name}");
                            Default::default()
                        });
                }
                _ => {}
            },
            _ => {}
        }
    }
    (objects, object_order)
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
        (".byte", data) => {
            // TODO alignment?
            vec![DataValue::Direct(
                data.iter()
                    .map(|x| {
                        if let Argument::Expression(Expression::Number(n)) = x {
                            *n as u8
                        } else {
                            panic!("Invalid argument to .byte directive")
                        }
                    })
                    .collect::<Vec<u8>>(),
            )]
        }
        _ => panic!(),
    }
}
