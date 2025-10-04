use itertools::Itertools;
use powdr_expression::visitors::Children;
use powdr_number::FieldElement;
use powdr_number::LargeInt;
use regex::Regex;
use std::collections::{BTreeSet, HashMap};
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

use crate::expression::{AlgebraicExpression, AlgebraicReference};
use crate::{SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine};
use powdr_expression::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};

fn get_values<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    vars: &BTreeSet<String>,
) -> Option<HashMap<String, String>> {
    let var_list = vars.iter().cloned().collect::<Vec<String>>().join(" ");
    let extra = format!("(get-value ({var_list}))");

    let (res, stdout, _stderr) = solve_ff(
        machine,
        Default::default(),
        None,
        Some(extra),
        SmtBusInteractionConfig::None,
    );

    match res {
        SmtResult::Unsat | SmtResult::Unknown => None,
        SmtResult::Sat => {
            let mut values = HashMap::new();
            let re = Regex::new(r"\(\s*(\w+)\s+([^\)]+)\)").unwrap();
            for cap in re.captures_iter(&stdout) {
                values.insert(cap[1].to_string(), cap[2].trim().to_string());
            }
            Some(values)
        }
    }
}

/// Check if a variable is uniquely determined by asserting it's not equal to its current value
fn is_unique<T: FieldElement>(machine: &SymbolicMachine<T>, var: &String, value: &String) -> bool {
    let extra = format!("(assert (not (= {var} {value})))");
    let (res, _stdout, _stderr) = solve_ff(
        machine,
        Default::default(),
        Some(extra),
        None,
        SmtBusInteractionConfig::None,
    );
    matches!(res, SmtResult::Unsat)
}

/// Return only uniquely determined variables and their values.
pub fn get_unique_vars<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    vars: &BTreeSet<String>,
) -> HashMap<String, String> {
    let Some(values) = get_values(machine, vars) else {
        return HashMap::new();
    };

    vars.iter()
        .filter_map(|var| {
            let val = values.get(var)?;
            if is_unique(machine, var, val) {
                Some((var.to_string(), val.to_string()))
            } else {
                None
            }
        })
        .collect()
}

fn solve_ff<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    decls: BTreeSet<String>,
    extra_before_check: Option<String>,
    extra_after_check: Option<String>,
    bus_interaction_config: SmtBusInteractionConfig,
) -> (SmtResult, String, String) {
    let mut smt2 = symbolic_machine_to_smtlib2_ff(machine, decls, bus_interaction_config);

    let mut file = NamedTempFile::new().unwrap();

    if let Some(extra) = extra_before_check {
        println!("adding before check: {extra}");
        smt2 = format!("{smt2}\n{extra}\n");
    }

    smt2 = format!("{smt2}\n(check-sat)");

    if let Some(extra) = extra_after_check {
        println!("adding after check {extra}");
        smt2 = format!("{smt2}\n{extra}\n");
    }

    writeln!(file, "{smt2}").unwrap();

    println!("Solving for\n{smt2}");

    let output = Command::new("cvc5")
        .arg(file.path())
        .arg("--produce-models")
        .arg("--tlimit-per=5000")
        .arg("--tlimit=10000")
        .output()
        .expect("Failed to run cvc5");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if !stdout.is_empty() {
        eprintln!("cvc5 stdout: {stdout}");
    }
    if !stderr.is_empty() {
        eprintln!("cvc5 stderr: {stderr}");
    }

    let res = if stdout.contains("timeout") || stdout.contains("unknown") {
        SmtResult::Unknown
    } else if stdout.contains("unsat") {
        SmtResult::Unsat
    } else {
        SmtResult::Sat
    };

    println!("SMT result = {res:?}");
    (res, stdout.to_string(), stderr.to_string())
}

fn algebraic_to_smt_ff<T: FieldElement>(expr: &AlgebraicExpression<T>) -> String {
    match expr {
        AlgebraicExpression::Number(x) => format!("(as ff{x} BB)"),
        AlgebraicExpression::Reference(AlgebraicReference { name, .. }) => (**name).clone(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = algebraic_to_smt_ff(left);
            let right = algebraic_to_smt_ff(right);
            match op {
                AlgebraicBinaryOperator::Add => format!("(ff.add {left} {right})"),
                AlgebraicBinaryOperator::Sub => format!("(ff.add {left} (ff.neg {right}))"),
                AlgebraicBinaryOperator::Mul => format!("(ff.mul {left} {right})"),
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr = algebraic_to_smt_ff(expr);
            let op_str = match op {
                AlgebraicUnaryOperator::Minus => "ff.neg",
            };
            format!("({op_str} {expr})")
        }
    }
}

fn try_algebraic_number<T: FieldElement>(expr: &AlgebraicExpression<T>) -> Option<T> {
    match expr {
        AlgebraicExpression::Number(n) => Some(*n),
        _ => None,
    }
}

fn is_range_constraint<T: FieldElement>(b: &SymbolicBusInteraction<T>) -> bool {
    [3, 6, 7].contains(&b.id)
}

#[derive(Debug, Ord, Eq, PartialEq, PartialOrd)]
struct SmtRangeConstraint {
    pub var: String,
    pub bits: u32,
}

impl SmtRangeConstraint {
    pub fn new(var: String, bits: u32) -> Self {
        SmtRangeConstraint { var, bits }
    }

    pub fn to_smt(&self) -> String {
        assert!(self.bits <= 8);
        let terms = (0..(1 << self.bits))
            .map(|i| format!("(ff.add {} (ff.neg (as ff{i} BB)))", self.var))
            .collect::<Vec<_>>();
        format!("(ff.mul {})", terms.join(" "))
    }
}

fn symbolic_bus_interaction_to_smt_ff<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<SmtRangeConstraint> {
    if b.id == 1 {
        memory_interaction_to_smt(b)
    } else if b.id == 3 {
        range_check_interaction_to_smt(b)
    } else if b.id == 6 {
        byte_check_interaction_to_smt(b)
    } else if b.id == 7 {
        tuple_range_check_interaction_to_smt(b)
    } else {
        vec![]
    }
}

fn memory_interaction_to_smt<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<SmtRangeConstraint> {
    assert_eq!(b.id, 1);
    assert!(b.args.len() > 5);
    vec![
        byte_check_to_smt(algebraic_to_smt_ff(&b.args[2])),
        byte_check_to_smt(algebraic_to_smt_ff(&b.args[3])),
        byte_check_to_smt(algebraic_to_smt_ff(&b.args[4])),
        byte_check_to_smt(algebraic_to_smt_ff(&b.args[5])),
    ]
}

fn range_check_interaction_to_smt<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<SmtRangeConstraint> {
    assert_eq!(b.id, 3);
    assert_eq!(b.args.len(), 2);
    let v = algebraic_to_smt_ff(&b.args[0]);
    let bits = try_algebraic_number(&b.args[1])
        .unwrap()
        .to_integer()
        .try_into_u32()
        .unwrap();
    if bits <= 8 {
        let max_range = 1 << bits;
        // vec![range_check_to_smt(v, max_range)]
        vec![SmtRangeConstraint::new(v, max_range)]
    } else {
        vec![]
    }
}

fn tuple_range_check_interaction_to_smt<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<SmtRangeConstraint> {
    assert_eq!(b.id, 7);
    assert_eq!(b.args.len(), 2);
    let v1 = algebraic_to_smt_ff(&b.args[0]);
    let v2 = algebraic_to_smt_ff(&b.args[1]);
    vec![byte_check_to_smt(v1), byte_check_to_smt(v2)]
}

fn byte_check_interaction_to_smt<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<SmtRangeConstraint> {
    assert_eq!(b.id, 6);
    assert_eq!(b.args.len(), 4);
    let v1 = algebraic_to_smt_ff(&b.args[0]);
    let v2 = algebraic_to_smt_ff(&b.args[1]);
    let v3 = algebraic_to_smt_ff(&b.args[2]);
    let v4 = algebraic_to_smt_ff(&b.args[3]);
    vec![
        byte_check_to_smt(v1),
        byte_check_to_smt(v2),
        byte_check_to_smt(v3),
        byte_check_to_smt(v4),
    ]
}

fn byte_check_to_smt(v: String) -> SmtRangeConstraint {
    SmtRangeConstraint::new(v, 8)
}

#[derive(Debug)]
enum SmtResult {
    Sat,
    Unsat,
    Unknown,
}

const P: u32 = (1 << 31) - (1 << 27) + 1;

enum SmtBusInteractionConfig {
    // No range constraints
    None,
    // All range constraints
    All,
    // Only range constraints over variables in this vector.
    Partial(Vec<String>),
}

fn symbolic_machine_to_smtlib2_ff<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    decls: BTreeSet<String>,
    bus_interaction_config: SmtBusInteractionConfig,
) -> String {
    let mut smt2 = String::new();
    smt2.push_str("(set-logic QF_FF)\n");
    smt2.push_str("(set-option :incremental true)\n");
    smt2.push_str(&format!("(define-sort BB () (_ FiniteField {P}))\n"));

    let mut this_decls = machine
        .main_columns()
        .map(|c|
                // Variable declarations.
                format!("{c}"))
        .collect::<BTreeSet<String>>();

    this_decls.extend(decls);

    let decls = this_decls
        .into_iter()
        .map(|c| format!("(declare-fun {c} () BB)"))
        .collect::<Vec<String>>()
        .join("\n");

    smt2.push_str(&decls);
    smt2.push('\n');

    match bus_interaction_config {
        SmtBusInteractionConfig::None => {
            // No bus interactions.
        }
        SmtBusInteractionConfig::All => {
            // All bus interactions.
            for bus_int in &machine.bus_interactions {
                let bus_int_smt = symbolic_bus_interaction_to_smt_ff(bus_int);
                for bus_constraint in bus_int_smt {
                    let a = format!("(assert (= (as ff0 BB) {}))", bus_constraint.to_smt());
                    smt2.push_str(&a);
                    smt2.push('\n');
                }
            }
        }
        SmtBusInteractionConfig::Partial(vars) => {
            let bus_ints: BTreeSet<_> = machine
                .bus_interactions
                .iter()
                .flat_map(|bus_int| symbolic_bus_interaction_to_smt_ff(bus_int))
                .collect();
            for bus_constraint in &bus_ints {
                if vars.iter().any(|v| &bus_constraint.var == v) {
                    let a = format!("(assert (= (as ff0 BB) {}))", bus_constraint.to_smt());
                    smt2.push_str(&a);
                    smt2.push('\n');
                }
            }
        }
    }

    for poly in &machine.constraints {
        let c_smt = algebraic_to_smt_ff(&poly.expr);
        let a = format!("(assert (= (as ff0 BB) {c_smt}))");
        smt2.push_str(&a);
        smt2.push('\n');
    }

    smt2
}

fn symbolic_bus_interaction_has_ref<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
    var: &String,
) -> bool {
    b.children().any(|arg| match arg {
        AlgebraicExpression::Reference(ref r) => &*r.name == var,
        _ => false,
    })
}

fn negate_constraints_smtlib2_ff<T: FieldElement>(
    constraints: Vec<SymbolicConstraint<T>>,
) -> String {
    let inner = constraints
        .iter()
        .map(|poly| format!("(= (as ff0 BB) {})", algebraic_to_smt_ff(&poly.expr)))
        .collect::<Vec<_>>()
        .join(" ");
    format!("(assert (not (and {inner})))")
}

pub fn check_equivalence<T: FieldElement>(machine: &SymbolicMachine<T>) -> Option<bool> {
    let r = solve_ff(
        machine,
        Default::default(),
        None,
        None,
        SmtBusInteractionConfig::All,
    );

    println!("Result: {r:?}");

    None
}

pub fn detect_equalities<T: FieldElement>(mut machine: SymbolicMachine<T>) -> SymbolicMachine<T> {
    let vars = machine.main_columns().collect::<Vec<_>>();

    if vars.len() > 20 {
        return machine;
    }

    let pairs: Vec<_> = vars.iter().combinations(2).collect();
    for pair in pairs {
        if check_equal_variables(&machine, pair[0], pair[1]) {
            print!("Found {} = {}", pair[0], pair[1]);
            let c = AlgebraicExpression::new_binary(
                AlgebraicExpression::Reference(pair[0].clone()),
                AlgebraicBinaryOperator::Sub,
                AlgebraicExpression::Reference(pair[1].clone()),
            );
            machine.constraints.push(c.into());
        }
    }

    machine
}

pub fn check_equal_variables<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    a: &AlgebraicReference,
    b: &AlgebraicReference,
) -> bool {
    println!("Checking whether {a} = {b}");
    let goal = format!("(assert (not (= {a} {b})))");
    match solve_ff(
        machine,
        Default::default(),
        Some(goal),
        None,
        SmtBusInteractionConfig::Partial(vec![format!("{a}"), format!("{b}")]),
    )
    .0
    {
        SmtResult::Unsat => true,
        SmtResult::Sat => false,
        SmtResult::Unknown => false,
    }
}

pub fn detect_redundant_constraints_ff<T: FieldElement>(
    mut machine: SymbolicMachine<T>,
) -> SymbolicMachine<T> {
    println!("Detecting redundant constraints");
    // Original constraint system should be SAT.
    match solve_ff(
        &machine,
        Default::default(),
        None,
        None,
        SmtBusInteractionConfig::None,
    )
    .0
    {
        SmtResult::Unsat => {
            panic!("Original system should be SAT.");
        }
        SmtResult::Sat => {
            // All good.
        }
        SmtResult::Unknown => {
            // Could not solve original system, assuming it is SAT.
            // It's better to continue than give up because UNSAT may be easier to prove in some
            // cases, so even if we can't prove that the original system is SAT, we could still
            // find redundant constraints.
        }
    }

    let decls = machine
        .main_columns()
        .map(|c|
                // Variable declarations.
                format!("{c}"))
        .collect::<BTreeSet<String>>();

    let mut redundant_set = Vec::new();
    for (i, p) in machine.constraints.iter().enumerate() {
        let mut test_machine = machine.clone();
        test_machine.constraints.remove(i);
        let extra = negate_constraints_smtlib2_ff(vec![p.clone()]);
        match solve_ff(
            &test_machine,
            decls.clone(),
            Some(extra),
            None,
            SmtBusInteractionConfig::None,
        )
        .0
        {
            SmtResult::Unsat => {
                println!("Constraint {i} is redundant: {p}");
                redundant_set.push(i);
            }
            SmtResult::Sat => {
                println!("Constraint {i} is NOT redundant: {p}");
            }

            SmtResult::Unknown => {
                println!("Could not solve for constraint {i}: {p}");
            }
        }
    }

    // Iterate from largest subsets to smallest.
    let mut redundant_sets = redundant_set.iter().map(|x| vec![*x]).collect::<Vec<_>>();
    for size in (2..=redundant_set.len()).rev() {
        for subset in redundant_set.iter().combinations(size) {
            let mut test_machine = machine.clone();
            let mut to_negate = Vec::new();
            for ip in subset.iter().rev() {
                let p = machine.constraints[**ip].clone();
                test_machine.constraints.remove(**ip);
                to_negate.push(p.clone());
            }
            let extra = negate_constraints_smtlib2_ff(to_negate);
            match solve_ff(
                &test_machine,
                decls.clone(),
                Some(extra),
                None,
                SmtBusInteractionConfig::None,
            )
            .0
            {
                SmtResult::Unsat => {
                    // println!("Constraints are redundant");
                    redundant_sets.push(subset.iter().map(|x| **x).collect());
                }
                SmtResult::Sat => {
                    // println!("Constraints are NOT redundant");
                }

                SmtResult::Unknown => {
                    // println!("Could not solve for constraints");
                }
            }
        }
    }

    redundant_sets.sort_by(|a, b| b.len().cmp(&a.len()));
    for ip in redundant_sets[0].iter().rev() {
        machine.constraints.remove(*ip);
    }

    machine
}
