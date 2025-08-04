use itertools::Itertools;
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

    let (res, stdout, _stderr) = solve_ff(machine, Default::default(), None, Some(extra));

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
    let (res, _stdout, _stderr) = solve_ff(machine, Default::default(), Some(extra), None);
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
) -> (SmtResult, String, String) {
    let mut smt2 = symbolic_machine_to_smtlib2_ff(machine, decls);

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
        .arg("--tlimit-per=2000")
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

fn solve_int<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    decls: BTreeSet<String>,
    extra_before_check: Option<String>,
    extra_after_check: Option<String>,
) -> (SmtResult, String, String) {
    let mut smt2 = symbolic_machine_to_smtlib2_int(machine, decls);

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

    let output = Command::new("z3")
        .arg(file.path())
        .arg("-T:2")
        .output()
        .expect("Failed to run z3");

    // let output = Command::new("cvc5")
    //     .arg(file.path())
    //     .arg("--produce-models")
    //     .arg("--tlimit-per=2000")
    //     .output()
    //     .expect("Failed to run cvc5");

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

fn algebraic_to_smt_int<T: FieldElement>(expr: &AlgebraicExpression<T>) -> String {
    match expr {
        AlgebraicExpression::Number(x) => format!("{x}"),
        AlgebraicExpression::Reference(AlgebraicReference { name, .. }) => (**name).clone(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left = algebraic_to_smt_int(left);
            let right = algebraic_to_smt_int(right);
            match op {
                AlgebraicBinaryOperator::Add => format!("(+ {left} {right})"),
                AlgebraicBinaryOperator::Sub => format!("(- {left} {right})"),
                AlgebraicBinaryOperator::Mul => format!("(* {left} {right})"),
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr = algebraic_to_smt_ff(expr);
            let op_str = match op {
                AlgebraicUnaryOperator::Minus => "-",
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

fn symbolic_bus_interaction_to_smt_int<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<String> {
    if b.id == 3 {
        vec![range_check_interaction_to_smt(b)]
    } else if b.id == 6 {
        byte_check_interaction_to_smt(b)
    } else if b.id == 7 {
        tuple_range_check_interaction_to_smt(b)
    } else {
        vec![]
    }
}

fn range_check_interaction_to_smt<T: FieldElement>(b: &SymbolicBusInteraction<T>) -> String {
    assert_eq!(b.id, 3);
    assert_eq!(b.args.len(), 2);
    let v = algebraic_to_smt_int(&b.args[0]);
    let bits = try_algebraic_number(&b.args[1])
        .unwrap()
        .to_integer()
        .try_into_u32()
        .unwrap();
    let max_range = 1 << bits;
    range_check_to_smt(v, max_range)
}

fn tuple_range_check_interaction_to_smt<T: FieldElement>(
    b: &SymbolicBusInteraction<T>,
) -> Vec<String> {
    assert_eq!(b.id, 7);
    assert_eq!(b.args.len(), 2);
    let v1 = algebraic_to_smt_int(&b.args[0]);
    let v2 = algebraic_to_smt_int(&b.args[1]);
    vec![byte_check_to_smt(v1), byte_check_to_smt(v2)]
}

fn byte_check_interaction_to_smt<T: FieldElement>(b: &SymbolicBusInteraction<T>) -> Vec<String> {
    assert_eq!(b.id, 6);
    assert_eq!(b.args.len(), 4);
    let v1 = algebraic_to_smt_int(&b.args[0]);
    let v2 = algebraic_to_smt_int(&b.args[1]);
    let v3 = algebraic_to_smt_int(&b.args[2]);
    let v4 = algebraic_to_smt_int(&b.args[3]);
    vec![
        byte_check_to_smt(v1),
        byte_check_to_smt(v2),
        byte_check_to_smt(v3),
        byte_check_to_smt(v4),
    ]
}

fn range_check_to_smt(v: String, max_range: u64) -> String {
    format!("(and (>= {v} 0) (< {v} {max_range}))")
}

fn byte_check_to_smt(v: String) -> String {
    range_check_to_smt(v, 256)
}

#[derive(Debug)]
enum SmtResult {
    Sat,
    Unsat,
    Unknown,
}

const P: u32 = (1 << 31) - (1 << 27) + 1;

fn symbolic_machine_to_smtlib2_ff<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    decls: BTreeSet<String>,
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

    println!("this_decls: {this_decls:?}");
    let decls = this_decls
        .into_iter()
        .map(|c| format!("(declare-fun {c} () BB)"))
        .collect::<Vec<String>>()
        .join("\n");

    smt2.push_str(&decls);
    smt2.push('\n');

    // We cannot encode range constraints into the SMT Finite Field theory.

    for poly in &machine.constraints {
        let c_smt = algebraic_to_smt_ff(&poly.expr);
        let a = format!("(assert (= (as ff0 BB) {c_smt}))");
        smt2.push_str(&a);
        smt2.push('\n');
    }

    smt2
}

fn symbolic_machine_to_smtlib2_int<T: FieldElement>(
    machine: &SymbolicMachine<T>,
    decls: BTreeSet<String>,
) -> String {
    let mut smt2 = String::new();
    // smt2.push_str("(set-logic QF_FF)\n");
    // smt2.push_str("(set-option :incremental true)\n");
    // smt2.push_str(&format!("(define-sort BB () (_ FiniteField {P}))\n"));

    let mut this_decls = machine
        .main_columns()
        .map(|c|
                // Variable declarations.
                format!("{c}"))
        .collect::<BTreeSet<String>>();

    this_decls.extend(decls);

    println!("this_decls: {this_decls:?}");
    let decls = this_decls
        .into_iter()
        .map(|c| format!("(declare-fun {c} () Int)"))
        .collect::<Vec<String>>()
        .join("\n");

    smt2.push_str(&decls);
    smt2.push('\n');

    for bus_int in &machine.bus_interactions {
        let bus_smt = symbolic_bus_interaction_to_smt_int(bus_int);
        if !bus_smt.is_empty() {
            let bus_smt = bus_smt.join(" ");
            let a = format!("(assert (and {bus_smt}))");
            smt2.push_str(&a);
            smt2.push('\n');
        }
    }

    // TODO not trying constraints mod P right now
    // for poly in &machine.constraints {
    //     let c_smt = algebraic_to_smt_int(&poly.expr);
    //     let a = format!("(assert (= 0 (mod {c_smt} {P})))");
    //     smt2.push_str(&a);
    //     smt2.push('\n');
    // }

    smt2
}

fn negate_constraints_smtlib2_int<T: FieldElement>(
    constraints: Vec<SymbolicConstraint<T>>,
) -> String {
    let inner = constraints
        .iter()
        .map(|poly| format!("(= 0 {})", algebraic_to_smt_int(&poly.expr)))
        .collect::<Vec<_>>()
        .join(" ");
    format!("(assert (not (and {inner})))")
}

fn negate_range_constraints_smtlib2_int<T: FieldElement>(
    bus_interactions: &[SymbolicBusInteraction<T>],
) -> String {
    let inner = bus_interactions
        .iter()
        .flat_map(|bus_int| symbolic_bus_interaction_to_smt_int(bus_int))
        .collect::<Vec<_>>()
        .join(" ");
    format!("(assert (not (and {inner})))")
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

pub fn detect_redundant_constraints_ff<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> Vec<Vec<usize>> {
    println!("machine:\n{machine}");
    // Original constraint system should be SAT.
    match solve_ff(machine, Default::default(), None, None).0 {
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
        match solve_ff(&test_machine, decls.clone(), Some(extra), None).0 {
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
            match solve_ff(&test_machine, decls.clone(), Some(extra), None).0 {
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

    redundant_sets
}

// Detects redundant constraints and returns subsets of contraints that are redundant together.
pub fn detect_redundant_constraints_int<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> Vec<Vec<usize>> {
    println!("machine:\n{machine}");
    // Original constraint system should be SAT.
    match solve_int(machine, Default::default(), None, None).0 {
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
        let extra = negate_constraints_smtlib2_int(vec![p.clone()]);
        match solve_int(&test_machine, decls.clone(), Some(extra), None).0 {
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
            let extra = negate_constraints_smtlib2_int(to_negate);
            match solve_int(&test_machine, decls.clone(), Some(extra), None).0 {
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

    redundant_sets
}

pub fn detect_redundant_bus_interactions_int<T: FieldElement>(
    machine: &SymbolicMachine<T>,
) -> Vec<Vec<usize>> {
    println!("machine:\n{machine}");
    // Original constraint system should be SAT.
    match solve_int(machine, Default::default(), None, None).0 {
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
    for (i, p) in machine
        .bus_interactions
        .iter()
        .enumerate()
        .filter(|(_, b)| is_range_constraint(b))
    {
        let mut test_machine = machine.clone();
        test_machine.bus_interactions.remove(i);
        assert!(is_range_constraint(p));
        let extra = negate_range_constraints_smtlib2_int(&[p.clone()]);
        match solve_int(&test_machine, decls.clone(), Some(extra), None).0 {
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
                let p = machine.bus_interactions[**ip].clone();
                assert!(is_range_constraint(&p));
                test_machine.bus_interactions.remove(**ip);
                to_negate.push(p.clone());
            }
            let extra = negate_range_constraints_smtlib2_int(&to_negate);
            match solve_ff(&test_machine, decls.clone(), Some(extra), None).0 {
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

    redundant_sets
}
