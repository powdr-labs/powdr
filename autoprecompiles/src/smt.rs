use regex::Regex;
use std::collections::{BTreeSet, HashMap};
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

/// Query Z3 for the values of specific variables
fn get_values(smt2: &String, vars: &BTreeSet<String>) -> Option<HashMap<String, String>> {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "{smt2}").unwrap();
    writeln!(file, "(check-sat)").unwrap();

    let var_list = vars.iter().cloned().collect::<Vec<String>>().join(" ");
    writeln!(file, "(get-value ({var_list}))").unwrap();

    let output = Command::new("z3")
        .arg(file.path())
        .output()
        .expect("Failed to run z3");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    println!("Query stdout: {stdout}");
    println!("Query stderr: {stderr}");
    if stdout.contains("unsat") {
        return None;
    }

    let mut values = HashMap::new();
    let re = Regex::new(r"\(\s*(\w+)\s+([^\)]+)\)").unwrap();
    for cap in re.captures_iter(&stdout) {
        values.insert(cap[1].to_string(), cap[2].trim().to_string());
    }

    Some(values)
}

/// Check if a variable is uniquely determined by asserting it's not equal to its current value
fn is_unique(smt2: &String, var: &String, value: &String) -> bool {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "{smt2}\n(assert (not (= {var} {value})))").unwrap();
    writeln!(file, "(check-sat)").unwrap();

    let output = Command::new("z3")
        .arg(file.path())
        .output()
        .expect("Failed to run z3");

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout.contains("unsat")
}

pub struct SmtConstraints {
    pub decls: Vec<String>,
    pub range_constraints: Vec<String>,
    pub poly_constraints: Vec<String>,
}

impl SmtConstraints {
    pub fn merge(mut self, other: SmtConstraints) -> Self {
        self.decls.extend(other.decls);
        self.range_constraints.extend(other.range_constraints);
        self.poly_constraints.extend(other.poly_constraints);
        self
    }

    pub fn from_decls(decls: Vec<String>) -> Self {
        SmtConstraints {
            decls,
            poly_constraints: Vec::new(),
            range_constraints: Vec::new(),
        }
    }

    pub fn from_range_constraints(range_constraints: Vec<String>) -> Self {
        SmtConstraints {
            range_constraints,
            poly_constraints: Vec::new(),
            decls: Vec::new(),
        }
    }

    pub fn from_poly_constraints(poly_constraints: Vec<String>) -> Self {
        SmtConstraints {
            poly_constraints,
            range_constraints: Vec::new(),
            decls: Vec::new(),
        }
    }
}

/// Return only uniquely determined variables and their values
pub fn get_unique_vars(
    smt2_constraints: SmtConstraints,
    vars: &BTreeSet<String>,
) -> HashMap<String, String> {
    let mut smt2: Vec<String> = Vec::new();
    smt2.extend(smt2_constraints.decls);
    smt2.extend(smt2_constraints.range_constraints);
    smt2.extend(smt2_constraints.poly_constraints);

    let smt2 = smt2.join("\n");
    println!("Get unique vars");
    println!("SMT:\n{smt2}");
    let Some(values) = get_values(&smt2, vars) else {
        println!("Original constraints unsat");
        return HashMap::new();
    };
    println!("Model: {vars:?}");

    vars.iter()
        .filter_map(|var| {
            println!("Testing var {var}");
            let Some(val) = values.get(var) else {
                println!("Var {var} not found in first model");
                return None;
            };
            if is_unique(&smt2, var, val) {
                println!("Var {var} is unique = {val}");
                Some((var.to_string(), val.to_string()))
            } else {
                println!("Var {var} is not unique");
                None
            }
        })
        .collect()
}
