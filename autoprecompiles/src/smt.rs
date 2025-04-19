use regex::Regex;
use std::collections::{BTreeSet, HashMap};
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

/// Query Z3 for the values of specific variables
fn get_values(smt2: &String, vars: &BTreeSet<String>) -> Option<HashMap<String, String>> {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "{}", smt2).unwrap();
    writeln!(file, "(check-sat)").unwrap();

    let var_list = vars
        .iter()
        .map(|s| s.clone())
        .collect::<Vec<String>>()
        .join(" ");
    writeln!(file, "(get-value ({}))", var_list).unwrap();

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
    writeln!(file, "{}\n(assert (not (= {} {})))", smt2, var, value).unwrap();
    writeln!(file, "(check-sat)").unwrap();

    let output = Command::new("z3")
        .arg(file.path())
        .output()
        .expect("Failed to run z3");

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout.contains("unsat")
}

/// Return only uniquely determined variables and their values
pub fn get_unique_vars(smt2: &String, vars: &BTreeSet<String>) -> HashMap<String, String> {
    println!("Get unique vars");
    let Some(values) = get_values(smt2, vars) else {
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
            if is_unique(smt2, &var, val) {
                println!("Var {var} is unique = {val}");
                Some((var.to_string(), val.to_string()))
            } else {
                println!("Var {var} is not unique");
                None
            }
        })
        .collect()
}
