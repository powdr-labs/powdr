use regex::Regex;
use std::collections::{BTreeMap, BTreeSet, HashMap};
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

    let output = Command::new("cvc5")
        .arg(file.path())
        .arg("--produce-models")
        .arg("--tlimit-per=2000")
        .output()
        .expect("Failed to run cvc5");

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

enum SmtResult {
    SAT,
    UNSAT,
    UNKNOWN,
}

fn solve(smt2: &SmtConstraints) -> SmtResult {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "{}", smt2.to_string()).unwrap();
    writeln!(file, "(check-sat)").unwrap();

    // let output = Command::new("z3")
    let output = Command::new("cvc5")
        .arg(file.path())
        .arg("--produce-models")
        .arg("--tlimit-per=2000")
        .output()
        // .expect("Failed to run z3");
        .expect("Failed to run cvc5");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    println!("Query stdout: {stdout}");
    println!("Query stderr: {stderr}");
    if stdout.contains("timeout") || stdout.contains("unknown") {
        SmtResult::UNKNOWN
    } else if stdout.contains("unsat") {
        SmtResult::UNSAT
    } else {
        SmtResult::SAT
    }
}

/// Check if a variable is uniquely determined by asserting it's not equal to its current value
fn is_unique(smt2: &String, var: &String, value: &String) -> bool {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "{smt2}\n(assert (not (= {var} {value})))").unwrap();
    writeln!(file, "(check-sat)").unwrap();

    let output = Command::new("cvc5")
        .arg(file.path())
        .output()
        .expect("Failed to run z3");

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout.contains("unsat")
}

#[derive(Debug, Clone)]
pub struct SmtConstraints {
    pub decls: Vec<String>,
    pub range_constraints: Vec<String>,
    pub poly_constraints: Vec<String>,
    pub poly_constraints_not_zero: Vec<String>,
}

const P: u32 = (1 << 31) - (1 << 27) + 1;
impl SmtConstraints {
    pub fn merge(mut self, other: SmtConstraints) -> Self {
        self.decls.extend(other.decls);
        self.range_constraints.extend(other.range_constraints);
        self.poly_constraints.extend(other.poly_constraints);
        self.poly_constraints_not_zero
            .extend(other.poly_constraints_not_zero);
        self
    }

    pub fn to_string(&self) -> String {
        let mut smt2 = String::new();
        smt2.push_str(&format!("(set-logic QF_FF)"));
        smt2.push('\n');
        smt2.push_str(&format!("(set-option :incremental true)"));
        smt2.push('\n');
        smt2.push_str(&format!("(define-sort BB () (_ FiniteField {P}))"));
        smt2.push('\n');
        for decl in &self.decls {
            let d = format!("(declare-fun {decl} () BB)");
            smt2.push_str(&d);
            smt2.push('\n');
        }
        // for range in &self.range_constraints {
        //     let a = format!("(assert {range})");
        //     smt2.push_str(&a);
        //     smt2.push('\n');
        // }
        for poly in &self.poly_constraints {
            let a = format!("(assert (= (as ff0 BB) {poly}))");
            smt2.push_str(&a);
            smt2.push('\n');
        }
        if !self.poly_constraints_not_zero.is_empty() {
            let inner = self
                .poly_constraints_not_zero
                .iter()
                .map(|poly| format!("(= (as ff0 BB) {poly})"))
                .collect::<Vec<_>>()
                .join(" ");
            let outer_neg = format!("(assert (not (and {inner})))");
            smt2.push_str(&outer_neg);
            smt2.push('\n');
        }

        smt2
    }

    pub fn from_decls(decls: Vec<String>) -> Self {
        SmtConstraints {
            decls,
            poly_constraints: Vec::new(),
            poly_constraints_not_zero: Vec::new(),
            range_constraints: Vec::new(),
        }
    }

    pub fn from_range_constraints(range_constraints: Vec<String>) -> Self {
        SmtConstraints {
            range_constraints,
            poly_constraints: Vec::new(),
            poly_constraints_not_zero: Vec::new(),
            decls: Vec::new(),
        }
    }

    pub fn from_poly_constraints(poly_constraints: Vec<String>) -> Self {
        SmtConstraints {
            poly_constraints,
            poly_constraints_not_zero: Vec::new(),
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
    let smt2 = smt2_constraints.to_string();

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

// Detects redundant constraints and returns subsets of contraints that are redundant together.
pub fn detect_redundant_constraints(smt2: SmtConstraints) -> Vec<Vec<usize>> {
    println!("Solving original system");
    // println!("{}", smt2.to_string());
    // match solve(&smt2) {
    //     SmtResult::UNSAT => {
    //         panic!("Original system should be SAT.");
    //     }
    //     SmtResult::SAT => {
    //         println!("Original system is SAT.");
    //     }
    //
    //     SmtResult::UNKNOWN => {
    //         println!("Could not solve original system, assuming it is SAT.");
    //     }
    // }
    let mut redundant_set = Vec::new();
    for (i, p) in smt2.poly_constraints.iter().enumerate() {
        let mut test_smt2 = smt2.clone();
        test_smt2.poly_constraints.remove(i);
        test_smt2.poly_constraints_not_zero.push(p.clone());
        println!("\n\n\nTesting constraint {p}...");
        println!("{}", test_smt2.to_string());
        match solve(&test_smt2) {
            SmtResult::UNSAT => {
                println!("Constraint {i} is redundant: {p}");
                redundant_set.push(i);
            }
            SmtResult::SAT => {
                println!("Constraint {i} is NOT redundant: {p}");
            }

            SmtResult::UNKNOWN => {
                println!("Could not solve for constraint {i}: {p}");
            }
        }
    }

    use itertools::Itertools;

    // Iterate from largest subsets to smallest
    let mut redundant_sets = redundant_set.iter().map(|x| vec![*x]).collect::<Vec<_>>();
    return redundant_sets;
    for size in (2..=redundant_set.len()).rev() {
        for subset in redundant_set.iter().combinations(size) {
            let mut test_smt2 = smt2.clone();
            for ip in subset.iter().rev() {
                let p = smt2.poly_constraints[**ip].clone();
                test_smt2.poly_constraints.remove(**ip);
                test_smt2.poly_constraints_not_zero.push(p.clone());
            }
            println!("\n\n\nTesting constraints {:?}...", subset);
            println!("{}", test_smt2.to_string());
            match solve(&test_smt2) {
                SmtResult::UNSAT => {
                    println!("Constraints are redundant");
                    redundant_sets.push(subset.iter().map(|x| **x).collect());
                }
                SmtResult::SAT => {
                    println!("Constraints are NOT redundant");
                }

                SmtResult::UNKNOWN => {
                    println!("Could not solve for constraints");
                }
            }
        }
    }

    redundant_sets
}

/// Compute Groebner basis of a SymbolicMachine's polynomial constraints
pub fn compute_groebner_basis<T: powdr_number::FieldElement + Clone + Ord + std::fmt::Display>(
    machine: &crate::SymbolicMachine<T>,
) -> Vec<crate::SymbolicConstraint<T>> {
    use crate::expression::AlgebraicReference;
    use std::collections::{BTreeMap, BTreeSet};

    println!(
        "Computing Groebner basis for {} constraints",
        machine.constraints.len()
    );

    if machine.constraints.is_empty() {
        println!("No polynomial constraints to process");
        return Vec::new();
    }

    // Use main_columns to get all variables with their IDs
    let variable_map: BTreeMap<String, AlgebraicReference> = machine
        .main_columns()
        .map(|col| ((*col.name).clone(), col))
        .collect();

    let variables: BTreeSet<String> = variable_map.keys().cloned().collect();

    println!(
        "Found {} constraints with {} variables",
        machine.constraints.len(),
        variables.len()
    );

    // Use max degree of 3
    const MAX_DEGREE: usize = 3;

    // Convert constraints to Python format and generate script
    let python_script = generate_groebner_script(&machine.constraints, &variables, MAX_DEGREE);
    println!("Python script:\n{python_script}\n");

    // Write Python script to temporary file
    let mut script_file = NamedTempFile::new().unwrap();
    writeln!(script_file, "{}", python_script).unwrap();

    // Execute Python script
    let output = Command::new("python3")
        .arg(script_file.path())
        .output()
        .expect("Failed to run Python for Groebner basis computation");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if !stderr.is_empty() {
        println!("Python stderr: {}", stderr);
    }

    println!("Python output:\n{}", stdout);

    // Parse the output polynomials
    let mut gb_constraints = Vec::new();

    for line in stdout.lines() {
        if let Some(poly_str) = line.strip_prefix("POLY:") {
            let poly_str = poly_str.trim();
            if !poly_str.is_empty() {
                if let Some(expr) = parse_sympy_polynomial::<T>(poly_str, &variable_map) {
                    gb_constraints.push(crate::SymbolicConstraint { expr });
                }
            }
        }
    }

    // Count eliminated variables
    let gb_variables: BTreeSet<String> = gb_constraints
        .iter()
        .flat_map(|c| {
            let mut vars = BTreeSet::new();
            collect_variables_from_expr(&c.expr, &mut vars);
            vars
        })
        .collect();

    let eliminated_vars: BTreeSet<_> = variables.difference(&gb_variables).collect();

    println!("\nGroebner basis computation complete:");
    println!("  Original constraints: {}", machine.constraints.len());
    println!("  Groebner basis size: {}", gb_constraints.len());
    println!("  Original variables: {}", variables.len());
    println!("  Variables in GB: {}", gb_variables.len());
    let eliminated_names = eliminated_vars
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(", ");

    println!(
        "  Eliminated variables: {} ({})",
        eliminated_vars.len(),
        eliminated_names
    );

    gb_constraints
}

/// Collect variable names from an AlgebraicExpression
fn collect_variables_from_expr<T>(
    expr: &crate::expression::AlgebraicExpression<T>,
    vars: &mut BTreeSet<String>,
) {
    use crate::expression::{AlgebraicExpression, AlgebraicReference};
    use powdr_expression::{AlgebraicBinaryOperation, AlgebraicUnaryOperation};

    match expr {
        AlgebraicExpression::Reference(AlgebraicReference { name, .. }) => {
            vars.insert((**name).clone());
        }
        AlgebraicExpression::Number(_) => {}
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, right, .. }) => {
            collect_variables_from_expr(left, vars);
            collect_variables_from_expr(right, vars);
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { expr, .. }) => {
            collect_variables_from_expr(expr, vars);
        }
    }
}

/// Parse a SymPy polynomial string back into AlgebraicExpression
fn parse_sympy_polynomial<T: powdr_number::FieldElement>(
    poly_str: &str,
    var_map: &BTreeMap<String, crate::expression::AlgebraicReference>,
) -> Option<crate::expression::AlgebraicExpression<T>> {
    // Tokenize and parse the polynomial
    let tokens = tokenize_polynomial(poly_str);
    parse_tokens::<T>(&tokens, var_map)
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(String),
    Variable(String),
    Plus,
    Minus,
    Star,
    Power,
    LParen,
    RParen,
}

fn tokenize_polynomial(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' => {
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '-' => {
                tokens.push(Token::Minus);
                chars.next();
            }
            '*' => {
                chars.next();
                if chars.peek() == Some(&'*') {
                    chars.next();
                    tokens.push(Token::Power);
                } else {
                    tokens.push(Token::Star);
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '0'..='9' => {
                let mut num = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_numeric() {
                        num.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num));
            }
            _ => {
                // Variable name
                let mut var = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        var.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !var.is_empty() {
                    tokens.push(Token::Variable(var));
                } else {
                    chars.next(); // Skip unknown character
                }
            }
        }
    }

    tokens
}

fn parse_tokens<T: powdr_number::FieldElement>(
    tokens: &[Token],
    var_map: &BTreeMap<String, crate::expression::AlgebraicReference>,
) -> Option<crate::expression::AlgebraicExpression<T>> {
    let mut parser = Parser::new(tokens, var_map);
    parser.parse_expression()
}

struct Parser<'a, T> {
    tokens: &'a [Token],
    pos: usize,
    var_map: &'a BTreeMap<String, crate::expression::AlgebraicReference>,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T: powdr_number::FieldElement> Parser<'a, T> {
    fn new(
        tokens: &'a [Token],
        var_map: &'a BTreeMap<String, crate::expression::AlgebraicReference>,
    ) -> Self {
        Parser {
            tokens,
            pos: 0,
            var_map,
            _phantom: std::marker::PhantomData,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn parse_expression(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        use crate::expression::AlgebraicExpression;
        use powdr_expression::AlgebraicBinaryOperator;
        let mut left = self.parse_multiplicative()?;

        while let Some(token) = self.current() {
            match token {
                Token::Plus => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    left =
                        AlgebraicExpression::new_binary(left, AlgebraicBinaryOperator::Add, right);
                }
                Token::Minus => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    left =
                        AlgebraicExpression::new_binary(left, AlgebraicBinaryOperator::Sub, right);
                }
                _ => break,
            }
        }

        Some(left)
    }

    fn parse_multiplicative(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        use crate::expression::AlgebraicExpression;
        use powdr_expression::AlgebraicBinaryOperator;
        let mut left = self.parse_power()?;

        while let Some(Token::Star) = self.current() {
            self.advance();
            let right = self.parse_power()?;
            left = AlgebraicExpression::new_binary(left, AlgebraicBinaryOperator::Mul, right);
        }

        Some(left)
    }

    fn parse_power(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        use crate::expression::AlgebraicExpression;
        use powdr_expression::AlgebraicBinaryOperator;
        let mut base = self.parse_unary()?;

        if let Some(Token::Power) = self.current() {
            self.advance();
            // For now, we'll expand powers by repeated multiplication
            if let Some(Token::Number(exp_str)) = self.current() {
                if let Ok(exp) = exp_str.parse::<u32>() {
                    self.advance();
                    // Expand power as repeated multiplication
                    for _ in 1..exp {
                        base = AlgebraicExpression::new_binary(
                            base.clone(),
                            AlgebraicBinaryOperator::Mul,
                            base.clone(),
                        );
                    }
                }
            }
        }

        Some(base)
    }

    fn parse_unary(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        use crate::expression::AlgebraicExpression;
        use powdr_expression::AlgebraicUnaryOperator;
        if let Some(Token::Minus) = self.current() {
            self.advance();
            let expr = self.parse_unary()?;
            return Some(AlgebraicExpression::new_unary(
                AlgebraicUnaryOperator::Minus,
                expr,
            ));
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Option<crate::expression::AlgebraicExpression<T>> {
        use crate::expression::AlgebraicExpression;
        match self.current()?.clone() {
            Token::Number(num_str) => {
                self.advance();
                let num = num_str.parse::<i64>().ok()?;
                Some(AlgebraicExpression::Number(T::from(num)))
            }
            Token::Variable(var_name) => {
                self.advance();
                let var_ref = self.var_map.get(&var_name)?;
                Some(AlgebraicExpression::Reference(var_ref.clone()))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                if let Some(Token::RParen) = self.current() {
                    self.advance();
                }
                Some(expr)
            }
            _ => None,
        }
    }
}

/// Convert AlgebraicExpression to Python/SymPy format
fn algebraic_to_python<T: powdr_number::FieldElement>(
    expr: &crate::expression::AlgebraicExpression<T>,
) -> String {
    use crate::expression::{AlgebraicExpression, AlgebraicReference};
    use powdr_expression::{
        AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
        AlgebraicUnaryOperator,
    };

    match expr {
        AlgebraicExpression::Number(n) => format!("{}", n),
        AlgebraicExpression::Reference(AlgebraicReference { name, .. }) => (**name).clone(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let left_str = algebraic_to_python(left);
            let right_str = algebraic_to_python(right);
            match op {
                AlgebraicBinaryOperator::Add => format!("({} + {})", left_str, right_str),
                AlgebraicBinaryOperator::Sub => format!("({} - {})", left_str, right_str),
                AlgebraicBinaryOperator::Mul => {
                    // Add parentheses for clarity when multiplying
                    let left_paren = match left.as_ref() {
                        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                            op: AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub,
                            ..
                        }) => true,
                        _ => false,
                    };
                    let right_paren = match right.as_ref() {
                        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                            op: AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub,
                            ..
                        }) => true,
                        _ => false,
                    };

                    let left_final = if left_paren {
                        format!("({})", left_str)
                    } else {
                        left_str
                    };
                    let right_final = if right_paren {
                        format!("({})", right_str)
                    } else {
                        right_str
                    };
                    format!("({} * {})", left_final, right_final)
                }
            }
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            let expr_str = algebraic_to_python(expr);
            match op {
                AlgebraicUnaryOperator::Minus => format!("(-{})", expr_str),
            }
        }
    }
}

/// Generate Python script for Groebner basis computation
fn generate_groebner_script<T: powdr_number::FieldElement>(
    constraints: &[crate::SymbolicConstraint<T>],
    variables: &BTreeSet<String>,
    max_degree: usize,
) -> String {
    let var_list = variables.iter().cloned().collect::<Vec<_>>().join(", ");

    let mut script = format!(
        r#"
from sympy import symbols, groebner, Poly, GF
from sympy.polys.domains import ZZ

# Define the prime field
p = 2013265921
field = GF(p)

# Define variables
{} = symbols('{}')

# Define polynomials
polys = []
"#,
        var_list, var_list
    );

    // Add each polynomial
    for constraint in constraints {
        let poly_str = algebraic_to_python(&constraint.expr);
        script.push_str(&format!("polys.append({})\n", poly_str));
    }

    script.push_str(&format!(
        r#"
# Compute truncated Groebner basis with degree bound
def truncated_groebner_basis(polys, vars, max_degree, field):
    """
    Compute a truncated Groebner basis that respects degree bounds.
    Uses a simple approach: compute reductions but skip high-degree results.
    """
    # Start with input polynomials that satisfy degree bound
    basis = []
    for p in polys:
        if p.total_degree() <= max_degree:
            basis.append(p)
    
    if not basis:
        return []
    
    # Simple reduction approach
    changed = True
    iterations = 0
    max_iterations = 10  # Prevent infinite loops
    
    while changed and iterations < max_iterations:
        changed = False
        iterations += 1
        new_polys = []
        
        # Try to reduce each pair
        for i in range(len(basis)):
            for j in range(i + 1, len(basis)):
                try:
                    # Try polynomial division
                    _, r = basis[i].div(basis[j])
                    if not r.is_zero and r.total_degree() <= max_degree:
                        # Check if this is genuinely new
                        is_new = True
                        for existing in basis + new_polys:
                            if r.rem(existing).is_zero:
                                is_new = False
                                break
                        if is_new:
                            new_polys.append(r)
                            changed = True
                    
                    # Try the other direction
                    _, r = basis[j].div(basis[i])
                    if not r.is_zero and r.total_degree() <= max_degree:
                        is_new = True
                        for existing in basis + new_polys:
                            if r.rem(existing).is_zero:
                                is_new = False
                                break
                        if is_new:
                            new_polys.append(r)
                            changed = True
                            
                except:
                    continue
        
        # Add new polynomials to basis
        basis.extend(new_polys)
        
        # Remove redundant polynomials
        if len(basis) > 20:  # Keep basis size manageable
            # Sort by degree and keep lowest degree ones
            basis.sort(key=lambda p: (p.total_degree(), len(str(p))))
            basis = basis[:15]
    
    # Final reduction pass
    final_basis = []
    for p in basis:
        if p.total_degree() <= max_degree and not p.is_zero:
            # Check if redundant
            is_redundant = False
            for q in final_basis:
                if p.rem(q).is_zero:
                    is_redundant = True
                    break
            if not is_redundant:
                final_basis.append(p)
    
    return final_basis

try:
    if len(polys) == 0:
        print("No polynomials to process")
    else:
        # Convert to polynomial objects over the field
        poly_objects = [Poly(p, [{}], domain=field) for p in polys]
        
        # Compute truncated Groebner basis with degree bound {}
        max_degree = {}
        gb = truncated_groebner_basis(poly_objects, [{}], max_degree, field)
        
        # Also compute full GB for comparison
        full_gb = groebner(poly_objects, [{}], domain=field)
        
        # Print results
        print(f"Original polynomials: {{len(polys)}}")
        print(f"Full Groebner basis size: {{len(full_gb)}} (max degree: {{max(p.total_degree() for p in full_gb) if full_gb else 0}})")
        print(f"Truncated GB size: {{len(gb)}} (max degree: {{max(p.total_degree() for p in gb) if gb else 0}})")
        
        # Verify equivalence: compute GB of truncated GB
        print("\\nVerifying equivalence...")
        if gb:
            gb_of_truncated = groebner(gb, [{}], domain=field)
            print(f"GB of truncated GB size: {{len(gb_of_truncated)}} (max degree: {{max(p.total_degree() for p in gb_of_truncated) if gb_of_truncated else 0}})")
            
            # Check if they generate the same ideal by comparing the full GBs
            # Two ideals are equal iff their reduced Groebner bases are equal
            full_gb_sorted = sorted([str(p.as_expr()) for p in full_gb])
            gb_of_truncated_sorted = sorted([str(p.as_expr()) for p in gb_of_truncated])
            
            if full_gb_sorted == gb_of_truncated_sorted:
                print("✓ Verification PASSED: Truncated GB generates the same ideal!")
            else:
                print("✗ Verification FAILED: Truncated GB generates a different ideal")
                print(f"  Full GB: {{len(full_gb_sorted)}} polynomials")
                print(f"  GB of truncated: {{len(gb_of_truncated_sorted)}} polynomials")
                
                # Additional check: verify each polynomial in full GB reduces to 0 modulo truncated GB
                all_reduce_to_zero = True
                for p in full_gb:
                    remainder = p
                    for q in gb:
                        remainder = remainder.rem(q)
                    if not remainder.is_zero:
                        all_reduce_to_zero = False
                        print(f"  Polynomial from full GB doesn't reduce to 0: {{p.as_expr()}}")
                        break
                
                if all_reduce_to_zero:
                    print("  However, all full GB polynomials reduce to 0 modulo truncated GB")
                    print("  This means truncated GB contains the original ideal (possibly larger)")
        
        for poly in gb:
            # Convert back to expression format
            expr = str(poly.as_expr())
            print(f"POLY:{{expr}}")
except Exception as e:
    import traceback
    print(f"ERROR: {{e}}")
    traceback.print_exc()
"#,
        var_list, max_degree, max_degree, var_list, var_list, var_list
    ));

    script
}
