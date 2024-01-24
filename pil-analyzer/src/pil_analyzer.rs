use std::collections::{HashMap, HashSet};

use std::fs;
use std::iter::once;
use std::path::{Path, PathBuf};

use powdr_ast::parsed::asm::{AbsoluteSymbolPath, SymbolPath};
use powdr_ast::parsed::{PILFile, PilStatement};
use powdr_number::{DegreeType, FieldElement};

use powdr_ast::analyzed::{
    Analyzed, Expression, FunctionValueDefinition, Identity, PublicDeclaration,
    StatementIdentifier, Symbol,
};

use crate::AnalysisDriver;

use crate::statement_processor::{Counters, PILItem, StatementProcessor};
use crate::{condenser, evaluator, expression_processor::ExpressionProcessor};

pub fn process_pil_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let files = import_all_dependencies(path);

    let mut analyzer = PILAnalyzer::new();
    analyzer.process(files);
    analyzer.condense()
}

pub fn process_pil_ast<T: FieldElement>(pil_file: PILFile<T>) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process(vec![pil_file]);
    analyzer.condense()
}

pub fn process_pil_file_contents<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let pil_file = powdr_parser::parse(Some("input"), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });

    process_pil_ast(pil_file)
}

#[derive(Default)]
struct PILAnalyzer<T> {
    known_symbols: HashSet<String>,
    current_namespace: AbsoluteSymbolPath,
    polynomial_degree: Option<DegreeType>,
    definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)>,
    public_declarations: HashMap<String, PublicDeclaration>,
    identities: Vec<Identity<Expression<T>>>,
    /// The order in which definitions and identities
    /// appear in the source.
    source_order: Vec<StatementIdentifier>,
    symbol_counters: Option<Counters>,
}

/// Reads and parses the given path and all its imports.
fn import_all_dependencies<T: FieldElement>(path: &Path) -> Vec<PILFile<T>> {
    let mut processed = Default::default();
    import_all_dependencies_internal(path, &mut processed)
}

fn import_all_dependencies_internal<T: FieldElement>(
    path: &Path,
    processed: &mut HashSet<PathBuf>,
) -> Vec<PILFile<T>> {
    let path = path
        .canonicalize()
        .unwrap_or_else(|e| panic!("File {path:?} not found: {e}"));
    if !processed.insert(path.clone()) {
        return vec![];
    }

    let contents = fs::read_to_string(path.clone()).unwrap();

    let ast = powdr_parser::parse(Some(path.to_str().unwrap()), &contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });

    // Filter out non-includes and compute the relative paths of includes.
    let (non_includes, includes) = ast.0.into_iter().fold(
        (vec![], vec![]),
        |(mut non_includes, mut included_paths), s| {
            match s {
                PilStatement::Include(_, include) => {
                    included_paths.push(path.parent().unwrap().join(include));
                }
                _ => non_includes.push(s),
            }
            (non_includes, included_paths)
        },
    );
    // Process includes and add the file itself.
    includes
        .into_iter()
        .flat_map(|path| import_all_dependencies_internal(&path, processed))
        .chain(once(PILFile(non_includes)))
        .collect::<Vec<_>>()
}

impl<T: FieldElement> PILAnalyzer<T> {
    pub fn new() -> PILAnalyzer<T> {
        PILAnalyzer {
            symbol_counters: Some(Default::default()),
            ..Default::default()
        }
    }

    pub fn process(&mut self, files: Vec<PILFile<T>>) {
        for PILFile(file) in &files {
            self.current_namespace = Default::default();
            for statement in file {
                self.collect_names(statement);
            }
        }

        for PILFile(file) in files {
            self.current_namespace = Default::default();
            for statement in file {
                self.handle_statement(statement);
            }
        }
    }

    pub fn condense(self) -> Analyzed<T> {
        condenser::condense(
            self.polynomial_degree,
            self.definitions,
            self.public_declarations,
            &self.identities,
            self.source_order,
        )
    }

    /// A step to collect all defined names in the statement.
    fn collect_names(&mut self, statement: &PilStatement<T>) {
        match statement {
            PilStatement::Namespace(_, name, _) => {
                self.current_namespace = AbsoluteSymbolPath::default().join(name.clone());
            }
            PilStatement::Include(_, _) => unreachable!(),
            _ => {
                for name in statement.symbol_definition_names() {
                    let absolute_name = self.driver().resolve_decl(name);
                    if !self.known_symbols.insert(absolute_name.clone()) {
                        panic!("Duplicate symbol definition: {absolute_name}");
                    }
                }
            }
        }
    }

    fn handle_statement(&mut self, statement: PilStatement<T>) {
        match statement {
            PilStatement::Include(_, _) => unreachable!(),
            PilStatement::Namespace(_, name, degree) => self.handle_namespace(name, degree),
            _ => {
                // We need a mutable reference to the counter, but it is short-lived.
                let mut counters = self.symbol_counters.take().unwrap();
                let items =
                    StatementProcessor::new(self.driver(), &mut counters, self.polynomial_degree)
                        .handle_statement(statement);
                self.symbol_counters = Some(counters);
                for item in items {
                    match item {
                        PILItem::Definition(symbol, value) => {
                            let name = symbol.absolute_name.clone();
                            let is_new = self
                                .definitions
                                .insert(name.clone(), (symbol, value))
                                .is_none();
                            assert!(is_new, "{name} already defined.");
                            self.source_order
                                .push(StatementIdentifier::Definition(name));
                        }
                        PILItem::PublicDeclaration(decl) => {
                            let name = decl.name.clone();
                            self.public_declarations.insert(name.clone(), decl);
                            self.source_order
                                .push(StatementIdentifier::PublicDeclaration(name));
                        }
                        PILItem::Identity(identity) => {
                            let index = self.identities.len();
                            self.source_order.push(StatementIdentifier::Identity(index));
                            self.identities.push(identity)
                        }
                    }
                }
            }
        }
    }

    fn handle_namespace(&mut self, name: SymbolPath, degree: ::powdr_ast::parsed::Expression<T>) {
        // TODO: the polynomial degree should be handled without going through a field element. This requires having types in Expression
        let degree = ExpressionProcessor::new(self.driver()).process_expression(degree);
        let namespace_degree = evaluator::evaluate_expression(&degree, &self.definitions)
            .unwrap()
            .try_to_number()
            .unwrap()
            .to_degree();
        if let Some(degree) = self.polynomial_degree {
            assert_eq!(
                degree, namespace_degree,
                "all namespaces must have the same degree"
            );
        } else {
            self.polynomial_degree = Some(namespace_degree);
        }
        self.current_namespace = AbsoluteSymbolPath::default().join(name);
    }

    fn driver(&self) -> Driver<T> {
        Driver(self)
    }
}

#[derive(Clone, Copy)]
struct Driver<'a, T>(&'a PILAnalyzer<T>);

impl<'a, T: FieldElement> AnalysisDriver<T> for Driver<'a, T> {
    fn resolve_decl(&self, name: &str) -> String {
        (if name.starts_with('%') {
            // Constants are not namespaced
            AbsoluteSymbolPath::default()
        } else {
            self.0.current_namespace.clone()
        })
        .with_part(name)
        .to_dotted_string()
    }

    fn resolve_ref(&self, path: &SymbolPath) -> String {
        // Try to resolve the name starting at the current namespace and then
        // go up level by level until the root.

        self.0
            .current_namespace
            .iter_to_root()
            .find_map(|prefix| {
                let path = prefix.join(path.clone()).to_dotted_string();
                self.0.known_symbols.contains(&path).then_some(path)
            })
            .unwrap_or_else(|| panic!("Symbol not found: {}", path.to_dotted_string()))
    }

    fn definitions(&self) -> &HashMap<String, (Symbol, Option<FunctionValueDefinition<T>>)> {
        &self.0.definitions
    }
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use test_log::test;

    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_print_analyzed() {
        // This is rather a test for the Display trait than for the analyzer.
        let input = r#"constant %N = 65536;
    public P = T.pc(2);
namespace Bin(65536);
    col witness bla;
namespace T(65536);
    col fixed first_step = [1] + [0]*;
    col fixed line(i) { i };
    col fixed ops(i) { ((i < 7) && (6 >= !i)) };
    col witness pc;
    col witness XInv;
    col witness XIsZero;
    T.XIsZero = (1 - (T.X * T.XInv));
    (T.XIsZero * T.X) = 0;
    (T.XIsZero * (1 - T.XIsZero)) = 0;
    col witness instr_jmpz;
    col witness instr_jmpz_param_l;
    col witness instr_jmp;
    col witness instr_jmp_param_l;
    col witness instr_dec_CNT;
    col witness instr_assert_zero;
    (T.instr_assert_zero * (T.XIsZero - 1)) = 0;
    col witness X;
    col witness X_const;
    col witness X_read_free;
    col witness A;
    col witness CNT;
    col witness read_X_A;
    col witness read_X_CNT;
    col witness reg_write_X_CNT;
    col witness read_X_pc;
    col witness reg_write_X_A;
    T.X = ((((T.read_X_A * T.A) + (T.read_X_CNT * T.CNT)) + T.X_const) + (T.X_read_free * T.X_free_value));
    T.A' = (((T.first_step' * 0) + (T.reg_write_X_A * T.X)) + ((1 - (T.first_step' + T.reg_write_X_A)) * T.A));
    col witness X_free_value(i) query match T.pc { 0 => ("input", 1), 3 => ("input", (T.CNT(i) + 1)), 7 => ("input", 0), };
    col fixed p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_X_read_free = [1, 0, 0, 1, 0, 0, 0, -1, 0] + [0]*;
    col fixed p_read_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 1] + [0]*;
    col fixed p_read_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_reg_write_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 0] + [0]*;
    col fixed p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    { T.pc, T.reg_write_X_A, T.reg_write_X_CNT } in (1 - T.first_step) { T.line, T.p_reg_write_X_A, T.p_reg_write_X_CNT };
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(input, formatted);
    }

    #[test]
    fn intermediate() {
        let input = r#"namespace N(65536);
    col witness x;
    col intermediate = x;
    intermediate = intermediate;
"#;
        let expected = r#"namespace N(65536);
    col witness x;
    col intermediate = N.x;
    N.intermediate = N.intermediate;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn intermediate_nested() {
        let input = r#"namespace N(65536);
    col witness x;
    col intermediate = x;
    col int2 = intermediate;
    col int3 = int2 + intermediate;
    int3 = 2 * x;
"#;
        let expected = r#"namespace N(65536);
    col witness x;
    col intermediate = N.x;
    col int2 = N.intermediate;
    col int3 = (N.int2 + N.intermediate);
    N.int3 = (2 * N.x);
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn let_definitions() {
        let input = r#"constant %r = 65536;
namespace N(%r);
    let x;
    let z = 2;
    let t = |i| i + z;
    let other = [1, z];
    let other_fun = |i, j| (i + 7, (|k| k - i));
"#;
        let expected = r#"constant %r = 65536;
namespace N(65536);
    col witness x;
    constant z = 2;
    col fixed t(i) { (i + N.z) };
    let other = [1, N.z];
    let other_fun = (|i, j| ((i + 7), (|k| (k - i))));
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn reparse_arrays() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y[1] - 2) = 0;
    (N.y[2]' - 2) = 0;
    public out = N.y[1](2);
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Operator - not supported on types"]
    fn no_direct_array_references() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y - 2) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Tried to access element 3 of array of size 3"]
    fn no_out_of_bounds() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y[3] - 2) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    fn namespaced_call() {
        let input = r#"namespace Assembly(2);
    col fixed A = [0]*;
    col fixed C(i) { (Assembly.A((i + 2)) + 3) };
    col fixed D(i) { Assembly.C((i + 3)) };
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    fn if_expr() {
        let input = r#"namespace Assembly(2);
    col fixed A = [0]*;
    col fixed C(i) { if (i < 3) { Assembly.A(i) } else { (i + 9) } };
    col fixed D(i) { if Assembly.C(i) { 3 } else { 2 } };
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    fn symbolic_functions() {
        let input = r#"namespace N(16);
    let last_row = 15;
    let ISLAST = |i| match i { last_row => 1, _ => 0 };
    let x;
    let y;
    let constrain_equal_expr = |A, B| A - B;
    let on_regular_row = |cond| (1 - ISLAST) * cond;
    on_regular_row(constrain_equal_expr(x', y)) = 0;
    on_regular_row(constrain_equal_expr(y', x + y)) = 0;
    "#;
        let expected = r#"namespace N(16);
    constant last_row = 15;
    col fixed ISLAST(i) { match i { N.last_row => 1, _ => 0, } };
    col witness x;
    col witness y;
    let constrain_equal_expr = (|A, B| (A - B));
    col fixed on_regular_row(cond) { ((1 - N.ISLAST) * cond) };
    ((1 - N.ISLAST) * (N.x' - N.y)) = 0;
    ((1 - N.ISLAST) * (N.y' - (N.x + N.y))) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn next_op_on_param() {
        let input = r#"namespace N(16);
    let last_row = 15;
    let ISLAST = |i| match i { last_row => 1, _ => 0 };
    let x;
    let y;
    let next_is_seven = |t| t' - 7;
    next_is_seven(y) = 0;
    "#;
        let expected = r#"namespace N(16);
    constant last_row = 15;
    col fixed ISLAST(i) { match i { N.last_row => 1, _ => 0, } };
    col witness x;
    col witness y;
    col fixed next_is_seven(t) { (t' - 7) };
    (N.y' - 7) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn fixed_concrete_and_symbolic() {
        let input = r#"namespace N(16);
    let last_row = 15;
    let ISLAST = |i| match i { last_row => 1, _ => 0, };
    let x;
    let y;
    y - ISLAST(3) = 0;
    x - ISLAST = 0;
    "#;
        let expected = r#"namespace N(16);
    constant last_row = 15;
    col fixed ISLAST(i) { match i { N.last_row => 1, _ => 0, } };
    col witness x;
    col witness y;
    (N.y - 0) = 0;
    (N.x - N.ISLAST) = 0;
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn parentheses_lambda() {
        let input = r#"namespace N(16);
    let w = || 2;
    let x = (|i| || w())(2)();
    "#;
        let expected = r#"namespace N(16);
    let w = (|| 2);
    constant x = (|i| (|| N.w()))(2)();
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }
}
