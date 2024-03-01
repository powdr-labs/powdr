use std::collections::{HashMap, HashSet};

use std::fs;
use std::iter::once;
use std::path::{Path, PathBuf};

use powdr_ast::analyzed::types::{Type, TypedExpression};
use powdr_ast::parsed::asm::{AbsoluteSymbolPath, SymbolPath};

use powdr_ast::parsed::{PILFile, PilStatement};
use powdr_number::{DegreeType, FieldElement};

use powdr_ast::analyzed::{
    type_from_definition, Analyzed, Expression, FunctionValueDefinition, Identity, IdentityKind,
    PublicDeclaration, StatementIdentifier, Symbol,
};

use crate::type_inference::{infer_types, ExpectedType};
use crate::AnalysisDriver;

use crate::statement_processor::{Counters, PILItem, StatementProcessor};
use crate::{condenser, evaluator, expression_processor::ExpressionProcessor};

pub fn analyze_file<T: FieldElement>(path: &Path) -> Analyzed<T> {
    let files = import_all_dependencies(path);
    analyze(files)
}

pub fn analyze_ast<T: FieldElement>(pil_file: PILFile<T>) -> Analyzed<T> {
    analyze(vec![pil_file])
}

pub fn analyze_string<T: FieldElement>(contents: &str) -> Analyzed<T> {
    let pil_file = powdr_parser::parse(Some("input"), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .pil file:");
        err.output_to_stderr();
        panic!();
    });
    analyze(vec![pil_file])
}

fn analyze<T: FieldElement>(files: Vec<PILFile<T>>) -> Analyzed<T> {
    let mut analyzer = PILAnalyzer::new();
    analyzer.process(files);
    analyzer.type_check();
    analyzer.condense()
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

    pub fn type_check(&mut self) {
        let mut expressions = vec![];
        // Collect all definitions with their types and expressions.
        // For Arrays, we also collect the inner expressions and expect them to be field elements.
        let definitions = self
            .definitions
            .iter_mut()
            .map(|(name, (symbol, value))| {
                let (type_scheme, expr) =
                    if let Some(FunctionValueDefinition::Expression(TypedExpression {
                        type_scheme,
                        e,
                    })) = value
                    {
                        (type_scheme.clone(), Some(e))
                    } else {
                        let type_scheme = type_from_definition(symbol, value);

                        // TOOD in order to type-check queries, we need enums.
                        if let Some(FunctionValueDefinition::Array(items)) = value {
                            // Expect all items in the arrays to be field elements.
                            expressions.extend(
                                items
                                    .iter_mut()
                                    .flat_map(|item| item.pattern_mut())
                                    .map(|e| (e, Type::Fe.into())),
                            );
                        };
                        (type_scheme, None)
                    };
                (name.clone(), (type_scheme, expr))
            })
            .collect();
        // Collect all expressions in identities.
        for id in &mut self.identities {
            if id.kind == IdentityKind::Polynomial {
                // At statement level, we allow constr or constr[].
                expressions.push((
                    id.expression_for_poly_id_mut(),
                    ExpectedType {
                        ty: Type::Constr,
                        allow_array: true,
                    },
                ));
            } else {
                for part in [&mut id.left, &mut id.right] {
                    if let Some(selector) = &mut part.selector {
                        expressions.push((selector, Type::Expr.into()))
                    }
                    for e in &mut part.expressions {
                        expressions.push((e, Type::Expr.into()))
                    }
                }
            }
        }

        let inferred_types = infer_types(definitions, &mut expressions)
            .map_err(|e| {
                eprintln!("\nError during type inference:\n{e}");
                e
            })
            .unwrap();
        // Store the inferred types.
        for (name, ty) in inferred_types {
            let Some(FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: ts @ None,
                e: _,
            })) = &mut self.definitions.get_mut(&name).unwrap().1
            else {
                panic!()
            };
            *ts = Some(ty.into());
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
        let degree = ExpressionProcessor::new(self.driver()).process_expression(degree);
        let namespace_degree: u64 = u64::try_from(
            evaluator::evaluate_expression(&degree, &self.definitions)
                .unwrap()
                .try_to_integer()
                .unwrap(),
        )
        .unwrap();
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
    let ops: int -> bool = (|i| ((i < 7) && (6 >= -i)));
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
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
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
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
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
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn let_definitions() {
        let input = r#"constant %r = 65536;
namespace N(%r);
    let x;
    let z: int = 2;
    let t: col = |i| i + z;
    let other = [1, z];
    let other_fun: int, fe -> (int, (int -> int)) = |i, j| (i + 7, (|k| k - i));
"#;
        let expected = r#"constant %r = 65536;
namespace N(65536);
    col witness x;
    let z: int = 2;
    col fixed t(i) { (i + N.z) };
    let other: int[] = [1, N.z];
    let other_fun: int, fe -> (int, (int -> int)) = (|i, j| ((i + 7), (|k| (k - i))));
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn reparse_arrays() {
        let input = r#"public out = N.y[1](2);
namespace N(16);
    col witness y[3];
    (N.y[1] - 2) = 0;
    (N.y[2]' - 2) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Type expr[] does not satisfy trait Sub."]
    fn no_direct_array_references() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y - 2) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Tried to access element 3 of array of size 3"]
    fn no_out_of_bounds() {
        let input = r#"namespace N(16);
    col witness y[3];
    (N.y[3] - 2) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    fn namespaced_call() {
        let input = r#"namespace Assembly(2);
    let A: int -> int = (|i| 0);
    let C = (|i| (Assembly.A((i + 2)) + 3));
    let D = (|i| Assembly.C((i + 3)));
"#;
        let expected = r#"namespace Assembly(2);
    let A: int -> int = (|i| 0);
    let C: int -> int = (|i| (Assembly.A((i + 2)) + 3));
    let D: int -> int = (|i| Assembly.C((i + 3)));
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn if_expr() {
        let input = r#"namespace Assembly(2);
    col fixed A = [0]*;
    let c = (|i| if (i < 3) { i } else { (i + 9) });
    col fixed D(i) { if (Assembly.c(i) != 0) { 3 } else { 2 } };
"#;
        let expected = r#"namespace Assembly(2);
    col fixed A = [0]*;
    let c: int -> int = (|i| if (i < 3) { i } else { (i + 9) });
    col fixed D(i) { if (Assembly.c(i) != 0) { 3 } else { 2 } };
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn symbolic_functions() {
        let input = r#"namespace N(16);
    let last_row: int = 15;
    let ISLAST: col = |i| match i { last_row => 1, _ => 0 };
    let x;
    let y;
    let constrain_equal_expr = |A, B| A - B;
    let on_regular_row = |cond| (1 - ISLAST) * cond;
    on_regular_row(constrain_equal_expr(x', y)) = 0;
    on_regular_row(constrain_equal_expr(y', x + y)) = 0;
    "#;
        let expected = r#"namespace N(16);
    let last_row: int = 15;
    col fixed ISLAST(i) { match i { N.last_row => 1, _ => 0, } };
    col witness x;
    col witness y;
    let constrain_equal_expr: expr, expr -> expr = (|A, B| (A - B));
    let on_regular_row: expr -> expr = (|cond| ((1 - N.ISLAST) * cond));
    ((1 - N.ISLAST) * (N.x' - N.y)) = 0;
    ((1 - N.ISLAST) * (N.y' - (N.x + N.y))) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn next_op_on_param() {
        let input = r#"namespace N(16);
    let x;
    let y;
    let next_is_seven = |t| t' - 7;
    next_is_seven(y) = 0;
    "#;
        let expected = r#"namespace N(16);
    col witness x;
    col witness y;
    let next_is_seven: expr -> expr = (|t| (t' - 7));
    (N.y' - 7) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn fixed_symbolic() {
        let input = r#"namespace N(16);
    let last_row = 15;
    let islast = |i| match i { N.last_row => 1, _ => 0, };
    let ISLAST: col = |i| islast(i);
    let x;
    let y;
    x - ISLAST = 0;
    "#;
        let expected = r#"namespace N(16);
    let last_row: int = 15;
    let islast: int -> fe = (|i| match i { N.last_row => 1, _ => 0, });
    col fixed ISLAST(i) { N.islast(i) };
    col witness x;
    col witness y;
    (N.x - N.ISLAST) = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn parentheses_lambda() {
        let input = r#"namespace N(16);
    let w = || 2;
    let x: fe = (|i| || w())(w())();
    "#;
        let expected = r#"namespace N(16);
    let w: -> fe = (|| 2);
    constant x = (|i| (|| N.w()))(N.w())();
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn simple_type_resolution() {
        let input = r#"namespace N(16);
    let w: col[3 + 4];
    "#;
        let expected = r#"namespace N(16);
    col witness w[7];
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn complex_type_resolution() {
        let input = r#"namespace N(16);
    let f: int -> int = |i| i + 10;
    let x: (int -> int), int -> int = |k, i| k(2**i);
    let y: col[x(f, 2)];
    let z: (((int -> int), int -> int)[], expr) = ([x, x, x, x, x, x, x, x], y[0]);
    "#;
        let expected = r#"namespace N(16);
    let f: int -> int = (|i| (i + 10));
    let x: (int -> int), int -> int = (|k, i| k((2 ** i)));
    col witness y[14];
    let z: (((int -> int), int -> int)[], expr) = ([N.x, N.x, N.x, N.x, N.x, N.x, N.x, N.x], N.y[0]);
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn function_type_display() {
        let input = r#"namespace N(16);
    let f: (-> int)[] = [(|| 10), (|| 12)];
    let g: (int -> int) -> int = (|f| f(0));
    let h: int -> (int -> int) = (|x| (|i| (x + i)));
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    fn expr_and_identity() {
        let input = r#"namespace N(16);
    let f: expr, expr -> constr[] = |x, y| [x = y];
    let g: expr -> constr[] = |x| [x = 0];
    let x: col;
    let y: col;
    f(x, y);
    g((x));
    "#;
        let expected = r#"namespace N(16);
    let f: expr, expr -> constr[] = (|x, y| [(x = y)]);
    let g: expr -> constr[] = (|x| [(x = 0)]);
    col witness x;
    col witness y;
    N.x = N.y;
    N.x = 0;
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, expected);
    }

    #[test]
    #[should_panic = "Expected type constr but got type expr"]
    fn expression_but_expected_constraint() {
        let input = r#"namespace N(16);
    col witness y;
    (N.y - 2);
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Expected type: expr\\nInferred type: constr\\n"]
    fn constraint_but_expected_expression() {
        let input = r#"namespace N(16);
    col witness y;
    { (N.y - 2) = 0 } in { N.y };
"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Set of declared and used type variables are not the same"]
    fn used_undeclared_type_var() {
        let input = r#"let x: T = 8;"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Set of declared and used type variables are not the same"]
    fn declared_unused_type_var() {
        let input = r#"let<T> x: int = 8;"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }

    #[test]
    #[should_panic = "Excess type variables in declaration: K\nExcess type variables in type: T"]
    fn double_used_undeclared_type_var() {
        let input = r#"let<K> x: T = 8;"#;
        let formatted = analyze_string::<GoldilocksField>(input).to_string();
        assert_eq!(formatted, input);
    }
}
