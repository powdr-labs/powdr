//! Parser for powdr assembly and PIL

#![deny(clippy::print_stdout)]

use lalrpop_util::*;
use powdr_ast::parsed::{
    asm::ASMProgram,
    types::{Type, TypeBounds, TypeScheme},
};
use powdr_ast::SourceRef;

use powdr_parser_util::{handle_parse_error, ParseError};

use std::sync::Arc;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub powdr,
    "/powdr.rs"
);

pub struct ParserContext {
    file_name: Option<Arc<str>>,
    line_starts: Vec<usize>,
}

impl ParserContext {
    pub fn new(file_name: Option<&str>, input: &str) -> Self {
        Self {
            file_name: file_name.map(|s| s.into()),
            line_starts: powdr_parser_util::lines::compute_line_starts(input),
        }
    }

    pub fn source_ref(&self, offset: usize) -> SourceRef {
        let (line, col) = powdr_parser_util::lines::offset_to_line_col(offset, &self.line_starts);
        SourceRef {
            file: self.file_name.clone(),
            line,
            col,
        }
    }
}

lazy_static::lazy_static! {
    static ref PIL_FILE_PARSER: powdr::PILFileParser = powdr::PILFileParser::new();
    static ref ASM_MODULE_PARSER: powdr::ASMModuleParser = powdr::ASMModuleParser::new();
    static ref TYPE_PARSER: powdr::TypeParser = powdr::TypeParser::new();
    static ref TYPE_VAR_BOUNDS_PARSER: powdr::TypeVarBoundsParser = powdr::TypeVarBoundsParser::new();
}

pub fn parse<'a>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::PILFile, ParseError<'a>> {
    let ctx = ParserContext::new(file_name, input);
    PIL_FILE_PARSER
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

pub fn parse_asm<'a>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::asm::ASMProgram, ParseError<'a>> {
    parse_module(file_name, input).map(|main| ASMProgram { main })
}

pub fn parse_module<'a>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<powdr_ast::parsed::asm::ASMModule, ParseError<'a>> {
    let ctx = ParserContext::new(file_name, input);
    ASM_MODULE_PARSER
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, file_name, input))
}

pub fn parse_type(input: &str) -> Result<Type<powdr_ast::parsed::Expression>, ParseError<'_>> {
    let ctx = ParserContext::new(None, input);
    TYPE_PARSER
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, None, input))
}

pub fn parse_type_var_bounds(input: &str) -> Result<TypeBounds, ParseError<'_>> {
    let ctx = ParserContext::new(None, input);
    // We use GoldilocksField here, because we need to specify a concrete type,
    // even though the grammar for TypeBounds does not depend on the field.
    TYPE_VAR_BOUNDS_PARSER
        .parse(&ctx, input)
        .map_err(|err| handle_parse_error(err, None, input))
}

pub fn parse_type_scheme(vars: &str, ty: &str) -> TypeScheme {
    let vars = parse_type_var_bounds(vars).unwrap();
    let mut ty = parse_type(ty).unwrap();
    ty.map_to_type_vars(&vars.vars().collect());
    TypeScheme {
        vars,
        ty: ty.into(),
    }
}

/// Parse an escaped string - used in the grammar.
pub fn unescape_string(s: &str) -> String {
    assert!(s.len() >= 2);
    assert!(s.starts_with('"') && s.ends_with('"'));
    let mut chars = s[1..s.len() - 1].chars();
    let mut result: String = Default::default();
    while let Some(c) = chars.next() {
        result.push(if c == '\\' {
            match chars.next().unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'b' => 8 as char,
                'f' => 12 as char,
                other => other,
            }
        } else {
            c
        })
    }
    result
}

#[cfg(test)]
mod test {
    use super::*;
    use powdr_ast::parsed::{
        self,
        asm::{ASMProgram, MachineProperties},
        build::direct_reference,
        visitor::Children,
        FunctionDefinition, LambdaExpression, PILFile, PilStatement, PolynomialName,
        SelectedExpressions, StatementInsideBlock,
    };
    use powdr_parser_util::UnwrapErrToStderr;
    use pretty_assertions::assert_eq;
    use similar::TextDiff;
    use test_log::test;
    use walkdir::WalkDir;

    #[test]
    fn empty() {
        let input = "";
        let ctx = ParserContext::new(None, input);
        assert!(powdr::PILFileParser::new().parse(&ctx, input).is_ok());
    }

    #[test]
    fn simple_include() {
        let input = "include \"x\";";
        let ctx = ParserContext::new(None, input);
        let parsed = powdr::PILFileParser::new().parse(&ctx, input).unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::Include(
                SourceRef {
                    file: None,
                    line: 1,
                    col: 0,
                },
                "x".to_string()
            )])
        );
    }

    #[test]
    fn start_offsets() {
        let input = "include \"x\"; pol commit t;";
        let ctx = ParserContext::new(None, input);
        let parsed = powdr::PILFileParser::new().parse(&ctx, input).unwrap();
        assert_eq!(
            parsed,
            PILFile(vec![
                PilStatement::Include(
                    SourceRef {
                        file: None,
                        line: 1,
                        col: 0,
                    },
                    "x".to_string()
                ),
                PilStatement::PolynomialCommitDeclaration(
                    SourceRef {
                        file: None,
                        line: 1,
                        col: 13,
                    },
                    None,
                    vec![PolynomialName {
                        name: "t".to_string(),
                        array_size: None
                    }],
                    None
                )
            ])
        );
    }

    #[test]
    fn simple_plookup() {
        let input = "f in g;";
        let ctx = ParserContext::new(None, input);
        let mut parsed = powdr::PILFileParser::new().parse(&ctx, "f in g;").unwrap();
        let mut drf = direct_reference("f");
        let mut drg = direct_reference("g");
        pil_expression_clear_source_ref(&mut drf);
        pil_expression_clear_source_ref(&mut drg);
        pil_clear_source_refs(&mut parsed);
        assert_eq!(
            parsed,
            PILFile(vec![PilStatement::PlookupIdentity(
                SourceRef {
                    file: None,
                    line: 0,
                    col: 0,
                },
                SelectedExpressions {
                    selector: None,
                    expressions: vec![drf]
                },
                SelectedExpressions {
                    selector: None,
                    expressions: vec![drg]
                }
            )])
        );
    }

    fn find_files_with_ext(
        dir: std::path::PathBuf,
        ext: String,
    ) -> impl Iterator<Item = (String, String)> {
        WalkDir::new(dir).into_iter().filter_map(move |e| {
            let entry = e.unwrap();
            let path = entry.path();
            match path.extension() {
                Some(path_ext) if path_ext.to_str() == Some(&ext) => Some((
                    std::fs::canonicalize(path)
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .into(),
                    std::fs::read_to_string(path).unwrap(),
                )),
                _ => None,
            }
        })
    }

    // helper function to clear SourceRef's inside the AST so we can compare for equality
    fn pil_clear_source_refs(ast: &mut PILFile) {
        ast.0.iter_mut().for_each(pil_statement_clear_source_ref);
    }

    fn pil_statement_clear_source_ref(stmt: &mut PilStatement) {
        match stmt {
            PilStatement::Include(s, _) => *s = SourceRef::unknown(),
            PilStatement::PolynomialConstantDefinition(s, _, v) => {
                *s = SourceRef::unknown();
                v.children_mut().for_each(pil_expression_clear_source_ref);
            }
            PilStatement::PolynomialConstantDeclaration(s, v) => {
                *s = SourceRef::unknown();
                v.iter_mut().for_each(|p| {
                    if let Some(ref mut array_size) = p.array_size {
                        pil_expression_clear_source_ref(array_size);
                    }
                });
            }
            PilStatement::PolynomialCommitDeclaration(s, _, v, ofc) => {
                *s = SourceRef::unknown();
                v.iter_mut().for_each(|p| {
                    if let Some(ref mut array_size) = p.array_size {
                        pil_expression_clear_source_ref(array_size);
                    }
                });
                if let Some(FunctionDefinition::Expression(e)) = ofc {
                    pil_expression_clear_source_ref(e)
                }
            }

            PilStatement::Namespace(s, _, e) => {
                if let Some(e) = e {
                    pil_expression_clear_source_ref(e);
                }
                *s = SourceRef::unknown();
            }
            PilStatement::LetStatement(s, _, te, e) => {
                if let Some(te) = te {
                    te.ty
                        .children_mut()
                        .for_each(pil_expression_clear_source_ref);
                }
                if let Some(e) = e {
                    pil_expression_clear_source_ref(e);
                }
                *s = SourceRef::unknown();
            }
            PilStatement::PolynomialDefinition(s, _, e)
            | PilStatement::ConstantDefinition(s, _, e) => {
                pil_expression_clear_source_ref(e);
                *s = SourceRef::unknown();
            }
            PilStatement::PublicDeclaration(s, _, _, oe, e) => {
                pil_expression_clear_source_ref(e);
                if let Some(oe) = oe {
                    pil_expression_clear_source_ref(oe);
                }
                *s = SourceRef::unknown();
            }
            PilStatement::PlookupIdentity(s, se1, se2)
            | PilStatement::PermutationIdentity(s, se1, se2) => {
                if let Some(exp) = se1.selector.as_mut() {
                    pil_expression_clear_source_ref(exp);
                }
                if let Some(exp) = se2.selector.as_mut() {
                    pil_expression_clear_source_ref(exp);
                }
                se1.expressions
                    .iter_mut()
                    .chain(se2.expressions.iter_mut())
                    .for_each(pil_expression_clear_source_ref);
                *s = SourceRef::unknown();
            }
            PilStatement::ConnectIdentity(s, v1, v2) => {
                v1.iter_mut()
                    .chain(v2.iter_mut())
                    .for_each(pil_expression_clear_source_ref);
                *s = SourceRef::unknown();
            }
            PilStatement::EnumDeclaration(s, _en) => {
                *s = SourceRef::unknown();
            }
            PilStatement::Expression(s, e) => {
                pil_expression_clear_source_ref(e);
                for co in e.children_mut() {
                    pil_expression_clear_source_ref(co);
                }
                *s = SourceRef::unknown();
            }
        }
    }

    fn pil_expression_clear_source_ref(expr: &mut parsed::Expression) {
        match expr {
            parsed::Expression::Reference(s, _)
            | parsed::Expression::PublicReference(s, _)
            | parsed::Expression::Number(s, _)
            | parsed::Expression::String(s, _) => *s = SourceRef::unknown(),
            parsed::Expression::FreeInput(s, e) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(e);
            }
            parsed::Expression::LambdaExpression(s, le) => {
                *s = SourceRef::unknown();
                let LambdaExpression { body, .. } = le;

                pil_expression_clear_source_ref(body);
            }
            parsed::Expression::BinaryOperation(s, bo) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(&mut bo.left);
                pil_expression_clear_source_ref(&mut bo.right);
            }
            parsed::Expression::UnaryOperation(s, uo) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(&mut uo.e);
            }
            parsed::Expression::IndexAccess(s, ia) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(&mut ia.array);
                pil_expression_clear_source_ref(&mut ia.index);
            }
            parsed::Expression::ArrayLiteral(s, a) => {
                *s = SourceRef::unknown();
                a.items.iter_mut().for_each(pil_expression_clear_source_ref);
            }
            parsed::Expression::Tuple(s, t) => {
                *s = SourceRef::unknown();
                t.iter_mut().for_each(pil_expression_clear_source_ref);
            }
            parsed::Expression::MatchExpression(s, me) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(&mut me.e);
                me.arms.iter_mut().for_each(|arm| {
                    pil_expression_clear_source_ref(&mut arm.value);
                });
            }
            parsed::Expression::FunctionCall(s, fc) => {
                *s = SourceRef::unknown();
                fc.arguments
                    .iter_mut()
                    .for_each(pil_expression_clear_source_ref);
                pil_expression_clear_source_ref(&mut fc.function);
            }
            parsed::Expression::IfExpression(s, ie) => {
                *s = SourceRef::unknown();
                pil_expression_clear_source_ref(&mut ie.condition);
                pil_expression_clear_source_ref(&mut ie.body);
                pil_expression_clear_source_ref(&mut ie.else_body);
            }
            parsed::Expression::BlockExpression(s, be) => {
                *s = SourceRef::unknown();
                be.statements.iter_mut().for_each(|s| match s {
                    StatementInsideBlock::Expression(e) => pil_expression_clear_source_ref(e),
                    StatementInsideBlock::LetStatement(lt) => {
                        if let Some(expr) = lt.value.as_mut() {
                            pil_expression_clear_source_ref(expr)
                        }
                    }
                });

                pil_expression_clear_source_ref(&mut be.expr);
            }
        }
    }

    // helper function to clear SourceRef's inside the AST so we can compare for equality
    fn asm_clear_source_refs(ast: &mut ASMProgram) {
        use powdr_ast::parsed::asm::{
            ASMModule, FunctionStatement, Instruction, InstructionBody, Machine, MachineStatement,
            Module, ModuleStatement, SymbolDefinition, SymbolValue,
        };

        fn clear_machine_stmt(stmt: &mut MachineStatement) {
            match stmt {
                MachineStatement::Submachine(s, _, _)
                | MachineStatement::RegisterDeclaration(s, _, _)
                | MachineStatement::OperationDeclaration(s, _, _, _) => {
                    *s = SourceRef::unknown();
                }
                MachineStatement::LinkDeclaration(s, lk) => {
                    pil_expression_clear_source_ref(&mut lk.flag);
                    lk.to
                        .params
                        .inputs
                        .iter_mut()
                        .chain(lk.to.params.outputs.iter_mut())
                        .for_each(pil_expression_clear_source_ref);
                    *s = SourceRef::unknown();
                }
                MachineStatement::Pil(s, stmt) => {
                    *s = SourceRef::unknown();
                    pil_statement_clear_source_ref(stmt)
                }
                MachineStatement::InstructionDeclaration(s, _, Instruction { body, .. }) => {
                    *s = SourceRef::unknown();
                    match body {
                        InstructionBody::Local(statements) => statements
                            .iter_mut()
                            .for_each(pil_statement_clear_source_ref),
                        InstructionBody::CallablePlookup(cf)
                        | InstructionBody::CallablePermutation(cf) => {
                            cf.params
                                .inputs
                                .iter_mut()
                                .chain(cf.params.outputs.iter_mut())
                                .for_each(pil_expression_clear_source_ref);
                        }
                    }
                }
                MachineStatement::FunctionDeclaration(s, _, _, statements) => {
                    *s = SourceRef::unknown();
                    for statement in statements {
                        match statement {
                            FunctionStatement::Label(s, _)
                            | FunctionStatement::DebugDirective(s, _) => *s = SourceRef::unknown(),
                            FunctionStatement::Return(s, v) => {
                                *s = SourceRef::unknown();
                                v.iter_mut().for_each(pil_expression_clear_source_ref);
                            }
                            FunctionStatement::Instruction(s, _, v) => {
                                *s = SourceRef::unknown();
                                v.iter_mut().for_each(pil_expression_clear_source_ref);
                            }
                            FunctionStatement::Assignment(s, _, _, e) => {
                                *s = SourceRef::unknown();
                                pil_expression_clear_source_ref(e)
                            }
                        }
                    }
                }
            }
        }

        fn clear_module_stmt(stmt: &mut ModuleStatement) {
            let ModuleStatement::SymbolDefinition(SymbolDefinition { value, .. }) = stmt;
            match value {
                SymbolValue::Machine(Machine {
                    properties,
                    statements,
                    ..
                }) => {
                    statements.iter_mut().for_each(clear_machine_stmt);
                    let MachineProperties { degree, .. } = properties;

                    if let Some(degree) = degree {
                        pil_expression_clear_source_ref(degree);
                    }
                }
                SymbolValue::Module(Module::Local(ASMModule { statements })) => {
                    statements.iter_mut().for_each(clear_module_stmt);
                }
                SymbolValue::Expression(e) => {
                    pil_expression_clear_source_ref(&mut e.e);
                }
                SymbolValue::Module(Module::External(_))
                | SymbolValue::Import(_)
                | SymbolValue::TypeDeclaration(_) => (),
            }
        }

        ast.main.statements.iter_mut().for_each(clear_module_stmt);
    }

    #[test]
    /// Test that (source -> AST -> source -> AST) works properly for asm files
    fn parse_write_reparse_asm() {
        let crate_dir = env!("CARGO_MANIFEST_DIR");
        let basedir = std::path::PathBuf::from(format!("{crate_dir}/../test_data/"));
        let asm_files = find_files_with_ext(basedir, "asm".into());
        for (file, orig_string) in asm_files {
            let mut orig_asm = parse_asm(Some(&file), &orig_string).unwrap_err_to_stderr();
            let orig_asm_to_string = format!("{}", orig_asm);
            let mut reparsed_asm = parse_asm(
                Some((file.clone() + " reparsed").as_ref()),
                &orig_asm_to_string,
            )
            .unwrap_err_to_stderr();
            asm_clear_source_refs(&mut orig_asm);
            asm_clear_source_refs(&mut reparsed_asm);
            if orig_asm != reparsed_asm {
                let orig_ast = format!("{orig_asm:#?}");
                let reparsed_ast = format!("{reparsed_asm:#?}");
                let diff = TextDiff::from_lines(&orig_ast, &reparsed_ast);
                eprintln!("parsed and re-parsed ASTs differ:");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Delete => "-",
                        similar::ChangeTag::Insert => "+",
                        similar::ChangeTag::Equal => " ",
                    };
                    eprint!("\t{}{}", sign, change);
                }
                panic!("parsed and re-parsed ASTs differ for file: {file}");
            }
        }
    }

    #[test]
    /// Test that (source -> AST -> source -> AST) works properly for pil files
    fn parse_write_reparse_pil() {
        let crate_dir = env!("CARGO_MANIFEST_DIR");
        let basedir = std::path::PathBuf::from(format!("{crate_dir}/../test_data/"));
        let pil_files = find_files_with_ext(basedir, "pil".into());
        for (file, orig_string) in pil_files {
            let mut orig_pil = parse(Some(&file), &orig_string).unwrap_err_to_stderr();
            let orig_pil_to_string = format!("{}", orig_pil);
            let mut reparsed_pil = parse(
                Some((file.clone() + " reparsed").as_ref()),
                &orig_pil_to_string,
            )
            .unwrap_err_to_stderr();
            pil_clear_source_refs(&mut orig_pil);
            pil_clear_source_refs(&mut reparsed_pil);
            assert_eq!(orig_pil, reparsed_pil);
            if orig_pil != reparsed_pil {
                let orig_ast = format!("{orig_pil:#?}");
                let reparsed_ast = format!("{reparsed_pil:#?}");
                let diff = TextDiff::from_lines(&orig_ast, &reparsed_ast);
                eprintln!("parsed and re-parsed ASTs differ:");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Delete => "-",
                        similar::ChangeTag::Insert => "+",
                        similar::ChangeTag::Equal => " ",
                    };
                    eprint!("\t{}{}", sign, change);
                }
                panic!("parsed and re-parsed ASTs differ for file: {file}");
            }
        }
    }

    use crate::parse;

    #[test]
    fn reparse() {
        let input = r#"
    constant %N = 16;
namespace Fibonacci(%N);
    constant %last_row = (%N - 1);
    let bool: expr -> expr = (|X| (X * (1 - X)));
    let one_hot = (|i, which| match i {
        which => 1,
        _ => 0,
    });
    pol constant ISLAST(i) { one_hot(i, %last_row) };
    pol commit arr[8];
    pol commit x, y;
    { (x + 2), y' } in { ISLAST, 7 };
    y { (x + 2), y' } is ISLAST { ISLAST, 7 };
    (((x - 2) * y) = 8);
    public out = y(%last_row);"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn reparse_witness_query() {
        let input = r#"pol commit wit(i) query (x(i), y(i));"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn reparse_arrays() {
        let input = "    pol commit y[3];\n    ((y - 2) = 0);\n    ((y[2] - 2) = 0);\n    public out = y[1](2);";
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn reparse_strings_and_tuples() {
        let input = r#"constant %N = ("abc", 3);"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn array_literals() {
        let input = r#"let x = [[1], [2], [(3 + 7)]];"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap_err_to_stderr());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn type_names_simple() {
        let input = r#"
    let a: col;
    let b: int;
    let c: fe;
    let d: int[];
    let e: int[7];
    let f: (int, fe, fe[3])[2];"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn type_names_complex() {
        let input = r#"
    let a: int -> fe;
    let b: int -> ();
    let c: -> ();
    let d: int, int -> fe;
    let e: int, int -> (fe, int[2]);
    let f: ((int, fe), fe[2] -> (fe -> int))[];
    let g: (int -> fe) -> int;
    let h: int -> (fe -> int);"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn enum_decls() {
        let input = r#"
namespace N(2);
    enum X {
    }
    enum Y {
        A,
        B(),
        C(int),
        D(int, (int -> fe)),
    }
"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn patterns() {
        let input = r#"
namespace N(2);
    let x = (|(x, y), [t, r, ..]| (x, y, t, r));
    {
        let (a, b, _, d) = x((1, 2), [3, 4]);
        b
    };
"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap_err_to_stderr());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn type_args() {
        let input = r#"
namespace N(2);
    let<T: Ord> max: T, T -> T = (|a, b| if (a < b) { b } else { a });
    let<T1, T2> left: T1, T2 -> T1 = (|a, b| a);
    let seven = max::<int>(3, 7);
    let five = left::<int, fe[]>(5, [7]);
    let also_five = five::<>;
"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap_err_to_stderr());
        assert_eq!(input.trim(), printed.trim());
    }

    #[test]
    fn type_args_with_space() {
        let input = r#"
namespace N(2);
    let<T: Ord> max: T, T -> T = (|a, b| if (a < b) { b } else { a });
    let seven = max :: <int>(3, 7);
"#;
        let expected = r#"
namespace N(2);
    let<T: Ord> max: T, T -> T = (|a, b| if (a < b) { b } else { a });
    let seven = max::<int>(3, 7);
"#;
        let printed = format!("{}", parse(Some("input"), input).unwrap_err_to_stderr());
        assert_eq!(expected.trim(), printed.trim());
    }
}
