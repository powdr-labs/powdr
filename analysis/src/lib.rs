pub mod machine_check;
mod vm;

use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use powdr_ast::asm_analysis::InstructionDefinitionStatement;
use powdr_ast::parsed::asm::{
    parse_absolute_path, AbsoluteSymbolPath, CallableRef, Instruction, InstructionBody,
    LinkDeclaration, MachineParams, OperationId, Param, Params, Part, SymbolPath,
};
use powdr_ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMProgram};
use powdr_ast::{
    asm_analysis::{
        CallableSymbol, CallableSymbolDefinitions, FunctionStatement, FunctionStatements,
        InstructionStatement, LabelStatement, LinkDefinition, Machine, MachineDegree, Module,
        OperationSymbol, RegisterTy, SubmachineDeclaration,
    },
    parsed::{
        visitor::{ExpressionVisitable, VisitOrder},
        BinaryOperator, FunctionCall, NamespacedPolynomialReference, Number, PilStatement,
        UnaryOperation, UnaryOperator,
    },
};
use powdr_number::BigUint;
use powdr_number::FieldElement;
use powdr_parser_util::SourceRef;

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

const MAIN_MACHINE_STR: &str = "::Main";

pub fn convert_asm_to_pil<T: FieldElement>(
    file: ASMProgram,
) -> Result<AnalysisASMFile, Vec<String>> {
    let file = analyze(file)?;
    Ok(powdr_asm_to_pil::compile::<T>(file))
}

pub fn check(file: ASMProgram) -> Result<AnalysisASMFile, Vec<String>> {
    log::debug!("Run machine check analysis step");
    let mut file = machine_check::check(file)?;
    annotate_basic_blocks(&mut file);

    Ok(file)
}

pub fn analyze_precompiles(
    analyzed_asm: AnalysisASMFile,
    selected: &BTreeSet<String>,
) -> AnalysisASMFile {
    let main_machine_path = parse_absolute_path(MAIN_MACHINE_STR);
    if analyzed_asm
        .machines()
        .all(|(path, _)| path != main_machine_path)
    {
        return analyzed_asm;
    }

    powdr_autoprecompiles::powdr::create_precompiles(analyzed_asm, selected)
}

pub fn analyze_only(file: AnalysisASMFile) -> Result<AnalysisASMFile, Vec<String>> {
    // run analysis on virtual machines, batching instructions
    log::debug!("Start asm analysis");
    let file = vm::analyze(file)?;
    log::debug!("End asm analysis");

    Ok(file)
}

pub fn analyze(file: ASMProgram) -> Result<AnalysisASMFile, Vec<String>> {
    log::debug!("Run machine check analysis step");
    let file = machine_check::check(file)?;

    // run analysis on virtual machines, batching instructions
    log::debug!("Start asm analysis");
    let file = vm::analyze(file)?;
    log::debug!("End asm analysis");

    Ok(file)
}

pub mod utils {
    use powdr_ast::parsed::PilStatement;
    use powdr_parser::powdr;

    lazy_static::lazy_static! {
        static ref PIL_STATEMENT_PARSER: powdr::PilStatementParser = powdr::PilStatementParser::new();
    }

    pub fn parse_pil_statement(input: &str) -> PilStatement {
        let ctx = powdr_parser::ParserContext::new(None, input);
        PIL_STATEMENT_PARSER.parse(&ctx, input).unwrap()
    }
}

pub fn annotate_basic_blocks(analyzed_asm: &mut AnalysisASMFile) {
    let machine = analyzed_asm
        .get_machine_mut(&parse_absolute_path("::Main"))
        .unwrap();
    let CallableSymbol::Function(ref mut main_function) =
        &mut machine.callable.0.get_mut("main").unwrap()
    else {
        panic!("main function missing")
    };

    let program = &mut main_function.body.statements.inner;

    let mut ghost_labels = 0;

    let mut i = 0;
    while i < program.len() {
        match &program[i] {
            FunctionStatement::Instruction(InstructionStatement {
                source,
                instruction,
                inputs: _,
            }) if instruction.starts_with("branch")
                || instruction.starts_with("jump")
                || instruction.starts_with("skip") =>
            {
                let curr_label = format!("ghost_label_{ghost_labels}");
                let new_label = FunctionStatement::Label(LabelStatement {
                    source: source.clone(),
                    name: curr_label.clone(),
                });
                program.insert(i + 1, new_label);
                ghost_labels += 1;
                i += 1;
            }
            FunctionStatement::Return(_) | FunctionStatement::Assignment(_) => {
                let curr_label = format!("ghost_label_{ghost_labels}");
                let new_label = FunctionStatement::Label(LabelStatement {
                    source: Default::default(),
                    name: curr_label.clone(),
                });
                program.insert(i + 1, new_label);
                ghost_labels += 1;
                i += 1;
            }
            _ => {}
        }
        i += 1;
    }
}

pub fn collect_basic_blocks(
    analyzed_asm: &AnalysisASMFile,
) -> Vec<(String, Vec<FunctionStatement>)> {
    let machine = analyzed_asm
        .get_machine(&parse_absolute_path("::Main"))
        .unwrap();
    let CallableSymbol::Function(ref main_function) = &mut machine.callable.0.get("main").unwrap()
    else {
        panic!("main function missing")
    };

    let program = &main_function.body.statements.inner;

    let mut blocks = Vec::new();
    //let ghost_labels = 0;
    //let mut curr_label = format!("ghost_label_{ghost_labels}");
    //let mut curr_label = "block_init".to_string();
    let mut curr_label: Option<String> = None;
    let mut block_statements = Vec::new();

    for op in program {
        match &op {
            FunctionStatement::Label(LabelStatement { source: _, name }) => {
                if let Some(label) = curr_label {
                    assert!(!blocks.iter().any(|(l, _)| l == &label));
                    blocks.push((label.clone(), block_statements.clone()));
                }
                block_statements.clear();
                curr_label = Some(name.clone());
            }
            FunctionStatement::Instruction(InstructionStatement {
                source: _,
                instruction,
                inputs: _,
            }) if instruction.starts_with("branch") || instruction.starts_with("jump") => {
                block_statements.push(op.clone());
                if let Some(label) = curr_label {
                    assert!(!blocks.iter().any(|(l, _)| l == &label));
                    blocks.push((label.clone(), block_statements.clone()));
                }
                block_statements.clear();
                curr_label = None;
                //assert!(!blocks.iter().any(|(label, _)| label == &curr_label));
                //blocks.push((curr_label.clone(), block_statements.clone()));
                //block_statements.clear();
                //ghost_labels += 1;
                //curr_label = format!("ghost_label_{ghost_labels}");
            }
            FunctionStatement::Return(_) | FunctionStatement::Assignment(_) => {
                if let Some(label) = curr_label {
                    assert!(!blocks.iter().any(|(l, _)| l == &label));
                    blocks.push((label.clone(), block_statements.clone()));
                }
                block_statements.clear();
                curr_label = None;
                //blocks.push((curr_label.clone(), block_statements.clone()));
                //block_statements.clear();
                //ghost_labels += 1;
                //curr_label = format!("ghost_label_{ghost_labels}");
            }
            FunctionStatement::Instruction(InstructionStatement {
                source: _,
                instruction,
                inputs: _,
            }) if instruction.starts_with("skip") => {
                if let Some(label) = curr_label {
                    assert!(!blocks.iter().any(|(l, _)| l == &label));
                    blocks.push((label.clone(), block_statements.clone()));
                }
                block_statements.clear();
                curr_label = None;
            }
            FunctionStatement::Instruction(InstructionStatement {
                source: _,
                instruction: _,
                inputs: _,
            }) => {
                block_statements.push(op.clone());
            }
            _ => {}
        }
    }

    blocks
}
