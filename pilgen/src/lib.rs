//! Compilation from powdr assembly to PIL

mod batcher;
mod pil_converter;

use batcher::ASMBatcher;
use number::FieldElement;

use parser::asm_ast::*;
use parser::ast::*;
use parser_util::ParseError;
use pil_converter::ASMPILConverter;

/// Compiles a stand-alone assembly file to PIL.
pub fn compile<'a, T: FieldElement>(
    file_name: Option<&str>,
    input: &'a str,
) -> Result<PILFile<T>, ParseError<'a>> {
    let statements = parser::parse_asm(file_name, input)
        .map(|ast| ASMBatcher::default().convert(ast))
        .map(|batched_asm| ASMPILConverter::default().convert(batched_asm, ASMKind::StandAlone))?;
    Ok(PILFile(statements))
}

/// Compiles inline assembly to PIL.
pub fn asm_to_pil<T: FieldElement>(
    statements: impl IntoIterator<Item = ASMStatement<T>>,
) -> Vec<Statement<T>> {
    let batched_asm = ASMBatcher::default().convert(ASMFile(statements.into_iter().collect()));
    ASMPILConverter::default().convert(batched_asm, ASMKind::Inline)
}

#[derive(PartialEq)]
pub enum ASMKind {
    Inline,
    StandAlone,
}

pub enum Input {
    Register(String),
    Literal(String, LiteralKind),
}

pub enum LiteralKind {
    Label,
    SignedConstant,
    UnsignedConstant,
}

#[cfg(test)]
mod test {
    use std::fs;

    use number::GoldilocksField;

    use super::compile;

    #[test]
    pub fn compile_simple_sum() {
        let expectation = r#"
namespace Assembly(1024);
pol constant first_step = [1] + [0]*;
pol commit pc;
pol commit X;
(first_step * A) = 0;
pol commit reg_write_X_A;
pol commit A;
(first_step * CNT) = 0;
pol commit reg_write_X_CNT;
pol commit CNT;
pol commit XInv;
pol commit XIsZero;
XIsZero = (1 - (X * XInv));
(XIsZero * X) = 0;
(XIsZero * (1 - XIsZero)) = 0;
pol commit instr_jmpz;
pol commit instr_jmpz_param_l;
pol commit instr_jmp;
pol commit instr_jmp_param_l;
pol commit instr_dec_CNT;
pol commit instr_assert_zero;
(instr_assert_zero * (XIsZero - 1)) = 0;
pol commit X_const;
pol commit X_read_free;
pol commit read_X_A;
pol commit read_X_CNT;
pol commit read_X_pc;
X = (((((read_X_A * A) + (read_X_CNT * CNT)) + (read_X_pc * pc)) + X_const) + (X_read_free * X_free_value));
A' = (((first_step' * 0) + (reg_write_X_A * X)) + ((1 - (first_step' + reg_write_X_A)) * A));
CNT' = ((((first_step' * 0) + (reg_write_X_CNT * X)) + (instr_dec_CNT * (CNT - 1))) + ((1 - ((first_step' + reg_write_X_CNT) + instr_dec_CNT)) * CNT));
pc' = ((1 - first_step') * (((instr_jmpz * ((XIsZero * instr_jmpz_param_l) + ((1 - XIsZero) * (pc + 1)))) + (instr_jmp * instr_jmp_param_l)) + ((1 - (instr_jmpz + instr_jmp)) * (pc + 1))));
pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] + [10]*;
pol commit X_free_value(i) query match pc { 0 => ("input", 1), 3 => ("input", (CNT + 1)), 7 => ("input", 0), };
pol constant p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [1, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0] + [0]*;
pol constant p_instr_assert_zero = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0] + [0]*;
pol constant p_instr_dec_CNT = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] + [1]*;
pol constant p_instr_jmp_param_l = [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 9] + [9]*;
pol constant p_instr_jmpz = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz_param_l = [0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0] + [0]*;
pol constant p_read_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };

"#;
        let file_name = "../test_data/asm/simple_sum.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let pil = compile::<GoldilocksField>(Some(file_name), &contents).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    pub fn compile_literal_number_args() {
        let source = r#"
reg pc[@pc];
reg fp;

instr inc_fp amount: unsigned { fp' = fp + amount }
instr adjust_fp amount: signed, t: label { fp' = fp + amount, pc' = label }

inc_fp 7;
loop::
adjust_fp -2, loop;
"#;
        let expectation = r#"
namespace Assembly(1024);
pol constant first_step = [1] + [0]*;
pol commit pc;
(first_step * fp) = 0;
pol commit fp;
pol commit instr_inc_fp;
pol commit instr_inc_fp_param_amount;
pol commit instr_adjust_fp;
pol commit instr_adjust_fp_param_amount;
pol commit instr_adjust_fp_param_t;
fp' = ((((first_step' * 0) + (instr_inc_fp * (fp + instr_inc_fp_param_amount))) + (instr_adjust_fp * (fp + instr_adjust_fp_param_amount))) + ((1 - ((first_step' + instr_inc_fp) + instr_adjust_fp)) * fp));
pc' = ((1 - first_step') * ((instr_adjust_fp * label) + ((1 - instr_adjust_fp) * (pc + 1))));
pol constant p_line = [0, 1, 2] + [2]*;
pol constant p_instr_adjust_fp = [0, 0, 1] + [1]*;
pol constant p_instr_adjust_fp_param_amount = [0, 0, -2] + [-2]*;
pol constant p_instr_adjust_fp_param_t = [0, 0, 1] + [1]*;
pol constant p_instr_inc_fp = [1, 0, 0] + [0]*;
pol constant p_instr_inc_fp_param_amount = [7, 0, 0] + [0]*;
{ pc, instr_inc_fp, instr_inc_fp_param_amount, instr_adjust_fp, instr_adjust_fp_param_amount, instr_adjust_fp_param_t } in { p_line, p_instr_inc_fp, p_instr_inc_fp_param_amount, p_instr_adjust_fp, p_instr_adjust_fp_param_amount, p_instr_adjust_fp_param_t };
"#;
        let pil = compile::<GoldilocksField>(None, source).unwrap();
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    #[should_panic]
    pub fn negative_for_unsigned() {
        let source = r#"
reg pc[@pc];
reg fp;

instr instro x: unsigned { pc' = pc + x }

instro 9223372034707292161;
"#;
        compile::<GoldilocksField>(None, source).unwrap();
    }
}
