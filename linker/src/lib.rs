use ast::{
    object::{Location, PILGraph},
    parsed::{Expression, PILFile, PilStatement},
};
use number::FieldElement;

/// a monolithic linker which outputs a single AIR
pub fn link<T: FieldElement>(mut graph: PILGraph<T>) -> PILFile<T> {
    assert_eq!(graph.objects.len(), 1, "only one machine is supported");
    let object = graph
        .objects
        .remove(&Location::default().join("main"))
        .unwrap();
    let location = Location::default().join("main");

    let mut pil = vec![];

    pil.push(PilStatement::Namespace(
        0,
        location.to_string(),
        Expression::Number(T::from(object.degree)),
    ));

    pil.extend(object.pil);

    PILFile(pil)
}

#[cfg(test)]
mod test {
    use std::fs;

    use ast::object::PILGraph;
    use number::{FieldElement, GoldilocksField};

    use analysis::analyze;
    use parser::parse_asm;

    use pretty_assertions::assert_eq;

    use crate::link;

    fn parse_analyse_and_compile<T: FieldElement>(input: &str) -> PILGraph<T> {
        asm_to_pil::compile(analyze(parse_asm(None, input).unwrap()).unwrap())
    }

    #[test]
    pub fn compile_simple_sum() {
        let expectation = r#"
namespace main(1024);
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
pol constant p_line = [0, 1, 2, 3, 4, 5, 6, 7] + [7]*;
pol commit X_free_value(i) query match pc { 0 => ("input", 1), 2 => ("input", (CNT + 1)), 5 => ("input", 0), };
pol constant p_X_const = [0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_X_read_free = [1, 0, 1, 0, 0, -1, 0, 0] + [0]*;
pol constant p_instr_assert_zero = [0, 0, 0, 0, 0, 0, 1, 0] + [0]*;
pol constant p_instr_dec_CNT = [0, 0, 0, 1, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmp = [0, 0, 0, 0, 1, 0, 0, 1] + [1]*;
pol constant p_instr_jmp_param_l = [0, 0, 0, 0, 1, 0, 0, 7] + [7]*;
pol constant p_instr_jmpz = [0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_instr_jmpz_param_l = [0, 5, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_A = [0, 0, 1, 0, 0, 1, 1, 0] + [0]*;
pol constant p_read_X_CNT = [0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
pol constant p_reg_write_X_A = [0, 0, 1, 0, 0, 1, 0, 0] + [0]*;
pol constant p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0] + [0]*;
{ pc, reg_write_X_A, reg_write_X_CNT, instr_jmpz, instr_jmpz_param_l, instr_jmp, instr_jmp_param_l, instr_dec_CNT, instr_assert_zero, X_const, X_read_free, read_X_A, read_X_CNT, read_X_pc } in { p_line, p_reg_write_X_A, p_reg_write_X_CNT, p_instr_jmpz, p_instr_jmpz_param_l, p_instr_jmp, p_instr_jmp_param_l, p_instr_dec_CNT, p_instr_assert_zero, p_X_const, p_X_read_free, p_read_X_A, p_read_X_CNT, p_read_X_pc };

"#;
        let file_name = "../test_data/asm/simple_sum.asm";
        let contents = fs::read_to_string(file_name).unwrap();
        let graph = parse_analyse_and_compile::<GoldilocksField>(&contents);
        let pil = link(graph);
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    pub fn compile_literal_number_args() {
        let source = r#"
machine Machine {
    reg pc[@pc];
    reg fp;

    instr inc_fp amount: unsigned { fp' = fp + amount }
    instr adjust_fp amount: signed, t: label { fp' = fp + amount, pc' = label }

    function main {
        inc_fp 7;
        loop::
        adjust_fp -2, loop;
    }
}
"#;
        let expectation = r#"
namespace main(1024);
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
pol constant p_line = [0, 1] + [1]*;
pol constant p_instr_adjust_fp = [0, 1] + [1]*;
pol constant p_instr_adjust_fp_param_amount = [0, -2] + [-2]*;
pol constant p_instr_adjust_fp_param_t = [0, 1] + [1]*;
pol constant p_instr_inc_fp = [1, 0] + [0]*;
pol constant p_instr_inc_fp_param_amount = [7, 0] + [0]*;
{ pc, instr_inc_fp, instr_inc_fp_param_amount, instr_adjust_fp, instr_adjust_fp_param_amount, instr_adjust_fp_param_t } in { p_line, p_instr_inc_fp, p_instr_inc_fp_param_amount, p_instr_adjust_fp, p_instr_adjust_fp_param_amount, p_instr_adjust_fp_param_t };
"#;
        let graph = parse_analyse_and_compile::<GoldilocksField>(source);
        let pil = link(graph);
        assert_eq!(format!("{pil}").trim(), expectation.trim());
    }

    #[test]
    #[should_panic(expected = "Number passed to unsigned parameter is negative or too large")]
    pub fn negative_for_unsigned() {
        let source = r#"
machine NegativeForUnsigned {
    reg pc[@pc];
    reg fp;
    
    instr instro x: unsigned { pc' = pc + x }
    
    function main {
        instro 9223372034707292161;
    }
}
"#;
        let graph = parse_analyse_and_compile::<GoldilocksField>(source);
        let _ = link(graph);
    }
}
