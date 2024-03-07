use powdr_number::FieldElement;

use std::collections::HashMap;

use crate::instr::{exec_instruction, Proc};
use crate::Elem;

pub fn execute<F: FieldElement>(
    length: usize,
    inputs: &Callback<F>,
    fixed: HashMap<String, Vec<F>>,
) -> Vec<(String, Vec<F>)> {
    println!("keys: {:?}", fixed.keys());

    let mut ctx = Context::new(length, inputs).with_fixed(fixed);

    ctx.run();

    vec![
        (
            "main._operation_id".to_string(),
            ctx._operation_id.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.pc".to_string(),
            ctx.pc.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.X".to_string(),
            ctx.X.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Y".to_string(),
            ctx.Y.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Z".to_string(),
            ctx.Z.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.W".to_string(),
            ctx.W.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_tmp1".to_string(),
            ctx.reg_write_X_tmp1
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_tmp1".to_string(),
            ctx.reg_write_Y_tmp1
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Z_tmp1".to_string(),
            ctx.reg_write_Z_tmp1
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.tmp1".to_string(),
            ctx.tmp1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_tmp2".to_string(),
            ctx.reg_write_X_tmp2
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Z_tmp2".to_string(),
            ctx.reg_write_Z_tmp2
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.tmp2".to_string(),
            ctx.tmp2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_tmp3".to_string(),
            ctx.reg_write_X_tmp3
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.tmp3".to_string(),
            ctx.tmp3.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.tmp4".to_string(),
            ctx.tmp4.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.lr_sc_reservation".to_string(),
            ctx.lr_sc_reservation
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_X_x0".to_string(),
            ctx.reg_write_X_x0
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_X_x1".to_string(),
            ctx.reg_write_X_x1
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x1".to_string(),
            ctx.reg_write_Y_x1
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x1".to_string(),
            ctx.x1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x2".to_string(),
            ctx.reg_write_X_x2
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x2".to_string(),
            ctx.x2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.x3".to_string(),
            ctx.x3.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.x4".to_string(),
            ctx.x4.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x5".to_string(),
            ctx.reg_write_X_x5
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x5".to_string(),
            ctx.x5.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x6".to_string(),
            ctx.reg_write_X_x6
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x6".to_string(),
            ctx.x6.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x7".to_string(),
            ctx.reg_write_X_x7
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x7".to_string(),
            ctx.x7.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x8".to_string(),
            ctx.reg_write_X_x8
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x8".to_string(),
            ctx.x8.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x9".to_string(),
            ctx.reg_write_X_x9
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x9".to_string(),
            ctx.x9.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x10".to_string(),
            ctx.reg_write_X_x10
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x10".to_string(),
            ctx.reg_write_Y_x10
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x10".to_string(),
            ctx.x10.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x11".to_string(),
            ctx.reg_write_X_x11
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x11".to_string(),
            ctx.x11.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x12".to_string(),
            ctx.reg_write_X_x12
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x12".to_string(),
            ctx.reg_write_Y_x12
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x12".to_string(),
            ctx.x12.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x13".to_string(),
            ctx.reg_write_X_x13
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x13".to_string(),
            ctx.reg_write_Y_x13
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x13".to_string(),
            ctx.x13.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x14".to_string(),
            ctx.reg_write_X_x14
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x14".to_string(),
            ctx.reg_write_Y_x14
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x14".to_string(),
            ctx.x14.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x15".to_string(),
            ctx.reg_write_X_x15
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x15".to_string(),
            ctx.x15.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x16".to_string(),
            ctx.reg_write_X_x16
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x16".to_string(),
            ctx.x16.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x17".to_string(),
            ctx.reg_write_X_x17
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x17".to_string(),
            ctx.x17.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x18".to_string(),
            ctx.reg_write_X_x18
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x18".to_string(),
            ctx.x18.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x19".to_string(),
            ctx.reg_write_X_x19
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x19".to_string(),
            ctx.x19.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x20".to_string(),
            ctx.reg_write_X_x20
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x20".to_string(),
            ctx.x20.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x21".to_string(),
            ctx.reg_write_X_x21
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x21".to_string(),
            ctx.x21.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x22".to_string(),
            ctx.reg_write_X_x22
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x22".to_string(),
            ctx.x22.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x23".to_string(),
            ctx.reg_write_X_x23
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.reg_write_Y_x23".to_string(),
            ctx.reg_write_Y_x23
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x23".to_string(),
            ctx.x23.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x24".to_string(),
            ctx.reg_write_X_x24
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x24".to_string(),
            ctx.x24.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x25".to_string(),
            ctx.reg_write_X_x25
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x25".to_string(),
            ctx.x25.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x26".to_string(),
            ctx.reg_write_X_x26
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x26".to_string(),
            ctx.x26.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x27".to_string(),
            ctx.reg_write_X_x27
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x27".to_string(),
            ctx.x27.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x28".to_string(),
            ctx.reg_write_X_x28
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x28".to_string(),
            ctx.x28.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x29".to_string(),
            ctx.reg_write_X_x29
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x29".to_string(),
            ctx.x29.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x30".to_string(),
            ctx.reg_write_X_x30
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x30".to_string(),
            ctx.x30.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.reg_write_X_x31".to_string(),
            ctx.reg_write_X_x31
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.x31".to_string(),
            ctx.x31.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_mload".to_string(),
            ctx.instr_mload.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_mstore".to_string(),
            ctx.instr_mstore.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_load_label".to_string(),
            ctx.instr_load_label
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_load_label_param_l".to_string(),
            ctx.instr_load_label_param_l
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_jump".to_string(),
            ctx.instr_jump.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_jump_param_l".to_string(),
            ctx.instr_jump_param_l
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_jump_dyn".to_string(),
            ctx.instr_jump_dyn
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_nonzero".to_string(),
            ctx.instr_branch_if_nonzero
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_nonzero_param_l".to_string(),
            ctx.instr_branch_if_nonzero_param_l
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_zero".to_string(),
            ctx.instr_branch_if_zero
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_zero_param_l".to_string(),
            ctx.instr_branch_if_zero_param_l
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_positive".to_string(),
            ctx.instr_branch_if_positive
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_branch_if_positive_param_l".to_string(),
            ctx.instr_branch_if_positive_param_l
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_is_positive".to_string(),
            ctx.instr_is_positive
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_and".to_string(),
            ctx.instr_and.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_or".to_string(),
            ctx.instr_or.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_xor".to_string(),
            ctx.instr_xor.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_shl".to_string(),
            ctx.instr_shl.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_shr".to_string(),
            ctx.instr_shr.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_wrap".to_string(),
            ctx.instr_wrap.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_wrap_signed".to_string(),
            ctx.instr_wrap_signed
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_sign_extend_byte".to_string(),
            ctx.instr_sign_extend_byte
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_to_signed".to_string(),
            ctx.instr_to_signed
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr_wrap16".to_string(),
            ctx.instr_wrap16.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_mul".to_string(),
            ctx.instr_mul.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr__jump_to_operation".to_string(),
            ctx.instr__jump_to_operation
                .iter()
                .map(|x| F::from(x.bin()))
                .collect(),
        ),
        (
            "main.instr__reset".to_string(),
            ctx.instr__reset.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr__loop".to_string(),
            ctx.instr__loop.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.instr_return".to_string(),
            ctx.instr_return.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x13".to_string(),
            ctx.read_W_x13.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x16".to_string(),
            ctx.read_W_x16.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x17".to_string(),
            ctx.read_W_x17.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x5".to_string(),
            ctx.read_W_x5.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x6".to_string(),
            ctx.read_W_x6.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_W_x7".to_string(),
            ctx.read_W_x7.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.X_const".to_string(),
            ctx.X_const.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.X_read_free".to_string(),
            ctx.X_read_free.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_tmp1".to_string(),
            ctx.read_X_tmp1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_tmp2".to_string(),
            ctx.read_X_tmp2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x0".to_string(),
            ctx.read_X_x0.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x1".to_string(),
            ctx.read_X_x1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x10".to_string(),
            ctx.read_X_x10.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x11".to_string(),
            ctx.read_X_x11.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x12".to_string(),
            ctx.read_X_x12.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x13".to_string(),
            ctx.read_X_x13.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x14".to_string(),
            ctx.read_X_x14.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x15".to_string(),
            ctx.read_X_x15.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x16".to_string(),
            ctx.read_X_x16.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x17".to_string(),
            ctx.read_X_x17.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x18".to_string(),
            ctx.read_X_x18.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x19".to_string(),
            ctx.read_X_x19.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x2".to_string(),
            ctx.read_X_x2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x20".to_string(),
            ctx.read_X_x20.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x21".to_string(),
            ctx.read_X_x21.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x22".to_string(),
            ctx.read_X_x22.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x23".to_string(),
            ctx.read_X_x23.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x24".to_string(),
            ctx.read_X_x24.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x25".to_string(),
            ctx.read_X_x25.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x26".to_string(),
            ctx.read_X_x26.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x27".to_string(),
            ctx.read_X_x27.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x28".to_string(),
            ctx.read_X_x28.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x29".to_string(),
            ctx.read_X_x29.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x31".to_string(),
            ctx.read_X_x31.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x5".to_string(),
            ctx.read_X_x5.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x6".to_string(),
            ctx.read_X_x6.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x7".to_string(),
            ctx.read_X_x7.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x8".to_string(),
            ctx.read_X_x8.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_X_x9".to_string(),
            ctx.read_X_x9.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Y_const".to_string(),
            ctx.Y_const.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Y_read_free".to_string(),
            ctx.Y_read_free.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_tmp1".to_string(),
            ctx.read_Y_tmp1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_tmp2".to_string(),
            ctx.read_Y_tmp2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_tmp3".to_string(),
            ctx.read_Y_tmp3.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x10".to_string(),
            ctx.read_Y_x10.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x11".to_string(),
            ctx.read_Y_x11.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x12".to_string(),
            ctx.read_Y_x12.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x13".to_string(),
            ctx.read_Y_x13.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x14".to_string(),
            ctx.read_Y_x14.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x15".to_string(),
            ctx.read_Y_x15.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x16".to_string(),
            ctx.read_Y_x16.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x18".to_string(),
            ctx.read_Y_x18.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x19".to_string(),
            ctx.read_Y_x19.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x2".to_string(),
            ctx.read_Y_x2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x20".to_string(),
            ctx.read_Y_x20.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x21".to_string(),
            ctx.read_Y_x21.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x22".to_string(),
            ctx.read_Y_x22.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x23".to_string(),
            ctx.read_Y_x23.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x24".to_string(),
            ctx.read_Y_x24.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x25".to_string(),
            ctx.read_Y_x25.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x26".to_string(),
            ctx.read_Y_x26.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x28".to_string(),
            ctx.read_Y_x28.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x29".to_string(),
            ctx.read_Y_x29.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x30".to_string(),
            ctx.read_Y_x30.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x31".to_string(),
            ctx.read_Y_x31.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x5".to_string(),
            ctx.read_Y_x5.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x6".to_string(),
            ctx.read_Y_x6.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x7".to_string(),
            ctx.read_Y_x7.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x8".to_string(),
            ctx.read_Y_x8.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Y_x9".to_string(),
            ctx.read_Y_x9.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Z_const".to_string(),
            ctx.Z_const.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Z_read_free".to_string(),
            ctx.Z_read_free.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_tmp1".to_string(),
            ctx.read_Z_tmp1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_tmp2".to_string(),
            ctx.read_Z_tmp2.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_tmp3".to_string(),
            ctx.read_Z_tmp3.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x0".to_string(),
            ctx.read_Z_x0.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x1".to_string(),
            ctx.read_Z_x1.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x10".to_string(),
            ctx.read_Z_x10.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x11".to_string(),
            ctx.read_Z_x11.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x12".to_string(),
            ctx.read_Z_x12.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x13".to_string(),
            ctx.read_Z_x13.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x14".to_string(),
            ctx.read_Z_x14.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x15".to_string(),
            ctx.read_Z_x15.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x17".to_string(),
            ctx.read_Z_x17.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x18".to_string(),
            ctx.read_Z_x18.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x19".to_string(),
            ctx.read_Z_x19.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x20".to_string(),
            ctx.read_Z_x20.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x21".to_string(),
            ctx.read_Z_x21.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x22".to_string(),
            ctx.read_Z_x22.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x23".to_string(),
            ctx.read_Z_x23.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x24".to_string(),
            ctx.read_Z_x24.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x25".to_string(),
            ctx.read_Z_x25.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x26".to_string(),
            ctx.read_Z_x26.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x27".to_string(),
            ctx.read_Z_x27.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x30".to_string(),
            ctx.read_Z_x30.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x8".to_string(),
            ctx.read_Z_x8.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.read_Z_x9".to_string(),
            ctx.read_Z_x9.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.W_free_value".to_string(),
            ctx.W_free_value.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.X_free_value".to_string(),
            ctx.X_free_value.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Y_free_value".to_string(),
            ctx.Y_free_value.iter().map(|x| F::from(x.bin())).collect(),
        ),
        (
            "main.Z_free_value".to_string(),
            ctx.Z_free_value.iter().map(|x| F::from(x.bin())).collect(),
        ),
    ]
}

type Callback<'a, F> = dyn powdr_executor::witgen::QueryCallback<F> + 'a;

#[allow(non_snake_case)]
struct Context<'a, F: FieldElement> {
    pub length: usize,
    pub current_row: usize,

    pub callback: &'a Callback<'a, F>,

    pub mem: HashMap<u32, u32>,

    pub fixed: HashMap<String, Vec<F>>,

    pub _operation_id: Vec<Elem<F>>,
    pub pc: Vec<Elem<F>>,
    pub X: Vec<Elem<F>>,
    pub Y: Vec<Elem<F>>,
    pub Z: Vec<Elem<F>>,
    pub W: Vec<Elem<F>>,
    pub reg_write_X_tmp1: Vec<Elem<F>>,
    pub reg_write_Y_tmp1: Vec<Elem<F>>,
    pub reg_write_Z_tmp1: Vec<Elem<F>>,
    pub tmp1: Vec<Elem<F>>,
    pub reg_write_X_tmp2: Vec<Elem<F>>,
    pub reg_write_Z_tmp2: Vec<Elem<F>>,
    pub tmp2: Vec<Elem<F>>,
    pub reg_write_X_tmp3: Vec<Elem<F>>,
    pub tmp3: Vec<Elem<F>>,
    pub tmp4: Vec<Elem<F>>,
    pub lr_sc_reservation: Vec<Elem<F>>,
    pub reg_write_X_x0: Vec<Elem<F>>,
    pub reg_write_X_x1: Vec<Elem<F>>,
    pub reg_write_Y_x1: Vec<Elem<F>>,
    pub x1: Vec<Elem<F>>,
    pub reg_write_X_x2: Vec<Elem<F>>,
    pub x2: Vec<Elem<F>>,
    pub x3: Vec<Elem<F>>,
    pub x4: Vec<Elem<F>>,
    pub reg_write_X_x5: Vec<Elem<F>>,
    pub x5: Vec<Elem<F>>,
    pub reg_write_X_x6: Vec<Elem<F>>,
    pub x6: Vec<Elem<F>>,
    pub reg_write_X_x7: Vec<Elem<F>>,
    pub x7: Vec<Elem<F>>,
    pub reg_write_X_x8: Vec<Elem<F>>,
    pub x8: Vec<Elem<F>>,
    pub reg_write_X_x9: Vec<Elem<F>>,
    pub x9: Vec<Elem<F>>,
    pub reg_write_X_x10: Vec<Elem<F>>,
    pub reg_write_Y_x10: Vec<Elem<F>>,
    pub x10: Vec<Elem<F>>,
    pub reg_write_X_x11: Vec<Elem<F>>,
    pub x11: Vec<Elem<F>>,
    pub reg_write_X_x12: Vec<Elem<F>>,
    pub reg_write_Y_x12: Vec<Elem<F>>,
    pub x12: Vec<Elem<F>>,
    pub reg_write_X_x13: Vec<Elem<F>>,
    pub reg_write_Y_x13: Vec<Elem<F>>,
    pub x13: Vec<Elem<F>>,
    pub reg_write_X_x14: Vec<Elem<F>>,
    pub reg_write_Y_x14: Vec<Elem<F>>,
    pub x14: Vec<Elem<F>>,
    pub reg_write_X_x15: Vec<Elem<F>>,
    pub x15: Vec<Elem<F>>,
    pub reg_write_X_x16: Vec<Elem<F>>,
    pub x16: Vec<Elem<F>>,
    pub reg_write_X_x17: Vec<Elem<F>>,
    pub x17: Vec<Elem<F>>,
    pub reg_write_X_x18: Vec<Elem<F>>,
    pub x18: Vec<Elem<F>>,
    pub reg_write_X_x19: Vec<Elem<F>>,
    pub x19: Vec<Elem<F>>,
    pub reg_write_X_x20: Vec<Elem<F>>,
    pub x20: Vec<Elem<F>>,
    pub reg_write_X_x21: Vec<Elem<F>>,
    pub x21: Vec<Elem<F>>,
    pub reg_write_X_x22: Vec<Elem<F>>,
    pub x22: Vec<Elem<F>>,
    pub reg_write_X_x23: Vec<Elem<F>>,
    pub reg_write_Y_x23: Vec<Elem<F>>,
    pub x23: Vec<Elem<F>>,
    pub reg_write_X_x24: Vec<Elem<F>>,
    pub x24: Vec<Elem<F>>,
    pub reg_write_X_x25: Vec<Elem<F>>,
    pub x25: Vec<Elem<F>>,
    pub reg_write_X_x26: Vec<Elem<F>>,
    pub x26: Vec<Elem<F>>,
    pub reg_write_X_x27: Vec<Elem<F>>,
    pub x27: Vec<Elem<F>>,
    pub reg_write_X_x28: Vec<Elem<F>>,
    pub x28: Vec<Elem<F>>,
    pub reg_write_X_x29: Vec<Elem<F>>,
    pub x29: Vec<Elem<F>>,
    pub reg_write_X_x30: Vec<Elem<F>>,
    pub x30: Vec<Elem<F>>,
    pub reg_write_X_x31: Vec<Elem<F>>,
    pub x31: Vec<Elem<F>>,
    pub instr_mload: Vec<Elem<F>>,
    pub instr_mstore: Vec<Elem<F>>,
    pub instr_load_label: Vec<Elem<F>>,
    pub instr_load_label_param_l: Vec<Elem<F>>,
    pub instr_jump: Vec<Elem<F>>,
    pub instr_jump_param_l: Vec<Elem<F>>,
    pub instr_jump_dyn: Vec<Elem<F>>,
    pub instr_branch_if_nonzero: Vec<Elem<F>>,
    pub instr_branch_if_nonzero_param_l: Vec<Elem<F>>,
    pub instr_branch_if_zero: Vec<Elem<F>>,
    pub instr_branch_if_zero_param_l: Vec<Elem<F>>,
    pub instr_branch_if_positive: Vec<Elem<F>>,
    pub instr_branch_if_positive_param_l: Vec<Elem<F>>,
    pub instr_is_positive: Vec<Elem<F>>,
    pub instr_and: Vec<Elem<F>>,
    pub instr_or: Vec<Elem<F>>,
    pub instr_xor: Vec<Elem<F>>,
    pub instr_shl: Vec<Elem<F>>,
    pub instr_shr: Vec<Elem<F>>,
    pub instr_wrap: Vec<Elem<F>>,
    pub instr_wrap_signed: Vec<Elem<F>>,
    pub instr_sign_extend_byte: Vec<Elem<F>>,
    pub instr_to_signed: Vec<Elem<F>>,
    pub instr_wrap16: Vec<Elem<F>>,
    pub instr_mul: Vec<Elem<F>>,
    pub instr__jump_to_operation: Vec<Elem<F>>,
    pub instr__reset: Vec<Elem<F>>,
    pub instr__loop: Vec<Elem<F>>,
    pub instr_return: Vec<Elem<F>>,
    pub read_W_x13: Vec<Elem<F>>,
    pub read_W_x16: Vec<Elem<F>>,
    pub read_W_x17: Vec<Elem<F>>,
    pub read_W_x5: Vec<Elem<F>>,
    pub read_W_x6: Vec<Elem<F>>,
    pub read_W_x7: Vec<Elem<F>>,
    pub X_const: Vec<Elem<F>>,
    pub X_read_free: Vec<Elem<F>>,
    pub read_X_tmp1: Vec<Elem<F>>,
    pub read_X_tmp2: Vec<Elem<F>>,
    pub read_X_x0: Vec<Elem<F>>,
    pub read_X_x1: Vec<Elem<F>>,
    pub read_X_x10: Vec<Elem<F>>,
    pub read_X_x11: Vec<Elem<F>>,
    pub read_X_x12: Vec<Elem<F>>,
    pub read_X_x13: Vec<Elem<F>>,
    pub read_X_x14: Vec<Elem<F>>,
    pub read_X_x15: Vec<Elem<F>>,
    pub read_X_x16: Vec<Elem<F>>,
    pub read_X_x17: Vec<Elem<F>>,
    pub read_X_x18: Vec<Elem<F>>,
    pub read_X_x19: Vec<Elem<F>>,
    pub read_X_x2: Vec<Elem<F>>,
    pub read_X_x20: Vec<Elem<F>>,
    pub read_X_x21: Vec<Elem<F>>,
    pub read_X_x22: Vec<Elem<F>>,
    pub read_X_x23: Vec<Elem<F>>,
    pub read_X_x24: Vec<Elem<F>>,
    pub read_X_x25: Vec<Elem<F>>,
    pub read_X_x26: Vec<Elem<F>>,
    pub read_X_x27: Vec<Elem<F>>,
    pub read_X_x28: Vec<Elem<F>>,
    pub read_X_x29: Vec<Elem<F>>,
    pub read_X_x31: Vec<Elem<F>>,
    pub read_X_x5: Vec<Elem<F>>,
    pub read_X_x6: Vec<Elem<F>>,
    pub read_X_x7: Vec<Elem<F>>,
    pub read_X_x8: Vec<Elem<F>>,
    pub read_X_x9: Vec<Elem<F>>,
    pub Y_const: Vec<Elem<F>>,
    pub Y_read_free: Vec<Elem<F>>,
    pub read_Y_tmp1: Vec<Elem<F>>,
    pub read_Y_tmp2: Vec<Elem<F>>,
    pub read_Y_tmp3: Vec<Elem<F>>,
    pub read_Y_x10: Vec<Elem<F>>,
    pub read_Y_x11: Vec<Elem<F>>,
    pub read_Y_x12: Vec<Elem<F>>,
    pub read_Y_x13: Vec<Elem<F>>,
    pub read_Y_x14: Vec<Elem<F>>,
    pub read_Y_x15: Vec<Elem<F>>,
    pub read_Y_x16: Vec<Elem<F>>,
    pub read_Y_x18: Vec<Elem<F>>,
    pub read_Y_x19: Vec<Elem<F>>,
    pub read_Y_x2: Vec<Elem<F>>,
    pub read_Y_x20: Vec<Elem<F>>,
    pub read_Y_x21: Vec<Elem<F>>,
    pub read_Y_x22: Vec<Elem<F>>,
    pub read_Y_x23: Vec<Elem<F>>,
    pub read_Y_x24: Vec<Elem<F>>,
    pub read_Y_x25: Vec<Elem<F>>,
    pub read_Y_x26: Vec<Elem<F>>,
    pub read_Y_x28: Vec<Elem<F>>,
    pub read_Y_x29: Vec<Elem<F>>,
    pub read_Y_x30: Vec<Elem<F>>,
    pub read_Y_x31: Vec<Elem<F>>,
    pub read_Y_x5: Vec<Elem<F>>,
    pub read_Y_x6: Vec<Elem<F>>,
    pub read_Y_x7: Vec<Elem<F>>,
    pub read_Y_x8: Vec<Elem<F>>,
    pub read_Y_x9: Vec<Elem<F>>,
    pub Z_const: Vec<Elem<F>>,
    pub Z_read_free: Vec<Elem<F>>,
    pub read_Z_tmp1: Vec<Elem<F>>,
    pub read_Z_tmp2: Vec<Elem<F>>,
    pub read_Z_tmp3: Vec<Elem<F>>,
    pub read_Z_x0: Vec<Elem<F>>,
    pub read_Z_x1: Vec<Elem<F>>,
    pub read_Z_x10: Vec<Elem<F>>,
    pub read_Z_x11: Vec<Elem<F>>,
    pub read_Z_x12: Vec<Elem<F>>,
    pub read_Z_x13: Vec<Elem<F>>,
    pub read_Z_x14: Vec<Elem<F>>,
    pub read_Z_x15: Vec<Elem<F>>,
    pub read_Z_x17: Vec<Elem<F>>,
    pub read_Z_x18: Vec<Elem<F>>,
    pub read_Z_x19: Vec<Elem<F>>,
    pub read_Z_x20: Vec<Elem<F>>,
    pub read_Z_x21: Vec<Elem<F>>,
    pub read_Z_x22: Vec<Elem<F>>,
    pub read_Z_x23: Vec<Elem<F>>,
    pub read_Z_x24: Vec<Elem<F>>,
    pub read_Z_x25: Vec<Elem<F>>,
    pub read_Z_x26: Vec<Elem<F>>,
    pub read_Z_x27: Vec<Elem<F>>,
    pub read_Z_x30: Vec<Elem<F>>,
    pub read_Z_x8: Vec<Elem<F>>,
    pub read_Z_x9: Vec<Elem<F>>,
    pub W_free_value: Vec<Elem<F>>,
    pub X_free_value: Vec<Elem<F>>,
    pub Y_free_value: Vec<Elem<F>>,
    pub Z_free_value: Vec<Elem<F>>,
}

impl<'a, F: FieldElement> Proc<F> for Context<'a, F> {
    fn get_pc(&self) -> Elem<F> {
        // TODO use {} -> self.pc
        *self.pc.last().unwrap()
    }
    fn set_pc(&mut self, pc: Elem<F>) {
        self.pc.push(pc);
    }
    fn get_mem(&self, addr: u32) -> u32 {
        *self.mem.get(&addr).unwrap_or(&0)
    }
    fn set_mem(&mut self, addr: u32, val: u32) {
        self.mem.insert(addr, val);
    }

    fn get_reg(&self, name: &str) -> Elem<F> {
        match name {
            "tmp1" => *self.tmp1.last().unwrap(),
            "tmp2" => *self.tmp2.last().unwrap(),
            "tmp3" => *self.tmp3.last().unwrap(),
            "tmp4" => *self.tmp4.last().unwrap(),
            "lr_sc_reservation" => *self.lr_sc_reservation.last().unwrap(),
            "x1" => *self.x1.last().unwrap(),
            "x2" => *self.x2.last().unwrap(),
            "x3" => *self.x3.last().unwrap(),
            "x4" => *self.x4.last().unwrap(),
            "x5" => *self.x5.last().unwrap(),
            "x6" => *self.x6.last().unwrap(),
            "x7" => *self.x7.last().unwrap(),
            "x8" => *self.x8.last().unwrap(),
            "x9" => *self.x9.last().unwrap(),
            "x10" => *self.x10.last().unwrap(),
            "x11" => *self.x11.last().unwrap(),
            "x12" => *self.x12.last().unwrap(),
            "x13" => *self.x13.last().unwrap(),
            "x14" => *self.x14.last().unwrap(),
            "x15" => *self.x15.last().unwrap(),
            "x16" => *self.x16.last().unwrap(),
            "x17" => *self.x17.last().unwrap(),
            "x18" => *self.x18.last().unwrap(),
            "x19" => *self.x19.last().unwrap(),
            "x20" => *self.x20.last().unwrap(),
            "x21" => *self.x21.last().unwrap(),
            "x22" => *self.x22.last().unwrap(),
            "x23" => *self.x23.last().unwrap(),
            "x24" => *self.x24.last().unwrap(),
            "x25" => *self.x25.last().unwrap(),
            "x26" => *self.x26.last().unwrap(),
            "x27" => *self.x27.last().unwrap(),
            "x28" => *self.x28.last().unwrap(),
            "x29" => *self.x29.last().unwrap(),
            "x30" => *self.x30.last().unwrap(),
            "x31" => *self.x31.last().unwrap(),
            _ => panic!("unknown register: {}", name),
        }
    }

    fn set_reg(&mut self, idx: &str, value: impl Into<Elem<F>>) {
        match idx {
            "tmp1" => self.tmp1.push(value.into()),
            "tmp2" => self.tmp2.push(value.into()),
            "tmp3" => self.tmp3.push(value.into()),
            "tmp4" => self.tmp4.push(value.into()),
            "lr_sc_reservation" => self.lr_sc_reservation.push(value.into()),
            "x1" => self.x1.push(value.into()),
            "x2" => self.x2.push(value.into()),
            "x3" => self.x3.push(value.into()),
            "x4" => self.x4.push(value.into()),
            "x5" => self.x5.push(value.into()),
            "x6" => self.x6.push(value.into()),
            "x7" => self.x7.push(value.into()),
            "x8" => self.x8.push(value.into()),
            "x9" => self.x9.push(value.into()),
            "x10" => self.x10.push(value.into()),
            "x11" => self.x11.push(value.into()),
            "x12" => self.x12.push(value.into()),
            "x13" => self.x13.push(value.into()),
            "x14" => self.x14.push(value.into()),
            "x15" => self.x15.push(value.into()),
            "x16" => self.x16.push(value.into()),
            "x17" => self.x17.push(value.into()),
            "x18" => self.x18.push(value.into()),
            "x19" => self.x19.push(value.into()),
            "x20" => self.x20.push(value.into()),
            "x21" => self.x21.push(value.into()),
            "x22" => self.x22.push(value.into()),
            "x23" => self.x23.push(value.into()),
            "x24" => self.x24.push(value.into()),
            "x25" => self.x25.push(value.into()),
            "x26" => self.x26.push(value.into()),
            "x27" => self.x27.push(value.into()),
            "x28" => self.x28.push(value.into()),
            "x29" => self.x29.push(value.into()),
            "x30" => self.x30.push(value.into()),
            "x31" => self.x31.push(value.into()),
            _ => panic!("unknown register: {}", idx),
        }
    }
}

impl<'a, F: FieldElement> Context<'a, F> {
    pub fn new(length: usize, callback: &'a Callback<F>) -> Self {
        Self {
            length,
            current_row: 0,
            callback,
            mem: HashMap::new(),
            fixed: HashMap::new(),

            _operation_id: Vec::new(),
            pc: Vec::new(),
            X: Vec::new(),
            Y: Vec::new(),
            Z: Vec::new(),
            W: Vec::new(),
            reg_write_X_tmp1: Vec::new(),
            reg_write_Y_tmp1: Vec::new(),
            reg_write_Z_tmp1: Vec::new(),
            tmp1: Vec::new(),
            reg_write_X_tmp2: Vec::new(),
            reg_write_Z_tmp2: Vec::new(),
            tmp2: Vec::new(),
            reg_write_X_tmp3: Vec::new(),
            tmp3: Vec::new(),
            tmp4: Vec::new(),
            lr_sc_reservation: Vec::new(),
            reg_write_X_x0: Vec::new(),
            reg_write_X_x1: Vec::new(),
            reg_write_Y_x1: Vec::new(),
            x1: Vec::new(),
            reg_write_X_x2: Vec::new(),
            x2: Vec::new(),
            x3: Vec::new(),
            x4: Vec::new(),
            reg_write_X_x5: Vec::new(),
            x5: Vec::new(),
            reg_write_X_x6: Vec::new(),
            x6: Vec::new(),
            reg_write_X_x7: Vec::new(),
            x7: Vec::new(),
            reg_write_X_x8: Vec::new(),
            x8: Vec::new(),
            reg_write_X_x9: Vec::new(),
            x9: Vec::new(),
            reg_write_X_x10: Vec::new(),
            reg_write_Y_x10: Vec::new(),
            x10: Vec::new(),
            reg_write_X_x11: Vec::new(),
            x11: Vec::new(),
            reg_write_X_x12: Vec::new(),
            reg_write_Y_x12: Vec::new(),
            x12: Vec::new(),
            reg_write_X_x13: Vec::new(),
            reg_write_Y_x13: Vec::new(),
            x13: Vec::new(),
            reg_write_X_x14: Vec::new(),
            reg_write_Y_x14: Vec::new(),
            x14: Vec::new(),
            reg_write_X_x15: Vec::new(),
            x15: Vec::new(),
            reg_write_X_x16: Vec::new(),
            x16: Vec::new(),
            reg_write_X_x17: Vec::new(),
            x17: Vec::new(),
            reg_write_X_x18: Vec::new(),
            x18: Vec::new(),
            reg_write_X_x19: Vec::new(),
            x19: Vec::new(),
            reg_write_X_x20: Vec::new(),
            x20: Vec::new(),
            reg_write_X_x21: Vec::new(),
            x21: Vec::new(),
            reg_write_X_x22: Vec::new(),
            x22: Vec::new(),
            reg_write_X_x23: Vec::new(),
            reg_write_Y_x23: Vec::new(),
            x23: Vec::new(),
            reg_write_X_x24: Vec::new(),
            x24: Vec::new(),
            reg_write_X_x25: Vec::new(),
            x25: Vec::new(),
            reg_write_X_x26: Vec::new(),
            x26: Vec::new(),
            reg_write_X_x27: Vec::new(),
            x27: Vec::new(),
            reg_write_X_x28: Vec::new(),
            x28: Vec::new(),
            reg_write_X_x29: Vec::new(),
            x29: Vec::new(),
            reg_write_X_x30: Vec::new(),
            x30: Vec::new(),
            reg_write_X_x31: Vec::new(),
            x31: Vec::new(),
            instr_mload: Vec::new(),
            instr_mstore: Vec::new(),
            instr_load_label: Vec::new(),
            instr_load_label_param_l: Vec::new(),
            instr_jump: Vec::new(),
            instr_jump_param_l: Vec::new(),
            instr_jump_dyn: Vec::new(),
            instr_branch_if_nonzero: Vec::new(),
            instr_branch_if_nonzero_param_l: Vec::new(),
            instr_branch_if_zero: Vec::new(),
            instr_branch_if_zero_param_l: Vec::new(),
            instr_branch_if_positive: Vec::new(),
            instr_branch_if_positive_param_l: Vec::new(),
            instr_is_positive: Vec::new(),
            instr_and: Vec::new(),
            instr_or: Vec::new(),
            instr_xor: Vec::new(),
            instr_shl: Vec::new(),
            instr_shr: Vec::new(),
            instr_wrap: Vec::new(),
            instr_wrap_signed: Vec::new(),
            instr_sign_extend_byte: Vec::new(),
            instr_to_signed: Vec::new(),
            instr_wrap16: Vec::new(),
            instr_mul: Vec::new(),
            instr__jump_to_operation: Vec::new(),
            instr__reset: Vec::new(),
            instr__loop: Vec::new(),
            instr_return: Vec::new(),
            read_W_x13: Vec::new(),
            read_W_x16: Vec::new(),
            read_W_x17: Vec::new(),
            read_W_x5: Vec::new(),
            read_W_x6: Vec::new(),
            read_W_x7: Vec::new(),
            X_const: Vec::new(),
            X_read_free: Vec::new(),
            read_X_tmp1: Vec::new(),
            read_X_tmp2: Vec::new(),
            read_X_x0: Vec::new(),
            read_X_x1: Vec::new(),
            read_X_x10: Vec::new(),
            read_X_x11: Vec::new(),
            read_X_x12: Vec::new(),
            read_X_x13: Vec::new(),
            read_X_x14: Vec::new(),
            read_X_x15: Vec::new(),
            read_X_x16: Vec::new(),
            read_X_x17: Vec::new(),
            read_X_x18: Vec::new(),
            read_X_x19: Vec::new(),
            read_X_x2: Vec::new(),
            read_X_x20: Vec::new(),
            read_X_x21: Vec::new(),
            read_X_x22: Vec::new(),
            read_X_x23: Vec::new(),
            read_X_x24: Vec::new(),
            read_X_x25: Vec::new(),
            read_X_x26: Vec::new(),
            read_X_x27: Vec::new(),
            read_X_x28: Vec::new(),
            read_X_x29: Vec::new(),
            read_X_x31: Vec::new(),
            read_X_x5: Vec::new(),
            read_X_x6: Vec::new(),
            read_X_x7: Vec::new(),
            read_X_x8: Vec::new(),
            read_X_x9: Vec::new(),
            Y_const: Vec::new(),
            Y_read_free: Vec::new(),
            read_Y_tmp1: Vec::new(),
            read_Y_tmp2: Vec::new(),
            read_Y_tmp3: Vec::new(),
            read_Y_x10: Vec::new(),
            read_Y_x11: Vec::new(),
            read_Y_x12: Vec::new(),
            read_Y_x13: Vec::new(),
            read_Y_x14: Vec::new(),
            read_Y_x15: Vec::new(),
            read_Y_x16: Vec::new(),
            read_Y_x18: Vec::new(),
            read_Y_x19: Vec::new(),
            read_Y_x2: Vec::new(),
            read_Y_x20: Vec::new(),
            read_Y_x21: Vec::new(),
            read_Y_x22: Vec::new(),
            read_Y_x23: Vec::new(),
            read_Y_x24: Vec::new(),
            read_Y_x25: Vec::new(),
            read_Y_x26: Vec::new(),
            read_Y_x28: Vec::new(),
            read_Y_x29: Vec::new(),
            read_Y_x30: Vec::new(),
            read_Y_x31: Vec::new(),
            read_Y_x5: Vec::new(),
            read_Y_x6: Vec::new(),
            read_Y_x7: Vec::new(),
            read_Y_x8: Vec::new(),
            read_Y_x9: Vec::new(),
            Z_const: Vec::new(),
            Z_read_free: Vec::new(),
            read_Z_tmp1: Vec::new(),
            read_Z_tmp2: Vec::new(),
            read_Z_tmp3: Vec::new(),
            read_Z_x0: Vec::new(),
            read_Z_x1: Vec::new(),
            read_Z_x10: Vec::new(),
            read_Z_x11: Vec::new(),
            read_Z_x12: Vec::new(),
            read_Z_x13: Vec::new(),
            read_Z_x14: Vec::new(),
            read_Z_x15: Vec::new(),
            read_Z_x17: Vec::new(),
            read_Z_x18: Vec::new(),
            read_Z_x19: Vec::new(),
            read_Z_x20: Vec::new(),
            read_Z_x21: Vec::new(),
            read_Z_x22: Vec::new(),
            read_Z_x23: Vec::new(),
            read_Z_x24: Vec::new(),
            read_Z_x25: Vec::new(),
            read_Z_x26: Vec::new(),
            read_Z_x27: Vec::new(),
            read_Z_x30: Vec::new(),
            read_Z_x8: Vec::new(),
            read_Z_x9: Vec::new(),
            W_free_value: Vec::new(),
            X_free_value: Vec::new(),
            Y_free_value: Vec::new(),
            Z_free_value: Vec::new(),
        }
    }

    pub fn with_fixed(mut self, fixed: HashMap<String, Vec<F>>) -> Self {
        self.fixed = fixed;
        self
    }

    fn update(&mut self) {
        // order matters here:
        // - the starting point is pc = 0, state registers = 0
        // - update the control flow flags
        // - update the instruction flags
        // - update writes from state registers, contants, and free values into assignment registers
        // - run the instructions
        // - update writes from assignment registers into state registers
        // - all assignment registers should have been updated by now
        // - update pc

        self.update_control_flow_flags();
        self.update_flags();
        self.update_inputs();
        self.update_writes_to_assignment_registers();

        if self.current_row < self.length - 1 {
            self.run_instructions();
            self.update_writes_to_state_registers();
            self.update_pc();
        }
    }

    pub fn run(&mut self) {
        self.init();

        while self.current_row < self.length {
            self.update();
            self.current_row += 1;
        }

        // Leo: can remove this for now, maybe Georg's PR already solves it
        // TODO fix

        *self.tmp1.first_mut().unwrap() = *self.tmp1.last().unwrap();
        *self.tmp2.first_mut().unwrap() = *self.tmp2.last().unwrap();
        *self.tmp3.first_mut().unwrap() = *self.tmp3.last().unwrap();
        *self.tmp4.first_mut().unwrap() = *self.tmp4.last().unwrap();
        *self.lr_sc_reservation.first_mut().unwrap() = *self.lr_sc_reservation.last().unwrap();
        *self.x1.first_mut().unwrap() = *self.x1.last().unwrap();
        *self.x2.first_mut().unwrap() = *self.x2.last().unwrap();
        *self.x3.first_mut().unwrap() = *self.x3.last().unwrap();
        *self.x4.first_mut().unwrap() = *self.x4.last().unwrap();
        *self.x5.first_mut().unwrap() = *self.x5.last().unwrap();
        *self.x6.first_mut().unwrap() = *self.x6.last().unwrap();
        *self.x7.first_mut().unwrap() = *self.x7.last().unwrap();
        *self.x8.first_mut().unwrap() = *self.x8.last().unwrap();
        *self.x9.first_mut().unwrap() = *self.x9.last().unwrap();
        *self.x10.first_mut().unwrap() = *self.x10.last().unwrap();
        *self.x11.first_mut().unwrap() = *self.x11.last().unwrap();
        *self.x12.first_mut().unwrap() = *self.x12.last().unwrap();
        *self.x13.first_mut().unwrap() = *self.x13.last().unwrap();
        *self.x14.first_mut().unwrap() = *self.x14.last().unwrap();
        *self.x15.first_mut().unwrap() = *self.x15.last().unwrap();
        *self.x16.first_mut().unwrap() = *self.x16.last().unwrap();
        *self.x17.first_mut().unwrap() = *self.x17.last().unwrap();
        *self.x18.first_mut().unwrap() = *self.x18.last().unwrap();
        *self.x19.first_mut().unwrap() = *self.x19.last().unwrap();
        *self.x20.first_mut().unwrap() = *self.x20.last().unwrap();
        *self.x21.first_mut().unwrap() = *self.x21.last().unwrap();
        *self.x22.first_mut().unwrap() = *self.x22.last().unwrap();
        *self.x23.first_mut().unwrap() = *self.x23.last().unwrap();
        *self.x24.first_mut().unwrap() = *self.x24.last().unwrap();
        *self.x25.first_mut().unwrap() = *self.x25.last().unwrap();
        *self.x26.first_mut().unwrap() = *self.x26.last().unwrap();
        *self.x27.first_mut().unwrap() = *self.x27.last().unwrap();
        *self.x28.first_mut().unwrap() = *self.x28.last().unwrap();
        *self.x29.first_mut().unwrap() = *self.x29.last().unwrap();
        *self.x30.first_mut().unwrap() = *self.x30.last().unwrap();
        *self.x31.first_mut().unwrap() = *self.x31.last().unwrap();
    }

    // for pc + each state register
    fn init(&mut self) {
        self.tmp1.push(0.into());
        self.tmp2.push(0.into());
        self.tmp3.push(0.into());
        self.tmp4.push(0.into());
        self.lr_sc_reservation.push(0.into());
        self.x1.push(0.into());
        self.x2.push(0.into());
        self.x3.push(0.into());
        self.x4.push(0.into());
        self.x5.push(0.into());
        self.x6.push(0.into());
        self.x7.push(0.into());
        self.x8.push(0.into());
        self.x9.push(0.into());
        self.x10.push(0.into());
        self.x11.push(0.into());
        self.x12.push(0.into());
        self.x13.push(0.into());
        self.x14.push(0.into());
        self.x15.push(0.into());
        self.x16.push(0.into());
        self.x17.push(0.into());
        self.x18.push(0.into());
        self.x19.push(0.into());
        self.x20.push(0.into());
        self.x21.push(0.into());
        self.x22.push(0.into());
        self.x23.push(0.into());
        self.x24.push(0.into());
        self.x25.push(0.into());
        self.x26.push(0.into());
        self.x27.push(0.into());
        self.x28.push(0.into());
        self.x29.push(0.into());
        self.x30.push(0.into());
        self.x31.push(0.into());
        self.pc.push(0.into());
    }

    fn instr_instr_mload(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("mload", &args, self);
        assert_eq!(res.len(), 2);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
        *self.Z.last_mut().unwrap() = res[1].clone();
        *self.Z_free_value.last_mut().unwrap() = res[1];
    }
    fn instr_instr_mstore(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("mstore", &args, self);
        assert_eq!(res.len(), 0);
    }
    fn instr_instr_load_label(&mut self) {
        let args = vec![self.instr_load_label_param_l.last().unwrap().clone()];
        let res = exec_instruction("load_label", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_jump(&mut self) {
        let args = vec![self.instr_jump_param_l.last().unwrap().clone()];
        let res = exec_instruction("jump", &args, self);
        assert_eq!(res.len(), 1);
        *self.Y.last_mut().unwrap() = res[0].clone();
        *self.Y_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_jump_dyn(&mut self) {
        let args = vec![self.X.last().unwrap().clone()];
        let res = exec_instruction("jump_dyn", &args, self);
        assert_eq!(res.len(), 1);
        *self.Y.last_mut().unwrap() = res[0].clone();
        *self.Y_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_branch_if_nonzero(&mut self) {
        let args = vec![
            self.X.last().unwrap().clone(),
            self.instr_branch_if_nonzero_param_l.last().unwrap().clone(),
        ];
        let res = exec_instruction("branch_if_nonzero", &args, self);
        assert_eq!(res.len(), 0);
    }
    fn instr_instr_branch_if_zero(&mut self) {
        let args = vec![
            self.X.last().unwrap().clone(),
            self.instr_branch_if_zero_param_l.last().unwrap().clone(),
        ];
        let res = exec_instruction("branch_if_zero", &args, self);
        assert_eq!(res.len(), 0);
    }
    fn instr_instr_branch_if_positive(&mut self) {
        let args = vec![
            self.X.last().unwrap().clone(),
            self.instr_branch_if_positive_param_l
                .last()
                .unwrap()
                .clone(),
        ];
        let res = exec_instruction("branch_if_positive", &args, self);
        assert_eq!(res.len(), 0);
    }
    fn instr_instr_is_positive(&mut self) {
        let args = vec![self.X.last().unwrap().clone()];
        let res = exec_instruction("is_positive", &args, self);
        assert_eq!(res.len(), 1);
        *self.Y.last_mut().unwrap() = res[0].clone();
        *self.Y_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_and(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("and", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_or(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("or", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_xor(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("xor", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_shl(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("shl", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_shr(&mut self) {
        let args = vec![
            self.Y.last().unwrap().clone(),
            self.Z.last().unwrap().clone(),
        ];
        let res = exec_instruction("shr", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_wrap(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("wrap", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_wrap_signed(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("wrap_signed", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_sign_extend_byte(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("sign_extend_byte", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_to_signed(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("to_signed", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_wrap16(&mut self) {
        let args = vec![self.Y.last().unwrap().clone()];
        let res = exec_instruction("wrap16", &args, self);
        assert_eq!(res.len(), 1);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
    }
    fn instr_instr_mul(&mut self) {
        let args = vec![
            self.Z.last().unwrap().clone(),
            self.W.last().unwrap().clone(),
        ];
        let res = exec_instruction("mul", &args, self);
        assert_eq!(res.len(), 2);
        *self.X.last_mut().unwrap() = res[0].clone();
        *self.X_free_value.last_mut().unwrap() = res[0];
        *self.Y.last_mut().unwrap() = res[1].clone();
        *self.Y_free_value.last_mut().unwrap() = res[1];
    }

    fn run_instructions(&mut self) {
        let instr_mload = self.instr_mload.last().unwrap();
        let instr_mstore = self.instr_mstore.last().unwrap();
        let instr_load_label = self.instr_load_label.last().unwrap();
        let instr_jump = self.instr_jump.last().unwrap();
        let instr_jump_dyn = self.instr_jump_dyn.last().unwrap();
        let instr_branch_if_nonzero = self.instr_branch_if_nonzero.last().unwrap();
        let instr_branch_if_zero = self.instr_branch_if_zero.last().unwrap();
        let instr_branch_if_positive = self.instr_branch_if_positive.last().unwrap();
        let instr_is_positive = self.instr_is_positive.last().unwrap();
        let instr_and = self.instr_and.last().unwrap();
        let instr_or = self.instr_or.last().unwrap();
        let instr_xor = self.instr_xor.last().unwrap();
        let instr_shl = self.instr_shl.last().unwrap();
        let instr_shr = self.instr_shr.last().unwrap();
        let instr_wrap = self.instr_wrap.last().unwrap();
        let instr_wrap_signed = self.instr_wrap_signed.last().unwrap();
        let instr_sign_extend_byte = self.instr_sign_extend_byte.last().unwrap();
        let instr_to_signed = self.instr_to_signed.last().unwrap();
        let instr_wrap16 = self.instr_wrap16.last().unwrap();
        let instr_mul = self.instr_mul.last().unwrap();
        if instr_mload.is_one() {
            self.instr_instr_mload();
        } else if instr_mstore.is_one() {
            self.instr_instr_mstore();
        } else if instr_load_label.is_one() {
            self.instr_instr_load_label();
        } else if instr_jump.is_one() {
            self.instr_instr_jump();
        } else if instr_jump_dyn.is_one() {
            self.instr_instr_jump_dyn();
        } else if instr_branch_if_nonzero.is_one() {
            self.instr_instr_branch_if_nonzero();
        } else if instr_branch_if_zero.is_one() {
            self.instr_instr_branch_if_zero();
        } else if instr_branch_if_positive.is_one() {
            self.instr_instr_branch_if_positive();
        } else if instr_is_positive.is_one() {
            self.instr_instr_is_positive();
        } else if instr_and.is_one() {
            self.instr_instr_and();
        } else if instr_or.is_one() {
            self.instr_instr_or();
        } else if instr_xor.is_one() {
            self.instr_instr_xor();
        } else if instr_shl.is_one() {
            self.instr_instr_shl();
        } else if instr_shr.is_one() {
            self.instr_instr_shr();
        } else if instr_wrap.is_one() {
            self.instr_instr_wrap();
        } else if instr_wrap_signed.is_one() {
            self.instr_instr_wrap_signed();
        } else if instr_sign_extend_byte.is_one() {
            self.instr_instr_sign_extend_byte();
        } else if instr_to_signed.is_one() {
            self.instr_instr_to_signed();
        } else if instr_wrap16.is_one() {
            self.instr_instr_wrap16();
        } else if instr_mul.is_one() {
            self.instr_instr_mul();
        } else {
        }
    }

    fn query(&self, query: &String) -> Elem<F> {
        match (self.callback)(query).unwrap() {
            Some(val) => Elem::new_from_fe_as_bin(&val),
            None => {
                panic!("unknown query command: {query}");
            }
        }
    }

    // for each assignment register
    fn update_inputs(&mut self) {
        let pc = self.pc.last().unwrap().bin();
        let prime = if self.X_read_free.last().unwrap().is_one() {
            if pc == 1527 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1551 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1566 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1568 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1570 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1572 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1574 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1576 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1578 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1580 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1582 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1584 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1586 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1588 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1590 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1592 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1594 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1596 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1598 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1600 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1602 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1604 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1606 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1608 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1610 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1612 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1614 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1616 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1618 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1620 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1622 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1624 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1626 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1628 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1630 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1632 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1634 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1636 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1638 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1640 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1642 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1644 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1646 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1648 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1650 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1652 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1654 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1656 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1658 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1660 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1662 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1664 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1666 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1668 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1670 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1672 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1674 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1676 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1678 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1680 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1682 {
                self.query(&format!("(\"print_char\", {})", self.x10.last().unwrap()))
            } else if pc == 1799 {
                self.query(&format!("(\"input\", {})", self.x10.last().unwrap()))
            } else if pc == 1803 {
                self.query(&format!("(\"input\", {})", self.x10.last().unwrap()))
            } else if pc == 1832 {
                self.query(&format!("(\"input\", {})", self.x10.last().unwrap()))
            } else {
                0.into()
            }
        } else {
            0.into()
        };
        self.X_free_value.push(prime);
        self.Y_free_value.push(0.into());
        self.Z_free_value.push(0.into());
        self.W_free_value.push(0.into());
    }

    #[allow(non_snake_case)]
    fn update_writes_to_assignment_registers(&mut self) {
        let mut X_prime: i64 = self.X_const.last().unwrap().bin();
        let mut Y_prime: i64 = self.Y_const.last().unwrap().bin();
        let mut Z_prime: i64 = self.Z_const.last().unwrap().bin();
        let mut W_prime: i64 = 0;

        X_prime += self.read_X_tmp1.last().unwrap().bin() * self.tmp1.last().unwrap().bin();

        X_prime += self.read_X_tmp2.last().unwrap().bin() * self.tmp2.last().unwrap().bin();

        X_prime += self.read_X_x1.last().unwrap().bin() * self.x1.last().unwrap().bin();

        X_prime += self.read_X_x2.last().unwrap().bin() * self.x2.last().unwrap().bin();

        X_prime += self.read_X_x5.last().unwrap().bin() * self.x5.last().unwrap().bin();

        X_prime += self.read_X_x6.last().unwrap().bin() * self.x6.last().unwrap().bin();

        X_prime += self.read_X_x7.last().unwrap().bin() * self.x7.last().unwrap().bin();

        X_prime += self.read_X_x8.last().unwrap().bin() * self.x8.last().unwrap().bin();

        X_prime += self.read_X_x9.last().unwrap().bin() * self.x9.last().unwrap().bin();

        X_prime += self.read_X_x10.last().unwrap().bin() * self.x10.last().unwrap().bin();

        X_prime += self.read_X_x11.last().unwrap().bin() * self.x11.last().unwrap().bin();

        X_prime += self.read_X_x12.last().unwrap().bin() * self.x12.last().unwrap().bin();

        X_prime += self.read_X_x13.last().unwrap().bin() * self.x13.last().unwrap().bin();

        X_prime += self.read_X_x14.last().unwrap().bin() * self.x14.last().unwrap().bin();

        X_prime += self.read_X_x15.last().unwrap().bin() * self.x15.last().unwrap().bin();

        X_prime += self.read_X_x16.last().unwrap().bin() * self.x16.last().unwrap().bin();

        X_prime += self.read_X_x17.last().unwrap().bin() * self.x17.last().unwrap().bin();

        X_prime += self.read_X_x18.last().unwrap().bin() * self.x18.last().unwrap().bin();

        X_prime += self.read_X_x19.last().unwrap().bin() * self.x19.last().unwrap().bin();

        X_prime += self.read_X_x20.last().unwrap().bin() * self.x20.last().unwrap().bin();

        X_prime += self.read_X_x21.last().unwrap().bin() * self.x21.last().unwrap().bin();

        X_prime += self.read_X_x22.last().unwrap().bin() * self.x22.last().unwrap().bin();

        X_prime += self.read_X_x23.last().unwrap().bin() * self.x23.last().unwrap().bin();

        X_prime += self.read_X_x24.last().unwrap().bin() * self.x24.last().unwrap().bin();

        X_prime += self.read_X_x25.last().unwrap().bin() * self.x25.last().unwrap().bin();

        X_prime += self.read_X_x26.last().unwrap().bin() * self.x26.last().unwrap().bin();

        X_prime += self.read_X_x27.last().unwrap().bin() * self.x27.last().unwrap().bin();

        X_prime += self.read_X_x28.last().unwrap().bin() * self.x28.last().unwrap().bin();

        X_prime += self.read_X_x29.last().unwrap().bin() * self.x29.last().unwrap().bin();

        X_prime += self.read_X_x31.last().unwrap().bin() * self.x31.last().unwrap().bin();

        Y_prime += self.read_Y_tmp1.last().unwrap().bin() * self.tmp1.last().unwrap().bin();

        Y_prime += self.read_Y_tmp2.last().unwrap().bin() * self.tmp2.last().unwrap().bin();

        Y_prime += self.read_Y_tmp3.last().unwrap().bin() * self.tmp3.last().unwrap().bin();

        Y_prime += self.read_Y_x2.last().unwrap().bin() * self.x2.last().unwrap().bin();

        Y_prime += self.read_Y_x5.last().unwrap().bin() * self.x5.last().unwrap().bin();

        Y_prime += self.read_Y_x6.last().unwrap().bin() * self.x6.last().unwrap().bin();

        Y_prime += self.read_Y_x7.last().unwrap().bin() * self.x7.last().unwrap().bin();

        Y_prime += self.read_Y_x8.last().unwrap().bin() * self.x8.last().unwrap().bin();

        Y_prime += self.read_Y_x9.last().unwrap().bin() * self.x9.last().unwrap().bin();

        Y_prime += self.read_Y_x10.last().unwrap().bin() * self.x10.last().unwrap().bin();

        Y_prime += self.read_Y_x11.last().unwrap().bin() * self.x11.last().unwrap().bin();

        Y_prime += self.read_Y_x12.last().unwrap().bin() * self.x12.last().unwrap().bin();

        Y_prime += self.read_Y_x13.last().unwrap().bin() * self.x13.last().unwrap().bin();

        Y_prime += self.read_Y_x14.last().unwrap().bin() * self.x14.last().unwrap().bin();

        Y_prime += self.read_Y_x15.last().unwrap().bin() * self.x15.last().unwrap().bin();

        Y_prime += self.read_Y_x16.last().unwrap().bin() * self.x16.last().unwrap().bin();

        Y_prime += self.read_Y_x18.last().unwrap().bin() * self.x18.last().unwrap().bin();

        Y_prime += self.read_Y_x19.last().unwrap().bin() * self.x19.last().unwrap().bin();

        Y_prime += self.read_Y_x20.last().unwrap().bin() * self.x20.last().unwrap().bin();

        Y_prime += self.read_Y_x21.last().unwrap().bin() * self.x21.last().unwrap().bin();

        Y_prime += self.read_Y_x22.last().unwrap().bin() * self.x22.last().unwrap().bin();

        Y_prime += self.read_Y_x23.last().unwrap().bin() * self.x23.last().unwrap().bin();

        Y_prime += self.read_Y_x24.last().unwrap().bin() * self.x24.last().unwrap().bin();

        Y_prime += self.read_Y_x25.last().unwrap().bin() * self.x25.last().unwrap().bin();

        Y_prime += self.read_Y_x26.last().unwrap().bin() * self.x26.last().unwrap().bin();

        Y_prime += self.read_Y_x28.last().unwrap().bin() * self.x28.last().unwrap().bin();

        Y_prime += self.read_Y_x29.last().unwrap().bin() * self.x29.last().unwrap().bin();

        Y_prime += self.read_Y_x30.last().unwrap().bin() * self.x30.last().unwrap().bin();

        Y_prime += self.read_Y_x31.last().unwrap().bin() * self.x31.last().unwrap().bin();

        Z_prime += self.read_Z_tmp1.last().unwrap().bin() * self.tmp1.last().unwrap().bin();

        Z_prime += self.read_Z_tmp2.last().unwrap().bin() * self.tmp2.last().unwrap().bin();

        Z_prime += self.read_Z_tmp3.last().unwrap().bin() * self.tmp3.last().unwrap().bin();

        Z_prime += self.read_Z_x1.last().unwrap().bin() * self.x1.last().unwrap().bin();

        Z_prime += self.read_Z_x8.last().unwrap().bin() * self.x8.last().unwrap().bin();

        Z_prime += self.read_Z_x9.last().unwrap().bin() * self.x9.last().unwrap().bin();

        Z_prime += self.read_Z_x10.last().unwrap().bin() * self.x10.last().unwrap().bin();

        Z_prime += self.read_Z_x11.last().unwrap().bin() * self.x11.last().unwrap().bin();

        Z_prime += self.read_Z_x12.last().unwrap().bin() * self.x12.last().unwrap().bin();

        Z_prime += self.read_Z_x13.last().unwrap().bin() * self.x13.last().unwrap().bin();

        Z_prime += self.read_Z_x14.last().unwrap().bin() * self.x14.last().unwrap().bin();

        Z_prime += self.read_Z_x15.last().unwrap().bin() * self.x15.last().unwrap().bin();

        Z_prime += self.read_Z_x17.last().unwrap().bin() * self.x17.last().unwrap().bin();

        Z_prime += self.read_Z_x18.last().unwrap().bin() * self.x18.last().unwrap().bin();

        Z_prime += self.read_Z_x19.last().unwrap().bin() * self.x19.last().unwrap().bin();

        Z_prime += self.read_Z_x20.last().unwrap().bin() * self.x20.last().unwrap().bin();

        Z_prime += self.read_Z_x21.last().unwrap().bin() * self.x21.last().unwrap().bin();

        Z_prime += self.read_Z_x22.last().unwrap().bin() * self.x22.last().unwrap().bin();

        Z_prime += self.read_Z_x23.last().unwrap().bin() * self.x23.last().unwrap().bin();

        Z_prime += self.read_Z_x24.last().unwrap().bin() * self.x24.last().unwrap().bin();

        Z_prime += self.read_Z_x25.last().unwrap().bin() * self.x25.last().unwrap().bin();

        Z_prime += self.read_Z_x26.last().unwrap().bin() * self.x26.last().unwrap().bin();

        Z_prime += self.read_Z_x27.last().unwrap().bin() * self.x27.last().unwrap().bin();

        Z_prime += self.read_Z_x30.last().unwrap().bin() * self.x30.last().unwrap().bin();

        W_prime += self.read_W_x5.last().unwrap().bin() * self.x5.last().unwrap().bin();

        W_prime += self.read_W_x6.last().unwrap().bin() * self.x6.last().unwrap().bin();

        W_prime += self.read_W_x7.last().unwrap().bin() * self.x7.last().unwrap().bin();

        W_prime += self.read_W_x13.last().unwrap().bin() * self.x13.last().unwrap().bin();

        W_prime += self.read_W_x16.last().unwrap().bin() * self.x16.last().unwrap().bin();

        W_prime += self.read_W_x17.last().unwrap().bin() * self.x17.last().unwrap().bin();
        X_prime += self.X_free_value.last().unwrap().bin();

        Y_prime += self.Y_free_value.last().unwrap().bin();

        Z_prime += self.Z_free_value.last().unwrap().bin();

        self.X.push(Elem::Binary(X_prime));
        self.Y.push(Elem::Binary(Y_prime));
        self.Z.push(Elem::Binary(Z_prime));
        self.W.push(Elem::Binary(W_prime));
    }

    fn update_writes_to_state_registers(&mut self) {
        if self.reg_write_X_tmp1.last().unwrap().is_one() {
            self.tmp1.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_tmp1.last().unwrap().is_one() {
            self.tmp1.push(*self.Y.last().unwrap());
        } else if self.reg_write_Z_tmp1.last().unwrap().is_one() {
            self.tmp1.push(*self.Z.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.tmp1.push(0.into());
        } else if self.tmp1.len() <= self.pc.len() {
            self.tmp1
                .push(self.tmp1.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_tmp2.last().unwrap().is_one() {
            self.tmp2.push(*self.X.last().unwrap());
        } else if self.reg_write_Z_tmp2.last().unwrap().is_one() {
            self.tmp2.push(*self.Z.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.tmp2.push(0.into());
        } else if self.tmp2.len() <= self.pc.len() {
            self.tmp2
                .push(self.tmp2.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_tmp3.last().unwrap().is_one() {
            self.tmp3.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.tmp3.push(0.into());
        } else if self.tmp3.len() <= self.pc.len() {
            self.tmp3
                .push(self.tmp3.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.instr__reset.last().unwrap().is_one() {
            self.tmp4.push(0.into());
        } else if self.tmp4.len() <= self.pc.len() {
            self.tmp4
                .push(self.tmp4.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.instr__reset.last().unwrap().is_one() {
            self.lr_sc_reservation.push(0.into());
        } else if self.lr_sc_reservation.len() <= self.pc.len() {
            self.lr_sc_reservation.push(
                self.lr_sc_reservation
                    .last()
                    .cloned()
                    .unwrap_or_else(|| 0.into()),
            );
        }
        if self.reg_write_X_x1.last().unwrap().is_one() {
            self.x1.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x1.last().unwrap().is_one() {
            self.x1.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x1.push(0.into());
        } else if self.x1.len() <= self.pc.len() {
            self.x1
                .push(self.x1.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x2.last().unwrap().is_one() {
            self.x2.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x2.push(0.into());
        } else if self.x2.len() <= self.pc.len() {
            self.x2
                .push(self.x2.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.instr__reset.last().unwrap().is_one() {
            self.x3.push(0.into());
        } else if self.x3.len() <= self.pc.len() {
            self.x3
                .push(self.x3.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.instr__reset.last().unwrap().is_one() {
            self.x4.push(0.into());
        } else if self.x4.len() <= self.pc.len() {
            self.x4
                .push(self.x4.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x5.last().unwrap().is_one() {
            self.x5.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x5.push(0.into());
        } else if self.x5.len() <= self.pc.len() {
            self.x5
                .push(self.x5.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x6.last().unwrap().is_one() {
            self.x6.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x6.push(0.into());
        } else if self.x6.len() <= self.pc.len() {
            self.x6
                .push(self.x6.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x7.last().unwrap().is_one() {
            self.x7.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x7.push(0.into());
        } else if self.x7.len() <= self.pc.len() {
            self.x7
                .push(self.x7.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x8.last().unwrap().is_one() {
            self.x8.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x8.push(0.into());
        } else if self.x8.len() <= self.pc.len() {
            self.x8
                .push(self.x8.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x9.last().unwrap().is_one() {
            self.x9.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x9.push(0.into());
        } else if self.x9.len() <= self.pc.len() {
            self.x9
                .push(self.x9.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x10.last().unwrap().is_one() {
            self.x10.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x10.last().unwrap().is_one() {
            self.x10.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x10.push(0.into());
        } else if self.x10.len() <= self.pc.len() {
            self.x10
                .push(self.x10.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x11.last().unwrap().is_one() {
            self.x11.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x11.push(0.into());
        } else if self.x11.len() <= self.pc.len() {
            self.x11
                .push(self.x11.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x12.last().unwrap().is_one() {
            self.x12.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x12.last().unwrap().is_one() {
            self.x12.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x12.push(0.into());
        } else if self.x12.len() <= self.pc.len() {
            self.x12
                .push(self.x12.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x13.last().unwrap().is_one() {
            self.x13.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x13.last().unwrap().is_one() {
            self.x13.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x13.push(0.into());
        } else if self.x13.len() <= self.pc.len() {
            self.x13
                .push(self.x13.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x14.last().unwrap().is_one() {
            self.x14.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x14.last().unwrap().is_one() {
            self.x14.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x14.push(0.into());
        } else if self.x14.len() <= self.pc.len() {
            self.x14
                .push(self.x14.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x15.last().unwrap().is_one() {
            self.x15.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x15.push(0.into());
        } else if self.x15.len() <= self.pc.len() {
            self.x15
                .push(self.x15.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x16.last().unwrap().is_one() {
            self.x16.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x16.push(0.into());
        } else if self.x16.len() <= self.pc.len() {
            self.x16
                .push(self.x16.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x17.last().unwrap().is_one() {
            self.x17.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x17.push(0.into());
        } else if self.x17.len() <= self.pc.len() {
            self.x17
                .push(self.x17.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x18.last().unwrap().is_one() {
            self.x18.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x18.push(0.into());
        } else if self.x18.len() <= self.pc.len() {
            self.x18
                .push(self.x18.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x19.last().unwrap().is_one() {
            self.x19.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x19.push(0.into());
        } else if self.x19.len() <= self.pc.len() {
            self.x19
                .push(self.x19.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x20.last().unwrap().is_one() {
            self.x20.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x20.push(0.into());
        } else if self.x20.len() <= self.pc.len() {
            self.x20
                .push(self.x20.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x21.last().unwrap().is_one() {
            self.x21.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x21.push(0.into());
        } else if self.x21.len() <= self.pc.len() {
            self.x21
                .push(self.x21.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x22.last().unwrap().is_one() {
            self.x22.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x22.push(0.into());
        } else if self.x22.len() <= self.pc.len() {
            self.x22
                .push(self.x22.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x23.last().unwrap().is_one() {
            self.x23.push(*self.X.last().unwrap());
        } else if self.reg_write_Y_x23.last().unwrap().is_one() {
            self.x23.push(*self.Y.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x23.push(0.into());
        } else if self.x23.len() <= self.pc.len() {
            self.x23
                .push(self.x23.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x24.last().unwrap().is_one() {
            self.x24.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x24.push(0.into());
        } else if self.x24.len() <= self.pc.len() {
            self.x24
                .push(self.x24.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x25.last().unwrap().is_one() {
            self.x25.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x25.push(0.into());
        } else if self.x25.len() <= self.pc.len() {
            self.x25
                .push(self.x25.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x26.last().unwrap().is_one() {
            self.x26.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x26.push(0.into());
        } else if self.x26.len() <= self.pc.len() {
            self.x26
                .push(self.x26.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x27.last().unwrap().is_one() {
            self.x27.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x27.push(0.into());
        } else if self.x27.len() <= self.pc.len() {
            self.x27
                .push(self.x27.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x28.last().unwrap().is_one() {
            self.x28.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x28.push(0.into());
        } else if self.x28.len() <= self.pc.len() {
            self.x28
                .push(self.x28.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x29.last().unwrap().is_one() {
            self.x29.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x29.push(0.into());
        } else if self.x29.len() <= self.pc.len() {
            self.x29
                .push(self.x29.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x30.last().unwrap().is_one() {
            self.x30.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x30.push(0.into());
        } else if self.x30.len() <= self.pc.len() {
            self.x30
                .push(self.x30.last().cloned().unwrap_or_else(|| 0.into()));
        }
        if self.reg_write_X_x31.last().unwrap().is_one() {
            self.x31.push(*self.X.last().unwrap());
        } else if self.instr__reset.last().unwrap().is_one() {
            self.x31.push(0.into());
        } else if self.x31.len() <= self.pc.len() {
            self.x31
                .push(self.x31.last().cloned().unwrap_or_else(|| 0.into()));
        }
    }

    #[allow(non_snake_case)]
    fn update_pc(&mut self) {
        let pc = self.pc.last().unwrap();
        if self.instr__jump_to_operation.last().unwrap().is_one() {
            self.pc.push(*self._operation_id.last().unwrap());
        } else if self.instr__loop.last().unwrap().is_one() {
            self.pc.push(*pc);
        } else if self.instr_return.last().unwrap().is_one() {
            self.pc.push(0.into());
        } else if self.current_row + 1 == self.pc.len() {
            self.pc.push(Elem::Binary(pc.bin() + 1));
        };
    }

    fn update_control_flow_flags(&mut self) {
        // TODO: automate
        if self.current_row == 0 {
            self._operation_id.push(2.into());
        } else if self.instr_return.last().unwrap().is_one() {
            // TODO: read the number from _operation_id hint
            self._operation_id.push(2191.into());
        } else {
            self._operation_id.push(*self._operation_id.last().unwrap());
        }
    }

    fn update_flags(&mut self) {
        let pc = self.pc.last().unwrap().bin();

        self.reg_write_X_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_tmp1").unwrap()[pc as usize],
        ));
        self.reg_write_Y_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_tmp1").unwrap()[pc as usize],
        ));
        self.reg_write_Z_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Z_tmp1").unwrap()[pc as usize],
        ));
        self.reg_write_X_tmp2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_tmp2").unwrap()[pc as usize],
        ));
        self.reg_write_Z_tmp2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Z_tmp2").unwrap()[pc as usize],
        ));
        self.reg_write_X_tmp3.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_tmp3").unwrap()[pc as usize],
        ));
        self.reg_write_X_x0.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x0").unwrap()[pc as usize],
        ));
        self.reg_write_X_x1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x1").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x1").unwrap()[pc as usize],
        ));
        self.reg_write_X_x2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x2").unwrap()[pc as usize],
        ));
        self.reg_write_X_x5.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x5").unwrap()[pc as usize],
        ));
        self.reg_write_X_x6.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x6").unwrap()[pc as usize],
        ));
        self.reg_write_X_x7.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x7").unwrap()[pc as usize],
        ));
        self.reg_write_X_x8.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x8").unwrap()[pc as usize],
        ));
        self.reg_write_X_x9.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x9").unwrap()[pc as usize],
        ));
        self.reg_write_X_x10.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x10").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x10.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x10").unwrap()[pc as usize],
        ));
        self.reg_write_X_x11.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x11").unwrap()[pc as usize],
        ));
        self.reg_write_X_x12.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x12").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x12.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x12").unwrap()[pc as usize],
        ));
        self.reg_write_X_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x13").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x13").unwrap()[pc as usize],
        ));
        self.reg_write_X_x14.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x14").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x14.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x14").unwrap()[pc as usize],
        ));
        self.reg_write_X_x15.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x15").unwrap()[pc as usize],
        ));
        self.reg_write_X_x16.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x16").unwrap()[pc as usize],
        ));
        self.reg_write_X_x17.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x17").unwrap()[pc as usize],
        ));
        self.reg_write_X_x18.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x18").unwrap()[pc as usize],
        ));
        self.reg_write_X_x19.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x19").unwrap()[pc as usize],
        ));
        self.reg_write_X_x20.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x20").unwrap()[pc as usize],
        ));
        self.reg_write_X_x21.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x21").unwrap()[pc as usize],
        ));
        self.reg_write_X_x22.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x22").unwrap()[pc as usize],
        ));
        self.reg_write_X_x23.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x23").unwrap()[pc as usize],
        ));
        self.reg_write_Y_x23.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_Y_x23").unwrap()[pc as usize],
        ));
        self.reg_write_X_x24.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x24").unwrap()[pc as usize],
        ));
        self.reg_write_X_x25.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x25").unwrap()[pc as usize],
        ));
        self.reg_write_X_x26.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x26").unwrap()[pc as usize],
        ));
        self.reg_write_X_x27.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x27").unwrap()[pc as usize],
        ));
        self.reg_write_X_x28.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x28").unwrap()[pc as usize],
        ));
        self.reg_write_X_x29.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x29").unwrap()[pc as usize],
        ));
        self.reg_write_X_x30.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x30").unwrap()[pc as usize],
        ));
        self.reg_write_X_x31.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_reg_write_X_x31").unwrap()[pc as usize],
        ));
        self.instr_mload.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_mload").unwrap()[pc as usize],
        ));
        self.instr_mstore.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_mstore").unwrap()[pc as usize],
        ));
        self.instr_load_label.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_load_label").unwrap()[pc as usize],
        ));
        self.instr_load_label_param_l.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_load_label_param_l").unwrap()[pc as usize],
        ));
        self.instr_jump.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_jump").unwrap()[pc as usize],
        ));
        self.instr_jump_param_l.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_jump_param_l").unwrap()[pc as usize],
        ));
        self.instr_jump_dyn.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_jump_dyn").unwrap()[pc as usize],
        ));
        self.instr_branch_if_nonzero.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_branch_if_nonzero").unwrap()[pc as usize],
        ));
        self.instr_branch_if_nonzero_param_l
            .push(Elem::new_from_fe_as_bin(
                &self
                    .fixed
                    .get("main.p_instr_branch_if_nonzero_param_l")
                    .unwrap()[pc as usize],
            ));
        self.instr_branch_if_zero.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_branch_if_zero").unwrap()[pc as usize],
        ));
        self.instr_branch_if_zero_param_l
            .push(Elem::new_from_fe_as_bin(
                &self
                    .fixed
                    .get("main.p_instr_branch_if_zero_param_l")
                    .unwrap()[pc as usize],
            ));
        self.instr_branch_if_positive.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_branch_if_positive").unwrap()[pc as usize],
        ));
        self.instr_branch_if_positive_param_l
            .push(Elem::new_from_fe_as_bin(
                &self
                    .fixed
                    .get("main.p_instr_branch_if_positive_param_l")
                    .unwrap()[pc as usize],
            ));
        self.instr_is_positive.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_is_positive").unwrap()[pc as usize],
        ));
        self.instr_and.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_and").unwrap()[pc as usize],
        ));
        self.instr_or.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_or").unwrap()[pc as usize],
        ));
        self.instr_xor.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_xor").unwrap()[pc as usize],
        ));
        self.instr_shl.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_shl").unwrap()[pc as usize],
        ));
        self.instr_shr.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_shr").unwrap()[pc as usize],
        ));
        self.instr_wrap.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_wrap").unwrap()[pc as usize],
        ));
        self.instr_wrap_signed.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_wrap_signed").unwrap()[pc as usize],
        ));
        self.instr_sign_extend_byte.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_sign_extend_byte").unwrap()[pc as usize],
        ));
        self.instr_to_signed.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_to_signed").unwrap()[pc as usize],
        ));
        self.instr_wrap16.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_wrap16").unwrap()[pc as usize],
        ));
        self.instr_mul.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_mul").unwrap()[pc as usize],
        ));
        self.instr__jump_to_operation.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr__jump_to_operation").unwrap()[pc as usize],
        ));
        self.instr__reset.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr__reset").unwrap()[pc as usize],
        ));
        self.instr__loop.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr__loop").unwrap()[pc as usize],
        ));
        self.instr_return.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_instr_return").unwrap()[pc as usize],
        ));
        self.read_W_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x13").unwrap()[pc as usize],
        ));
        self.read_W_x16.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x16").unwrap()[pc as usize],
        ));
        self.read_W_x17.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x17").unwrap()[pc as usize],
        ));
        self.read_W_x5.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x5").unwrap()[pc as usize],
        ));
        self.read_W_x6.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x6").unwrap()[pc as usize],
        ));
        self.read_W_x7.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_W_x7").unwrap()[pc as usize],
        ));
        self.X_const.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_X_const").unwrap()[pc as usize],
        ));
        self.X_read_free.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_X_read_free").unwrap()[pc as usize],
        ));
        self.read_X_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_tmp1").unwrap()[pc as usize],
        ));
        self.read_X_tmp2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_tmp2").unwrap()[pc as usize],
        ));
        self.read_X_x0.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x0").unwrap()[pc as usize],
        ));
        self.read_X_x1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x1").unwrap()[pc as usize],
        ));
        self.read_X_x10.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x10").unwrap()[pc as usize],
        ));
        self.read_X_x11.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x11").unwrap()[pc as usize],
        ));
        self.read_X_x12.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x12").unwrap()[pc as usize],
        ));
        self.read_X_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x13").unwrap()[pc as usize],
        ));
        self.read_X_x14.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x14").unwrap()[pc as usize],
        ));
        self.read_X_x15.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x15").unwrap()[pc as usize],
        ));
        self.read_X_x16.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x16").unwrap()[pc as usize],
        ));
        self.read_X_x17.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x17").unwrap()[pc as usize],
        ));
        self.read_X_x18.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x18").unwrap()[pc as usize],
        ));
        self.read_X_x19.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x19").unwrap()[pc as usize],
        ));
        self.read_X_x2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x2").unwrap()[pc as usize],
        ));
        self.read_X_x20.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x20").unwrap()[pc as usize],
        ));
        self.read_X_x21.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x21").unwrap()[pc as usize],
        ));
        self.read_X_x22.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x22").unwrap()[pc as usize],
        ));
        self.read_X_x23.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x23").unwrap()[pc as usize],
        ));
        self.read_X_x24.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x24").unwrap()[pc as usize],
        ));
        self.read_X_x25.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x25").unwrap()[pc as usize],
        ));
        self.read_X_x26.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x26").unwrap()[pc as usize],
        ));
        self.read_X_x27.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x27").unwrap()[pc as usize],
        ));
        self.read_X_x28.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x28").unwrap()[pc as usize],
        ));
        self.read_X_x29.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x29").unwrap()[pc as usize],
        ));
        self.read_X_x31.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x31").unwrap()[pc as usize],
        ));
        self.read_X_x5.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x5").unwrap()[pc as usize],
        ));
        self.read_X_x6.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x6").unwrap()[pc as usize],
        ));
        self.read_X_x7.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x7").unwrap()[pc as usize],
        ));
        self.read_X_x8.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x8").unwrap()[pc as usize],
        ));
        self.read_X_x9.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_X_x9").unwrap()[pc as usize],
        ));
        self.Y_const.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_Y_const").unwrap()[pc as usize],
        ));
        self.Y_read_free.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_Y_read_free").unwrap()[pc as usize],
        ));
        self.read_Y_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_tmp1").unwrap()[pc as usize],
        ));
        self.read_Y_tmp2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_tmp2").unwrap()[pc as usize],
        ));
        self.read_Y_tmp3.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_tmp3").unwrap()[pc as usize],
        ));
        self.read_Y_x10.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x10").unwrap()[pc as usize],
        ));
        self.read_Y_x11.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x11").unwrap()[pc as usize],
        ));
        self.read_Y_x12.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x12").unwrap()[pc as usize],
        ));
        self.read_Y_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x13").unwrap()[pc as usize],
        ));
        self.read_Y_x14.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x14").unwrap()[pc as usize],
        ));
        self.read_Y_x15.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x15").unwrap()[pc as usize],
        ));
        self.read_Y_x16.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x16").unwrap()[pc as usize],
        ));
        self.read_Y_x18.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x18").unwrap()[pc as usize],
        ));
        self.read_Y_x19.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x19").unwrap()[pc as usize],
        ));
        self.read_Y_x2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x2").unwrap()[pc as usize],
        ));
        self.read_Y_x20.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x20").unwrap()[pc as usize],
        ));
        self.read_Y_x21.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x21").unwrap()[pc as usize],
        ));
        self.read_Y_x22.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x22").unwrap()[pc as usize],
        ));
        self.read_Y_x23.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x23").unwrap()[pc as usize],
        ));
        self.read_Y_x24.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x24").unwrap()[pc as usize],
        ));
        self.read_Y_x25.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x25").unwrap()[pc as usize],
        ));
        self.read_Y_x26.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x26").unwrap()[pc as usize],
        ));
        self.read_Y_x28.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x28").unwrap()[pc as usize],
        ));
        self.read_Y_x29.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x29").unwrap()[pc as usize],
        ));
        self.read_Y_x30.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x30").unwrap()[pc as usize],
        ));
        self.read_Y_x31.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x31").unwrap()[pc as usize],
        ));
        self.read_Y_x5.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x5").unwrap()[pc as usize],
        ));
        self.read_Y_x6.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x6").unwrap()[pc as usize],
        ));
        self.read_Y_x7.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x7").unwrap()[pc as usize],
        ));
        self.read_Y_x8.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x8").unwrap()[pc as usize],
        ));
        self.read_Y_x9.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Y_x9").unwrap()[pc as usize],
        ));
        self.Z_const.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_Z_const").unwrap()[pc as usize],
        ));
        self.Z_read_free.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_Z_read_free").unwrap()[pc as usize],
        ));
        self.read_Z_tmp1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_tmp1").unwrap()[pc as usize],
        ));
        self.read_Z_tmp2.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_tmp2").unwrap()[pc as usize],
        ));
        self.read_Z_tmp3.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_tmp3").unwrap()[pc as usize],
        ));
        self.read_Z_x0.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x0").unwrap()[pc as usize],
        ));
        self.read_Z_x1.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x1").unwrap()[pc as usize],
        ));
        self.read_Z_x10.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x10").unwrap()[pc as usize],
        ));
        self.read_Z_x11.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x11").unwrap()[pc as usize],
        ));
        self.read_Z_x12.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x12").unwrap()[pc as usize],
        ));
        self.read_Z_x13.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x13").unwrap()[pc as usize],
        ));
        self.read_Z_x14.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x14").unwrap()[pc as usize],
        ));
        self.read_Z_x15.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x15").unwrap()[pc as usize],
        ));
        self.read_Z_x17.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x17").unwrap()[pc as usize],
        ));
        self.read_Z_x18.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x18").unwrap()[pc as usize],
        ));
        self.read_Z_x19.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x19").unwrap()[pc as usize],
        ));
        self.read_Z_x20.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x20").unwrap()[pc as usize],
        ));
        self.read_Z_x21.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x21").unwrap()[pc as usize],
        ));
        self.read_Z_x22.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x22").unwrap()[pc as usize],
        ));
        self.read_Z_x23.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x23").unwrap()[pc as usize],
        ));
        self.read_Z_x24.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x24").unwrap()[pc as usize],
        ));
        self.read_Z_x25.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x25").unwrap()[pc as usize],
        ));
        self.read_Z_x26.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x26").unwrap()[pc as usize],
        ));
        self.read_Z_x27.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x27").unwrap()[pc as usize],
        ));
        self.read_Z_x30.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x30").unwrap()[pc as usize],
        ));
        self.read_Z_x8.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x8").unwrap()[pc as usize],
        ));
        self.read_Z_x9.push(Elem::new_from_fe_as_bin(
            &self.fixed.get("main.p_read_Z_x9").unwrap()[pc as usize],
        ));
    }
}
