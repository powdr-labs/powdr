// Verfies that a sum in the input has been computed properly.
// Input: sum, cnt, x_1, x_2, ..., x_cnt
// This input is assumed to be present in a minirust variable called "input"
// of type "Vec<FieldElement>"

// Code in `${`...`}` is rust-like code that is run by the prover
// to generate free inputs.

reg X[<=]; // "<=" means it is the default assignment register.
reg A;
reg CNT;
reg pc[@pc]; // "@pc" means "pc' = pc + 1" is the default propagation (instead of pc' = pc) and it tracks the line in the program.

// Code in `pil{`..`}` is pil code that is inserted into the pil file.
pil{
    col witness XInv;
    col XIsZero = 1 - X * XInv;
    XIsZero * X = 0;
}

instr jmpz <=X= c, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }
instr jmp l: label { pc' = l }
instr dec_CNT { CNT' = CNT - 1 }
instr assert_zero <=X= a { XIsZero = 1 }

CNT <=X= ${ input[1] };

start::
 jmpz CNT, end;
 A <=X= A + ${ input[CNT + 1] };
 // Could use "CNT <=X= CNT - 1", but that would need X.
 dec_CNT;
 jmp start;

end::
 A <=X= A - ${ input[0] };
 assert_zero A;

 
/// -------------------------- compiled into the following pil file -------------------------------

// // ===== Register definitions
// col witness A;
// col witness CNT;
// col witness pc;
// col witness X;
// 
// // ===== Inline PIL
// col witness XInv;
// col XIsZero = 1 - X * XInv;
// XIsZero * X = 0;
// 
// // ===== Encoding of the instructions
// 
// // New powdr feature, creates a bit field type.
// // This is a bit field and not an enum so that multiple instructions
// // can be combined in a single line.
// BitField Instr {
//     jmpz
//     jmp
//     dec_CNT
//     assert_zero
// }
// 
// col witness instr: Instr;
// // The above automatically generates the following commit polys:
// // col witness instr_jmp_set: bool;
// // col witness instr_jmpz_set: bool;
// // col witness instr_jmp_set: bool;
// // col witness instr_dec_CNT_set: bool;
// // col witness instr_assert_zero_set: bool;
// // and the following constraint:
// // instr = 1 * 
// //   1 * instr_jmp_set +
// //   2 * instr_jmpz_set +
// //   4 * instr_jmp_set +
// //   8 * instr_dec_CNT_set +
// //   16 * instr_assert_zero_set;
// // The expression `instr == Instr::jmpz` is replaced by the flag.
// 
// col witness instr_jmp_arg0;
// 
// // ===== Register propagation
// 
// // There is only a single write poly per register (not one per register/assignment register combination)
// // because we do not want to write from multiple assignment registers into the same register.
// col witness write_A: bool;
// col witness read_X_A;
// col witness write_CNT: bool;
// col witness const_X;
// 
// A' = write_A * X + (1 - write_A) * A;
// // The compiler ensures that write_CNT and dec_cnt cannot both be set at the same time.
// CNT' = write_CNT * X + instr_dec_cnt_set * (CNT - 1) + (1 - write_CNT - instr_dec_cnt_set) * CNT;
// X = read_X_A * A + const_X;
// 
// // The match expression is replaced by the usual if-then-else construction.
// // Note that multiple arms can match. The result is the sum.
// // Maybe then match is not the right construct?
// pc' = match instr {
//     Instr::jmpz => (XIsZero * l + (1 - XIsZero) * (pc + 1)),
//     Instr::jmp => instr_jmp_arg0,
//     _ => pc + 1
// };
// 
// 
// // ===== Constraints from instructions
// 
// instr_assert_zero_set * (XIsZero - 1) = 0;
// 
// // ===== Fixed columns representing the program
// // Their contents are generated from the program, but not explicitly expressed in the PIL language.
// 
// col fixed line;
// col fixed p_instr;
// // The compiler can actually combine multiple boolean columns into one.
// col fixed p_write_A;
// col fixed p_read_X_A;
// col fixed p_write_CNT;
// col fixed p_const_X;
// col fixed p_instr_jmp_arg0;
// 
// // ===== Lookups connecting the execution to the program
// 
// { pc, instr, write_A, read_X_A, write_CNT, const_x, instr_jmp_arg0 }
//   in
// { line, p_instr, p_write_A, p_read_X_A, p_write_CNT, p_const_x, p_instr_jmp_arg0};
// 
// // TODO What is missing here is the termination of the program and making all polynomials cyclic.