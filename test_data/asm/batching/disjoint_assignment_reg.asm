degree 256;

reg pc[@pc];
reg X[<=];
reg Y[<=];
reg Z[<=];
reg A;
reg B;
reg addr;
reg tmp1;
reg tmp2;

instr jump l: label { pc' = l }
instr loop { pc' = pc }
instr mstore X { addr = X }
instr wrap Z -> Y { Z = Y }

// same registers, same assignment registers
same_reg_same_assignment_reg::
 A <=X= ${ ("input", 0) };
 A <=X= ${ ("input", 1) };

// same registers, different assignment registers
same_reg_different_assignment_reg::
 A <=X= ${ ("input", 0) };
 A <=Y= ${ ("input", 1) };

// different registers, same assignment registers
different_reg_same_assignment_reg::
 A <=X= ${ ("input", 0) };
 B <=X= ${ ("input", 1) };

// different registers, different assignment registers
different_reg_different_assignment_reg::
 A <=X= ${ ("input", 0) };
 B <=Y= ${ ("input", 1) };

// a label can be combined with the next statement
label_with_next:: // batch 1
 A <=X= ${ ("input", 2) };

// an assignment which uses pc can be combined
read_pc::
 A <=X= pc;
 B <=Y= 2;

// an assignment to pc cannot be combined
write_pc::
 jump write_pc;
 A <=Y= 3;

// we can apply a single jump per batch, in the last statement
jump_last::
 A <=X= 3;
 jump jump_last;
 B <=Z= 3;

batch_constants::
addr <=Y= 2;
// END BATCH ReadAfterWrite
mstore 2;
addr <=Y= 3;
// END BATCH BusyAssignmentRegister, ReadAfterWrite
mstore 3;
// END BATCH Label

batch_registers::
addr <=Y= tmp1;
// END BATCH ReadAfterWrite
mstore tmp1;
addr <=Y= tmp2;
// END BATCH BusyAssignmentRegister, ReadAfterWrite
mstore tmp2;
// END BATCH Label

batch_with_calls::
addr <=Y= wrap(tmp1); // Y -> X -> addr
// END BATCH ReadAfterWrite
mstore tmp1; // tmp1 -> X -> ()
addr <=Y= wrap(tmp1); // Y -> X -> addr
// END BATCH BusyAssignmentRegister, ReadAfterWrite
mstore tmp1; // tmp1 -> X -> ()
// END BATCH Label

end::
 loop;