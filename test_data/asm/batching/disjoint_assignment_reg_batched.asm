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
instr jump l: label {(pc' - l)}
instr loop {(pc' - pc)}
instr mstore X {(addr - X)}
instr wrap Z -> Y {(Z - Y)}

same_reg_same_assignment_reg::
A <=X= ${ ("input", 0) };
// END BATCH BusyWriteRegister, BusyAssignmentRegister
A <=X= ${ ("input", 1) };
// END BATCH Label

same_reg_different_assignment_reg::
A <=X= ${ ("input", 0) };
// END BATCH BusyWriteRegister
A <=Y= ${ ("input", 1) };
// END BATCH Label

different_reg_same_assignment_reg::
A <=X= ${ ("input", 0) };
// END BATCH BusyAssignmentRegister
B <=X= ${ ("input", 1) };
// END BATCH Label

different_reg_different_assignment_reg::
A <=X= ${ ("input", 0) };
B <=Y= ${ ("input", 1) };
// END BATCH Label

label_with_next::
A <=X= ${ ("input", 2) };
// END BATCH Label

read_pc::
A <=X= pc;
B <=Y= 2;
// END BATCH Label

write_pc::
jump write_pc;
// END BATCH Jump
A <=Y= 3;
// END BATCH Label

jump_last::
A <=X= 3;
jump jump_last;
// END BATCH Jump
B <=Z= 3;
// END BATCH Label

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
addr <=Y= wrap(tmp1);
// END BATCH ReadAfterWrite
mstore tmp1;
addr <=Y= wrap(tmp1);
// END BATCH BusyAssignmentRegister, ReadAfterWrite
mstore tmp1;
// END BATCH Label

end::
loop;
// END BATCH
