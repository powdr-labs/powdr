degree 256;
reg pc[@pc];
reg X[<=];
reg Y[<=];
reg C;
reg D;

pil{
pol constant FIRST = [1] + [0]*;
(FIRST * C) = 2;
(FIRST * D) = 2;
(C * D) = 4;
}

pil{
pol commit ones;
(FIRST * ones) = 1;
ones' = ones;
}

instr loop {(pc' - pc)}
instr is_one_using_X X {{ X } in { ones }}
instr is_one_using_Y Y {{ Y } in { ones }}

connected_by_inline_pil::
C <=X= 1;
// END BATCH BusyWriteRegister
D <=Y= 4;
// END BATCH Label

parallel_coprocessor_access::
is_one_using_X 1;
is_one_using_Y 1;
// END BATCH Label

end::
loop;
// END BATCH
