degree 256;
reg pc[@pc];
reg X[<=];
reg Y[<=];

instr loop { pc' = pc }

// if we're using inline pil, any column referenced both there and in an expression needs to be added to the footprint (recursively)
connected_by_inline_pil::
// C and D are linked in inline PIL
reg C;
reg D;
pil {
 col fixed FIRST = [1] + [0]*;
 FIRST * C = 2;
 FIRST * D = 2;
 C*D = 4;
}
// we change C
C <=X= 1;
// C*D should break here, which is why we cannot batch with the next
// we change D
D <=Y= 4;
// C*D is verified again here

pil {
    col witness ones;
    FIRST * ones = 1;
    ones' = ones;
}
// two instructions which access the same coprocessor column with different assignment registers
instr is_one_using_X X { { X } in ones }
instr is_one_using_Y Y { { Y } in ones }

parallel_coprocessor_access::
 is_one_using_X 1;
 is_one_using_Y 1;

end::
 loop;