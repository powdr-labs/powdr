degree 256;
reg pc[@pc];
reg X[<=];
reg A;

instr loop { pc' = pc }

label_with_next::
 A <=X= 1;

labels_with_next::
other_label_just_after::
 A <=X= 2;
 A <=X= 2;

end::
loop;