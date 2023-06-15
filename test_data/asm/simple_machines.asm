// a machine to add numbers between 0 and 1
machine Adder {
    instr add A, B -> C {}

    constraints {
        col fixed A = [0, 0, 1, 1] + [0]*;
        col fixed B = [0, 1, 0, 1] + [0]*;
        col fixed C = [0, 1, 1, 2] + [0]*;
    }
}


machine Main {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;
    reg B;
    
    Adder adder;
    Adder other_adder;

    instr assert_two X { X = 2 }
    instr add X, Y -> Z = adder.add
    instr other_add Y, X -> Z = other_adder.add

    instr loop { pc' = pc }

    program {
        A <=X= 0;
        B <=Z= add(A, 1);
        B <=Z= other_add(B, 1);
        assert_two(B);
        loop;
    }
}
