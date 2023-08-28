machine MacroAsm {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;

    constraints {
        macro branch_if(condition, target) {
            pc' = condition * target + (1 - condition) * (pc + 1);
        };

        col witness XInv;
        col witness XIsZero;
        XIsZero  = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;
    }

    instr bz X, target: label { branch_if(XIsZero, target) }
    instr fail { X = X + 1 }
    instr assert_zero X { XIsZero = 1 }

    function main {
        A <=X= 0;
        bz A, is_zero;
        fail;
      is_zero::
        assert_zero A;
        return;
    }
}