machine Main with degree: 1024 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    b::Arith arith;
    a::WithArg subm(arith);

    instr add X, Y -> Z = subm.add;
    instr sub X, Y -> Z = subm.sub;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== add(1,3);
        assert_eq A, 4;
        A <== add(2,5);
        assert_eq A, 7;

        A <== sub(3,1);
        assert_eq A, 2;
        A <== sub(5,2);
        assert_eq A, 3;

        return;
    }
}

// check that relative paths work in machine parameters
mod a {
    machine WithArg(arith: super::b::Arith) {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg Z[<=];
        reg A;

        instr add X, Y -> Z = arith.add;
        instr sub X, Y -> Z = arith.sub;

        function add a, b -> c {
            A <== add(a,b);
            return A;
        }

        function sub a, b -> c {
            A <== sub(a,b);
            return A;
        }
    }
}

mod b {
    machine Arith {
        reg pc[@pc];
        reg X[<=];
        reg A;

        function add a, b -> c {
           A <=X= a + b;
           return A;
        }

        function sub a, b -> c {
           A <=X= a - b;
           return A;
        }
    }
}
