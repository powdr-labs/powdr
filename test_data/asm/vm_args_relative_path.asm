
let N: int = 32;

machine Main with degree: N {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    b::Arith arith;
    a::WithArg subm(arith);

    instr add X, Y -> Z link => Z = subm.add(X, Y);
    instr sub X, Y -> Z link => Z = subm.sub(X, Y);

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
    machine WithArg(arith: super::b::Arith) with degree: super::N {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg Z[<=];
        reg A;

        instr add X, Y -> Z link => Z = arith.add(X, Y);
        instr sub X, Y -> Z link => Z = arith.sub(X, Y);

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
    machine Arith with degree: super::N {
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
