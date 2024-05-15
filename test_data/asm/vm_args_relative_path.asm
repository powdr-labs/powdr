machine Main with degree: 262144 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A;

    b::Binary binary;
    a::WithArg sub(binary);

    instr and X, Y -> Z = sub.and;

    instr assert_eq X, Y { X = Y }

    function main {
        A <== and(0xffffffff, 0xabcdef01);
        assert_eq A, 0xabcdef01;
        A <== and(0xabcdef01, 0xffffffff);
        assert_eq A, 0xabcdef01;

        return;
    }
}

// check that relative paths work in machine parameters
mod a {
    machine WithArg(bin: super::b::Binary) {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg Z[<=];
        reg A;

        instr and X, Y -> Z ~ bin.and;

        function and a, b -> c {
            A <== and(a,b);
            return A;
        }
    }
}

mod b {
    use std::machines::binary::Binary;
}
