use powdr_analysis::analyze;
use powdr_asmopt::optimize;
//use powdr_importer::load_dependencies_and_resolve_str;
use powdr_parser::parse_asm;

#[test]
fn remove_unused_machine() {
    let input = r#"
    machine Main with degree: 8 {
        reg pc[@pc];
        reg X[<=];
        reg A;

        instr assert_eq X, A { X = A }

        function main {
            assert_eq 1, 1;
            return;
        }
    }

    // This machine should be removed since it's never used
    machine Unused with degree: 8 {
        reg pc[@pc];
        col witness w;
        w = w * w;
    }
    "#;

    let expectation = r#"machine Main with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg A;
    instr assert_eq X, A{ X = A }
    function main {
        assert_eq 1, 1;
        // END BATCH Unimplemented
        return;
        // END BATCH
    }
}
"#;

    let parsed = parse_asm(None, input).unwrap();
    let analyzed = analyze(parsed).unwrap();
    let optimized = optimize(analyzed).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn remove_unused_instruction_and_machine() {
    let input = r#"
    machine Main with degree: 8 {
        Helper helper;
        
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg A;

        // This instruction is never used and should be removed
        // which will also remove Helper machine since it's the only usage
        instr unused X -> Y link ~> Z = helper.double(X);
        instr assert_eq X, A { X = A }

        function main {
            assert_eq 1, 1;
            return;
        }
    }

    machine Helper with degree: 8 {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];

        function double x: field -> field {
            return x + x;
        }
    }
    "#;

    let expectation = r#"machine Main with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    instr assert_eq X, A{ X = A }
    function main {
        assert_eq 1, 1;
        // END BATCH Unimplemented
        return;
        // END BATCH
    }
}
"#;

    let parsed = parse_asm(None, input).unwrap();
    let analyzed = analyze(parsed).unwrap();
    let optimized = optimize(analyzed).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn keep_machine_with_multiple_references() {
    let input = r#"
    machine Main with degree: 8 {
        Helper helper;
        
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg A;

        // Two different instructions using the same machine
        instr double X -> Y link => Y = helper.double(X);
        instr triple X -> Y link => Y = helper.triple(X);

        function main {
            // Only using one instruction
            A <== double(2);
            return;
        }
    }

    machine Helper with degree: 8 {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];

        function double x: field -> field { return x + x; }
        function triple x: field -> field { return x + x + x; }
    }
    "#;

    let expectation = r#"machine Main with degree: 8 {
    ::Helper helper
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    instr double X -> Y link => Y = helper.double(X){  }
    function main {
        A <=Y= double(2);
        // END BATCH Unimplemented
        return;
        // END BATCH
    }
}
machine Helper with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    function double x: field -> field {
        return x + x;
        // END BATCH
    }
    function triple x: field -> field {
        return x + x + x;
        // END BATCH
    }
}
"#;

    let parsed = parse_asm(None, input).unwrap();
    let analyzed = analyze(parsed).unwrap();
    let optimized = optimize(analyzed).to_string();
    assert_eq!(optimized, expectation);
}

#[test]
fn keep_machine_parameters() {
    let input = r#"
    machine Main with degree: 8 {
        Required required;
        ParamMachine sub(required);
        Unused unused;
        
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];
        reg A;

        instr compute X -> Y link => Y = sub.compute(X);

        function main {
            A <== compute(1);
            return;
        }
    }

    machine ParamMachine(mem: Required) with degree: 8 {
        reg pc[@pc];
        reg X[<=];
        reg Y[<=];

        function compute x: field -> field {
            return x + x;
        }
    }

    machine Required with
        latch: latch,
        operation_id: operation_id
    {
        operation compute<0> x -> y;

        col fixed latch = [1]*;
        col witness operation_id;
        col witness x;
        col witness y;
        
        y = x + x;
    }

    machine Unused with degree: 8 {
        reg pc[@pc];
        col witness w;
        w = w * w;
    }
    "#;

    let expectation = r#"machine Main with degree: 8 {
    ::ParamMachine sub
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg A;
    instr compute X -> Y link => Y = sub.compute(X){  }
    function main {
        A <=Y= compute(1);
        // END BATCH Unimplemented
        return;
        // END BATCH
    }
}
machine ParamMachine with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    function compute x: field -> field {
        return x + x;
        // END BATCH
    }
}
machine Required with , latch: latch, operation_id: operation_id {
    operation compute<0> x -> y;
    pol constant latch = [1]*;
    pol commit operation_id;
    pol commit x;
    pol commit y;
    y = x + x;
}
"#;

    let parsed = parse_asm(None, input).unwrap();
    let analyzed = analyze(parsed).unwrap();
    let optimized = optimize(analyzed).to_string();
    assert_eq!(optimized, expectation);
}
