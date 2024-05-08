#[cfg(test)]
mod test {
    use powdr_parser::parse;
    use powdr_parser_util::UnwrapErrToStderr;
    use pretty_assertions::assert_eq;
    use test_log::test;

    type TestCase = (&'static str, &'static str);

    fn test_paren(test_case: &TestCase) {
        let (input, expected) = test_case;
        let printed = format!("{}", parse(None, input).unwrap_err_to_stderr());
        assert_eq!(expected.trim(), printed.trim());
    }

    #[test]
    fn test_binary_op_parentheses() {
        let test_cases: Vec<TestCase> = vec![
            // Complete line
            ("let t = ((x + y) * z);", "let t = (x + y) * z;"),
            // Don't add extra
            ("-x + y * !z;", "-x + y * !z;"),
            ("x = (y <= z);", "x = (y <= z);"),
            ("(x = y) <= z;", "(x = y) <= z;"),
            ("x + y + z;", "x + y + z;"),
            ("x * y * z;", "x * y * z;"),
            ("x / y / z;", "x / y / z;"),
            // Remove unneeded
            ("(-x) + y * (!z);", "-x + y * !z;"),
            ("(x * y) * z;", "x * y * z;"),
            ("(x / y) / z;", "x / y / z;"),
            ("(x ** (y ** z));", "x ** y ** z;"),
            ("(x - (y + z));", "x - (y + z);"),
            // Observe associativity
            ("x * (y * z);", "x * (y * z);"),
            ("x / (y / z);", "x / (y / z);"),
            ("x ** (y ** z);", "x ** y ** z;"),
            ("(x ** y) ** z;", "(x ** y) ** z;"),
            // Don't remove needed
            ("(x + y) * z;", "(x + y) * z;"),
            ("((x + y) * z);", "(x + y) * z;"),
            ("-(x + y);", "-(x + y);"),
            // function call
            ("(a + b)(2);", "(a + b)(2);"),
            // // Index access
            ("(a + b)[2];", "(a + b)[2];"),
            ("(i < 7) && (6 >= -i);", "i < 7 && 6 >= -i;"),
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }

    #[test]
    fn test_lambda_ex_parentheses() {
        let test_cases: Vec<TestCase> = vec![
            ("let x = 1 + (|i| i + 2);", "let x = 1 + (|i| i + 2);"),
            ("let x = 1 + (|i| i) + 2;", "let x = 1 + (|i| i) + 2;"),
            ("let x = 1 + (|i| (i + 2));", "let x = 1 + (|i| i + 2);"),
            ("let x = (1 + (|i| i)) + 2;", "let x = 1 + (|i| i) + 2;"),
            ("let x = (1 + (|i| (i + 2)));", "let x = 1 + (|i| i + 2);"),
            ("let x = (1 + (|i| i + 2));", "let x = 1 + (|i| i + 2);"),
            // Index access
            ("(|i| i)[j];", "(|i| i)[j];"),
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }

    #[test]
    fn test_parentheses_complex() {
        let test_cases: Vec<TestCase> = vec![
            // Don't change concise expression
            (
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
            ),
            // Remove extra parentheses
            (
                "(a | ((b * (c << (d + e))) & (f ^ g))) = (h * ((i + g)));",
                "a | b * (c << d + e) & (f ^ g) = h * (i + g);",
            ),
            (
                "instr_or { 0, X, Y, Z } is (main_bin.latch * main_bin.sel[0]) { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };",
                "instr_or { 0, X, Y, Z } is main_bin.latch * main_bin.sel[0] { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };",
            ),
            (
                "instr_or { 0, X, Y, Z } is main_bin.latch * main_bin.sel[0] { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };",
                "instr_or { 0, X, Y, Z } is main_bin.latch * main_bin.sel[0] { main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C };",
            ),
            (
                "pc' = (1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1)));",
                "pc' = (1 - first_step') * (instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1));",
            )
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }
}
