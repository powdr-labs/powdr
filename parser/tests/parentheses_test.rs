#[cfg(test)]
mod test {
    use powdr_parser::{parse, test_utils::pil_clear_source_refs};
    use powdr_parser_util::UnwrapErrToStderr;
    use pretty_assertions::assert_eq;
    use test_log::test;

    type TestCase = (&'static str, &'static str);

    fn test_paren(test_case: &TestCase) {
        let (input, expected) = test_case;
        let mut parsed = parse(None, input).unwrap_err_to_stderr();
        let printed = parsed.to_string();
        assert_eq!(expected.trim(), printed.trim());
        let mut re_parsed = parse(None, printed.as_str()).unwrap_err_to_stderr();

        pil_clear_source_refs(&mut parsed);
        pil_clear_source_refs(&mut re_parsed);
        assert_eq!(parsed, re_parsed);
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
            ("(x ** (y ** z));", "x ** (y ** z);"),
            ("(x - (y + z));", "x - (y + z);"),
            // Observe associativity
            ("x * (y * z);", "x * (y * z);"),
            ("x / (y / z);", "x / (y / z);"),
            ("x ** (y ** z);", "x ** (y ** z);"),
            ("(x ** y) ** z;", "(x ** y) ** z;"),
            // Don't remove needed
            ("(x + y) * z;", "(x + y) * z;"),
            ("((x + y) * z);", "(x + y) * z;"),
            ("-(x + y);", "-(x + y);"),
            // function call
            ("(a + b)(2);", "(a + b)(2);"),
            // Index access
            ("(a + b)[2];", "(a + b)[2];"),
            ("(i < 7) && (6 >= -i);", "i < 7 && 6 >= -i;"),
            // Power test
            ("(-x) ** (-y);", "(-x) ** (-y);"),
            ("2 ** x';", "2 ** (x');"),
            ("(2 ** x)';", "(2 ** x)';"),
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
                "instr_or $ [0, X, Y, Z] is (main_bin.latch * main_bin.sel[0]) $ [main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C];",
                "instr_or $ [0, X, Y, Z] is main_bin.latch * main_bin.sel[0] $ [main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C];",
            ),
            (
                "instr_or $ [0, X, Y, Z] is main_bin.latch * main_bin.sel[0] $ [main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C];",
                "instr_or $ [0, X, Y, Z] is main_bin.latch * main_bin.sel[0] $ [main_bin.operation_id, main_bin.A, main_bin.B, main_bin.C];",
            ),
            (
                "pc' = (1 - first_step') * ((((instr__jump_to_operation * _operation_id) + (instr__loop * pc)) + (instr_return * 0)) + ((1 - ((instr__jump_to_operation + instr__loop) + instr_return)) * (pc + 1)));",
                "pc' = (1 - first_step') * (instr__jump_to_operation * _operation_id + instr__loop * pc + instr_return * 0 + (1 - (instr__jump_to_operation + instr__loop + instr_return)) * (pc + 1));",
            ),
            (
                "let root_of_unity_for_log_degree: int -> fe = |n| root_of_unity ** (2**(32 - n));",
                "let root_of_unity_for_log_degree: int -> fe = (|n| root_of_unity ** (2 ** (32 - n)));",
            ),
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }

    #[test]
    fn test_index_access_parentheses() {
        let test_cases: Vec<TestCase> = vec![
            ("(x')(2);", "(x')(2);"),
            ("x[2](2);", "x[2](2);"),
            ("(x')[2];", "(x')[2];"),
            ("-x[2];", "-x[2];"),
            ("(-x)[2];", "(-x)[2];"),
            ("-(x[2]);", "-x[2];"),
            ("1 + x[2];", "1 + x[2];"),
            ("1 + x(2);", "1 + x(2);"),
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }
}
