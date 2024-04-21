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
    fn test_parentheses_simple() {
        let test_cases: Vec<TestCase> = vec![
            // Complete line
            ("let t = ((x + y) * z);", "let t = (x + y) * z;"),
            // Don't add extra
            ("-x + y * !z;", "-x + y * !z;"),
            ("x = (y <= z);", "x = (y <= z);"),
            ("x + y + z;", "x + y + z;"),
            ("x * y * z;", "x * y * z;"),
            ("x / y / z;", "x / y / z;"),
            // Remove unneeded
            ("(-x) + y * (!z);", "-x + y * !z;"),
            ("(x * y) * z;", "x * y * z;"),
            ("(x / y) / z;", "x / y / z;"),
            ("x ** (y ** z);", "x ** (y ** z);"),
            ("(x ** (y ** z));", "x ** (y ** z);"),
            // Observe associativity
            ("x * (y * z);", "x * (y * z);"),
            ("x / (y / z);", "x / (y / z);"),
            ("(x ** y) ** z;", "x ** y ** z;"),
            ("((x ** y) ** z);", "x ** y ** z;"),
            // Don't remove needed
            ("(x + y) * z;", "(x + y) * z;"),
            ("((x + y) * z);", "(x + y) * z;"),
            ("-(x + y);", "-(x + y);"),
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
        ];

        for test_case in test_cases {
            test_paren(&test_case);
        }
    }
}
