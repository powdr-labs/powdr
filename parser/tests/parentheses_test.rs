#[cfg(test)]
mod test {
    use powdr_parser::parse;
    use powdr_parser_util::UnwrapErrToStderr;
    use pretty_assertions::assert_eq;
    use test_log::test;

    fn test_paren(input: &str, expected: &str) {
        let printed = format!("{}", parse(None, input).unwrap_err_to_stderr());
        assert_eq!(expected.trim(), printed.trim());
    }

    #[test]
    fn test_parentheses() {
        type TestCase = (&'static str, &'static str);
        let test_cases: Vec<TestCase> = vec![
            // Remove unneeded
            ("(x * y) * z;", "x * y * z;"),
            ("(x / y) / z;", "x / y / z;"),
            ("x ** (y ** z);", "x ** y ** z;"),
            ("(x ** (y ** z));", "x ** y ** z;"),
            // Observe associativity
            ("x * (y * z);", "x * (y * z);"),
            ("x / (y / z);", "x / (y / z);"),
            ("(x ** y) ** z;", "(x ** y) ** z;"),
            ("((x ** y) ** z);", "(x ** y) ** z;"),
            // Don't remove needed
            ("(x + y) * z;", "(x + y) * z;"),
            ("((x + y) * z);", "(x + y) * z;"),
            // Don't add extra
            ("x + y + z;", "x + y + z;"),
            ("x * y * z;", "x * y * z;"),
            ("x / y / z;", "x / y / z;"),
        ];

        for (input, expected) in test_cases {
            test_paren(input, expected);
        }
    }
}
