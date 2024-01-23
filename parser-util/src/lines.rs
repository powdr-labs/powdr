pub fn compute_line_starts(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect::<Vec<_>>()
}

/// Returns a tuple `(line, col)` given the file offset of line starts.
/// `line` is 1 based and `col` is 0 based.
pub fn offset_to_line_col(offset: usize, line_starts: &[usize]) -> (usize, usize) {
    let line = match line_starts.binary_search(&offset) {
        Ok(line) => line + 1,
        Err(next_line) => next_line,
    };
    (line, offset - line_starts[line - 1])
}

pub fn indent(input: &str, indentation: &str) -> String {
    if input.is_empty() {
        String::new()
    } else {
        indentation.to_string() + &input.replace('\n', &format!("\n{indentation}"))
    }
}

#[cfg(test)]
mod test {
    use super::{compute_line_starts, offset_to_line_col};
    use test_log::test;

    #[test]
    pub fn line_calc() {
        let input = "abc\nde";
        let breaks = compute_line_starts(input);
        let line_col_pairs = (0..input.len())
            .map(|o| offset_to_line_col(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(
            line_col_pairs,
            [(1, 0), (1, 1), (1, 2), (1, 3), (2, 0), (2, 1)]
        );
    }

    #[test]
    pub fn line_calc_empty_start() {
        let input = "\nab\n\nc\nde\n";
        let breaks = compute_line_starts(input);
        let line_col_pairs = (0..input.len())
            .map(|o| offset_to_line_col(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(
            line_col_pairs,
            [
                (1, 0),
                (2, 0),
                (2, 1),
                (2, 2),
                (3, 0),
                (4, 0),
                (4, 1),
                (5, 0),
                (5, 1),
                (5, 2)
            ]
        );
    }
}
