pub fn compute_line_starts(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect::<Vec<_>>()
}

pub fn offset_to_line(offset: usize, line_starts: &[usize]) -> usize {
    match line_starts.binary_search(&offset) {
        Ok(line) => line + 1,
        Err(next_line) => next_line,
    }
}

pub fn quote(input: &str) -> String {
    format!("\"{}\"", input.replace('\\', "\\\\").replace('"', "\\\""))
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
    use super::{compute_line_starts, offset_to_line};

    #[test]
    pub fn line_calc() {
        let input = "abc\nde";
        let breaks = compute_line_starts(input);
        let lines = (0..input.len())
            .map(|o| offset_to_line(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(lines, [1, 1, 1, 1, 2, 2]);
    }

    #[test]
    pub fn line_calc_empty_start() {
        let input = "\nab\n\nc\nde\n";
        let breaks = compute_line_starts(input);
        let lines = (0..input.len())
            .map(|o| offset_to_line(o, &breaks))
            .collect::<Vec<_>>();
        assert_eq!(lines, [1, 2, 2, 2, 3, 4, 4, 5, 5, 5]);
    }
}
