/// A single 32-bit data value.
pub enum SingleDataValue {
    /// A literal value.
    Value(u32),
    /// The value of a pointer to a text label. Since there may be not a
    /// 1-to-1 correspondence between nativa ISAs and Powdr ASM instructions,
    /// this is passed unresolved to the code generator.
    LabelReference(String),
    /// Currently not supported.
    Offset(String, String),
}

pub fn quote(s: &str) -> String {
    let escaped = s
        .chars()
        .map(|c| match c {
            '\\' => "\\\\".to_string(),
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            c if c.is_control() => format!("\\u{{{:x}}}", c as u32),
            c => c.to_string(),
        })
        .collect::<String>();
    format!("\"{escaped}\"")
}

pub fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_")
        .replace('/', "_slash_")
        .replace("[]", "_slice_")
        .replace(",", "_comma_")
        .replace("(", "_left_parens_")
        .replace(")", "_right_parens_")
        .replace("[", "_left_square_")
        .replace("]", "_right_square_")
        .replace("{", "_left_brace_")
        .replace("}", "_right_brace_")
        .replace(" ", "_space_")
        .replace("'", "_quote_")
        .replace("*", "_deref_")
}

#[cfg(test)]
mod tests {
    use super::quote;

    #[test]
    fn quote_escapes_control_characters() {
        assert_eq!(
            quote("line\ncolumn\treturn\r"),
            "\"line\\ncolumn\\treturn\\r\""
        );
    }
}
