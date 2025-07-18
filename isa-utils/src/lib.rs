/// A single 32-bit data value.
pub enum SingleDataValue {
    /// A literal value.
    Value(u64),
    /// The value of a pointer to a text label. Since there may be not a
    /// 1-to-1 correspondence between nativa ISAs and Powdr ASM instructions,
    /// this is passed unresolved to the code generator.
    LabelReference(String),
    /// Currently not supported.
    Offset(String, String),
}

pub fn quote(s: &str) -> String {
    // TODO more things to quote
    format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\""))
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
