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
    // TODO more things to quote
    format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\""))
}

pub fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_").replace('/', "_slash_")
}
