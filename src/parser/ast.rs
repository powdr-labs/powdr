#[derive(Debug, PartialEq, Eq)]
pub struct PILFile {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    IncludeStatement(IncludeStatement),
}

#[derive(Debug, PartialEq, Eq)]
pub struct IncludeStatement {
    pub file: String,
}
