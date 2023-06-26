use number::AbstractNumberType;

use super::{Expression, SelectedExpressions, Statement};

pub mod batched {
    use std::collections::BTreeSet;

    use super::*;

    #[derive(Default, Debug, PartialEq, Eq)]
    pub struct ASMStatementBatch<T> {
        // the set of compatible statements
        pub statements: Vec<ASMStatement<T>>,
        // the reason why this batch ended (for debugging purposes), None if we ran out of statements to batch
        pub reason: Option<IncompatibleSet>,
    }

    impl<T> ASMStatementBatch<T> {
        pub fn into_statements(self) -> impl Iterator<Item = ASMStatement<T>> {
            self.statements.into_iter()
        }

        pub fn statements(&self) -> impl Iterator<Item = &ASMStatement<T>> {
            self.statements.iter()
        }

        pub fn size(&self) -> usize {
            self.statements.len()
        }

        pub fn insert(&mut self, s: ASMStatement<T>) {
            self.statements.push(s)
        }
    }

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Incompatible {
        Label,
        Unimplemented,
    }

    #[derive(Debug, PartialEq, Eq, Default)]
    pub struct IncompatibleSet(pub BTreeSet<Incompatible>);

    #[derive(Debug, PartialEq, Eq)]
    pub struct BatchedASMFile<T> {
        pub declarations: Vec<ASMStatement<T>>,
        pub batches: Vec<ASMStatementBatch<T>>,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ASMFile<T>(pub Vec<ASMStatement<T>>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParamList {
    pub params: Vec<InstructionParam>,
}

impl InstructionParamList {
    pub fn new(params: Vec<InstructionParam>) -> Self {
        Self { params }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParams {
    pub inputs: InstructionParamList,
    pub outputs: Option<InstructionParamList>,
}

impl InstructionParams {
    pub fn new(inputs: InstructionParamList, outputs: Option<InstructionParamList>) -> Self {
        Self { inputs, outputs }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASMStatement<T> {
    Degree(usize, AbstractNumberType),
    RegisterDeclaration(usize, String, Option<RegisterFlag>),
    InstructionDeclaration(
        usize,
        String,
        InstructionParams,
        Vec<InstructionBodyElement<T>>,
    ),
    InlinePil(usize, Vec<Statement<T>>),
    Assignment(usize, Vec<String>, Option<String>, Box<Expression<T>>),
    Instruction(usize, String, Vec<Expression<T>>),
    Label(usize, String),
    DebugDirective(usize, DebugDirective),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DebugDirective {
    File(usize, String, String),
    Loc(usize, usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RegisterFlag {
    IsPC,
    IsAssignment,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InstructionParam {
    pub name: String,
    pub ty: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionBodyElement<T> {
    Expression(Expression<T>),
    PlookupIdentity(
        SelectedExpressions<T>,
        PlookupOperator,
        SelectedExpressions<T>,
    ),
    FunctionCall(String, Vec<Expression<T>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlookupOperator {
    In,
    Is,
}
