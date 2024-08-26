use powdr_ast::parsed::asm::{
    ASMModule, ASMProgram, FunctionStatement, Instruction, Machine, MachineStatement, Module,
    SymbolValue, Symbols,
};
use powdr_ast::parsed::visitor::Children;
use powdr_ast::parsed::{
    BlockExpression, Expression, LambdaExpression, LetStatementInsideBlock, MatchExpression,
    PILFile, Pattern, PilStatement, SourceReference, StatementInsideBlock, TraitImplementation,
};
use powdr_parser_util::SourceRef;

// Helper to clear SourceRef's inside the AST so we can compare for equality
pub trait ClearSourceRefs {
    fn clear_source_refs(&mut self);
}

impl<S: Symbols> ClearSourceRefs for ASMProgram<S> {
    fn clear_source_refs(&mut self) {
        self.main.clear_source_refs();
    }
}

impl<S: Symbols> ClearSourceRefs for ASMModule<S> {
    fn clear_source_refs(&mut self) {
        self.symbols
            .iter_mut()
            .map(|d| d.value)
            .for_each(ClearSourceRefs::clear_source_refs);
        self.implementations
            .iter_mut()
            .for_each(ClearSourceRefs::clear_source_refs);
    }
}

impl<E> ClearSourceRefs for TraitImplementation<E> {
    fn clear_source_refs(&mut self) {
        todo!()
    }
}

impl ClearSourceRefs for MachineStatement {
    fn clear_source_refs(&mut self) {
        match self {
            MachineStatement::Submachine(s, _, _, _)
            | MachineStatement::RegisterDeclaration(s, _, _)
            | MachineStatement::OperationDeclaration(s, _, _, _)
            | MachineStatement::LinkDeclaration(s, _) => {
                *s = SourceRef::unknown();
            }
            MachineStatement::Pil(s, stmt) => {
                *s = SourceRef::unknown();
                stmt.clear_source_refs()
            }
            MachineStatement::InstructionDeclaration(s, _, Instruction { body, .. }) => {
                *s = SourceRef::unknown();
                body.0
                    .iter_mut()
                    .for_each(ClearSourceRefs::clear_source_refs)
            }
            MachineStatement::FunctionDeclaration(s, _, _, statements) => {
                *s = SourceRef::unknown();
                for statement in statements {
                    match statement {
                        FunctionStatement::Assignment(s, _, _, _)
                        | FunctionStatement::Instruction(s, _, _)
                        | FunctionStatement::Label(s, _)
                        | FunctionStatement::DebugDirective(s, _)
                        | FunctionStatement::Return(s, _) => *s = SourceRef::unknown(),
                    }
                }
            }
        }
    }
}

impl<S: Symbols> ClearSourceRefs for SymbolValue<S> {
    fn clear_source_refs(&mut self) {
        match self {
            SymbolValue::Machine(Machine { statements, .. }) => statements
                .iter_mut()
                .for_each(ClearSourceRefs::clear_source_refs),
            SymbolValue::Module(Module::Local(m)) => {
                m.clear_source_refs();
            }
            SymbolValue::Module(Module::External(_)) | SymbolValue::Import(_) => {}
            SymbolValue::Expression(e) => e.e.clear_source_refs(),
            SymbolValue::TypeDeclaration(decl) => decl
                .children_mut()
                .for_each(ClearSourceRefs::clear_source_refs),
            SymbolValue::TraitDeclaration(trait_decl) => trait_decl
                .children_mut()
                .for_each(ClearSourceRefs::clear_source_refs),
        }
    }
}

impl ClearSourceRefs for PILFile {
    fn clear_source_refs(&mut self) {
        self.0
            .iter_mut()
            .for_each(ClearSourceRefs::clear_source_refs);
    }
}

impl ClearSourceRefs for PilStatement {
    fn clear_source_refs(&mut self) {
        self.children_mut()
            .for_each(ClearSourceRefs::clear_source_refs);

        match self {
            PilStatement::Include(s, _)
            | PilStatement::Namespace(s, _, _)
            | PilStatement::LetStatement(s, _, _, _)
            | PilStatement::PolynomialDefinition(s, _, _)
            | PilStatement::PublicDeclaration(s, _, _, _, _)
            | PilStatement::PolynomialConstantDeclaration(s, _)
            | PilStatement::PolynomialConstantDefinition(s, _, _)
            | PilStatement::PolynomialCommitDeclaration(s, _, _, _)
            | PilStatement::PlookupIdentity(s, _, _)
            | PilStatement::PermutationIdentity(s, _, _)
            | PilStatement::ConnectIdentity(s, _, _)
            | PilStatement::Expression(s, _)
            | PilStatement::EnumDeclaration(s, _)
            | PilStatement::TraitDeclaration(s, _)
            | PilStatement::TraitImplementation(s, _) => *s = SourceRef::unknown(),
        }
    }
}

impl<R> ClearSourceRefs for Expression<R> {
    fn clear_source_refs(&mut self) {
        *self.source_reference_mut() = SourceRef::unknown();
        self.children_mut().for_each(|e| e.clear_source_refs());
        match self {
            Expression::Reference(_, _)
            | Expression::PublicReference(_, _)
            | Expression::Number(_, _)
            | Expression::String(_, _)
            | Expression::Tuple(_, _)
            | Expression::ArrayLiteral(_, _)
            | Expression::UnaryOperation(_, _)
            | Expression::BinaryOperation(_, _)
            | Expression::IndexAccess(_, _)
            | Expression::FunctionCall(_, _)
            | Expression::FreeInput(_, _)
            | Expression::IfExpression(_, _) => {}
            Expression::BlockExpression(_, b) => b.clear_source_refs(),
            Expression::MatchExpression(_, m) => m.clear_source_refs(),
            Expression::LambdaExpression(_, l) => l.clear_source_refs(),
        }
    }
}

impl<E> ClearSourceRefs for BlockExpression<E> {
    fn clear_source_refs(&mut self) {
        for s in &mut self.statements {
            match s {
                StatementInsideBlock::LetStatement(LetStatementInsideBlock { pattern, .. }) => {
                    pattern.clear_source_refs()
                }
                StatementInsideBlock::Expression(_) => {}
            }
        }
    }
}

impl<E> ClearSourceRefs for MatchExpression<E> {
    fn clear_source_refs(&mut self) {
        for arm in &mut self.arms {
            arm.pattern.clear_source_refs()
        }
    }
}

impl<E> ClearSourceRefs for LambdaExpression<E> {
    fn clear_source_refs(&mut self) {
        self.params
            .iter_mut()
            .for_each(ClearSourceRefs::clear_source_refs)
    }
}

impl ClearSourceRefs for Pattern {
    fn clear_source_refs(&mut self) {
        *self.source_reference_mut() = SourceRef::unknown();
        self.children_mut().for_each(|p| p.clear_source_refs());
    }
}
