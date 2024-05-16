use powdr_ast::{
    parsed::{PILFile, PilStatement},
    SourceRef,
};
pub fn pil_statement_clear_source_ref(stmt: &mut PilStatement) {
    match stmt {
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
        | PilStatement::ConstantDefinition(s, _, _)
        | PilStatement::Expression(s, _)
        | PilStatement::StructDeclaration(s, _)
        | PilStatement::EnumDeclaration(s, _) => *s = SourceRef::unknown(),
    }
}
// helper function to clear SourceRef's inside the AST so we can compare for equality
pub fn pil_clear_source_refs(ast: &mut PILFile) {
    ast.0.iter_mut().for_each(pil_statement_clear_source_ref);
}
