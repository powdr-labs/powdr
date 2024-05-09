use powdr_ast::parsed::visitor::Children;
use powdr_ast::{
    parsed::{PILFile, PilStatement},
    SourceRef,
};
pub fn pil_statement_clear_source_ref(stmt: &mut PilStatement) {
    stmt.children_mut()
        .for_each(pil_expression_clear_source_ref);

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
        | PilStatement::EnumDeclaration(s, _) => *s = SourceRef::unknown(),
    }
}

fn pil_expression_clear_source_ref(expr: &mut powdr_ast::parsed::Expression) {
    use powdr_ast::parsed::Expression::*;
    match expr {
        Reference(s, _)
        | PublicReference(s, _)
        | Number(s, _)
        | String(s, _)
        | Tuple(s, _)
        | ArrayLiteral(s, _)
        | UnaryOperation(s, _)
        | BinaryOperation(s, _)
        | IndexAccess(s, _)
        | IfExpression(s, _)
        | MatchExpression(s, _)
        | FunctionCall(s, _)
        | LambdaExpression(s, _)
        | BlockExpression(s, _)
        | FreeInput(s, _) => {
            *s = SourceRef::unknown();
        }
    }
}

// helper function to clear SourceRef's inside the AST so we can compare for equality
pub fn pil_clear_source_refs(ast: &mut PILFile) {
    ast.0.iter_mut().for_each(pil_statement_clear_source_ref);
}
