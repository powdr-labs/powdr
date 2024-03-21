use super::{
    asm::{
        ASMModule, ASMProgram, Import, Machine, Module, ModuleStatement, SymbolDefinition,
        SymbolValue,
    },
    ArrayLiteral, EnumDeclaration, Expression, FunctionCall, IfExpression, IndexAccess,
    LambdaExpression, LetStatementInsideBlock, MatchArm, MatchPattern,
};

pub trait Folder {
    type Error;

    fn fold_program(&mut self, p: ASMProgram) -> Result<ASMProgram, Self::Error> {
        let main = self.fold_module_value(p.main)?;

        Ok(ASMProgram { main })
    }

    fn fold_module_value(&mut self, module: ASMModule) -> Result<ASMModule, Self::Error> {
        let statements = module
            .statements
            .into_iter()
            .map(|s| match s {
                ModuleStatement::SymbolDefinition(d) => match d.value {
                    SymbolValue::Machine(machine) => self.fold_machine(machine).map(From::from),
                    SymbolValue::Import(import) => self.fold_import(import).map(From::from),
                    SymbolValue::Module(module) => self.fold_module(module).map(From::from),
                    SymbolValue::Expression(e) => {
                        // Not folding expressions by default because the ExpressionFolder
                        // is a different trait.
                        Ok(SymbolValue::Expression(e))
                    }
                    SymbolValue::TypeDeclaration(ty) => {
                        self.fold_type_declaration(ty).map(From::from)
                    }
                }
                .map(|value| ModuleStatement::SymbolDefinition(SymbolDefinition { value, ..d })),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ASMModule { statements })
    }

    fn fold_module(&mut self, m: Module) -> Result<Module, Self::Error> {
        Ok(match m {
            Module::External(e) => Module::External(e),
            Module::Local(m) => Module::Local(self.fold_module_value(m)?),
        })
    }

    fn fold_machine(&mut self, machine: Machine) -> Result<Machine, Self::Error> {
        Ok(machine)
    }

    fn fold_import(&mut self, import: Import) -> Result<Import, Self::Error> {
        Ok(import)
    }

    fn fold_type_declaration(
        &mut self,
        ty: EnumDeclaration<Expression>,
    ) -> Result<EnumDeclaration<Expression>, Self::Error> {
        Ok(ty)
    }
}

pub trait ExpressionFolder<Ref> {
    type Error;
    fn fold_expression(&mut self, e: Expression<Ref>) -> Result<Expression<Ref>, Self::Error> {
        self.fold_expression_default(e)
    }

    fn fold_expression_default(
        &mut self,
        e: Expression<Ref>,
    ) -> Result<Expression<Ref>, Self::Error> {
        Ok(match e {
            Expression::Reference(r) => Expression::Reference(self.fold_reference(r)?),
            Expression::PublicReference(r) => Expression::PublicReference(r),
            Expression::Number(n, t) => Expression::Number(n, t),
            Expression::String(s) => Expression::String(s),
            Expression::Tuple(t) => Expression::Tuple(self.fold_expressions(t)?),
            Expression::LambdaExpression(l) => Expression::LambdaExpression(self.fold_lambda(l)?),
            Expression::ArrayLiteral(lit) => Expression::ArrayLiteral(ArrayLiteral {
                items: self.fold_expressions(lit.items)?,
            }),
            Expression::BinaryOperation(l, op, r) => Expression::BinaryOperation(
                self.fold_boxed_expression(*l)?,
                op,
                self.fold_boxed_expression(*r)?,
            ),
            Expression::UnaryOperation(op, inner) => {
                Expression::UnaryOperation(op, Box::new(self.fold_expression(*inner)?))
            }
            Expression::IndexAccess(index_access) => {
                Expression::IndexAccess(self.fold_index_access(index_access)?)
            }
            Expression::FunctionCall(fun_call) => {
                Expression::FunctionCall(self.fold_function_call(fun_call)?)
            }
            Expression::FreeInput(input) => {
                Expression::FreeInput(self.fold_boxed_expression(*input)?)
            }
            Expression::MatchExpression(scr, arms) => Expression::MatchExpression(
                self.fold_boxed_expression(*scr)?,
                arms.into_iter()
                    .map(|a| self.fold_match_arm(a))
                    .collect::<Result<_, _>>()?,
            ),
            Expression::IfExpression(if_expr) => {
                Expression::IfExpression(self.fold_if_expression(if_expr)?)
            }
            Expression::BlockExpression(statements, expr) => Expression::BlockExpression(
                statements
                    .into_iter()
                    .map(|s| self.fold_let_statement_inside_block(s))
                    .collect::<Result<_, _>>()?,
                self.fold_boxed_expression(*expr)?,
            ),
        })
    }

    fn fold_reference(&mut self, r: Ref) -> Result<Ref, Self::Error> {
        Ok(r)
    }

    fn fold_lambda(
        &mut self,
        l: LambdaExpression<Ref>,
    ) -> Result<LambdaExpression<Ref>, Self::Error> {
        Ok(LambdaExpression {
            kind: l.kind,
            params: l.params,
            body: self.fold_boxed_expression(*l.body)?,
        })
    }

    fn fold_index_access(
        &mut self,
        IndexAccess { array, index }: IndexAccess<Ref>,
    ) -> Result<IndexAccess<Ref>, Self::Error> {
        Ok(IndexAccess {
            array: self.fold_boxed_expression(*array)?,
            index: self.fold_boxed_expression(*index)?,
        })
    }

    fn fold_function_call(
        &mut self,
        FunctionCall {
            function,
            arguments,
        }: FunctionCall<Ref>,
    ) -> Result<FunctionCall<Ref>, Self::Error> {
        Ok(FunctionCall {
            function: self.fold_boxed_expression(*function)?,
            arguments: self.fold_expressions(arguments)?,
        })
    }

    fn fold_match_arm(
        &mut self,
        MatchArm { pattern, value }: MatchArm<Ref>,
    ) -> Result<MatchArm<Ref>, Self::Error> {
        Ok(MatchArm {
            pattern: self.fold_match_pattern(pattern)?,
            value: self.fold_expression(value)?,
        })
    }

    fn fold_match_pattern(
        &mut self,
        pattern: MatchPattern<Ref>,
    ) -> Result<MatchPattern<Ref>, Self::Error> {
        Ok(match pattern {
            MatchPattern::CatchAll => MatchPattern::CatchAll,
            MatchPattern::Pattern(p) => MatchPattern::Pattern(self.fold_expression(p)?),
        })
    }

    fn fold_if_expression(
        &mut self,
        IfExpression {
            condition,
            body,
            else_body,
        }: IfExpression<Ref>,
    ) -> Result<IfExpression<Ref>, Self::Error> {
        Ok(IfExpression {
            condition: self.fold_boxed_expression(*condition)?,
            body: self.fold_boxed_expression(*body)?,
            else_body: self.fold_boxed_expression(*else_body)?,
        })
    }

    fn fold_let_statement_inside_block(
        &mut self,
        LetStatementInsideBlock { name, value }: LetStatementInsideBlock<Ref>,
    ) -> Result<LetStatementInsideBlock<Ref>, Self::Error> {
        Ok(LetStatementInsideBlock {
            name,
            value: value.map(|v| self.fold_expression(v)).transpose()?,
        })
    }

    fn fold_boxed_expression(
        &mut self,
        e: Expression<Ref>,
    ) -> Result<Box<Expression<Ref>>, Self::Error> {
        Ok(Box::new(self.fold_expression(e)?))
    }

    fn fold_expressions<I: IntoIterator<Item = Expression<Ref>>>(
        &mut self,
        items: I,
    ) -> Result<Vec<Expression<Ref>>, Self::Error> {
        items
            .into_iter()
            .map(|x| self.fold_expression(x))
            .collect::<Result<_, _>>()
    }
}
