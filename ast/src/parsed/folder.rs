use super::{
    asm::{
        ASMModule, ASMProgram, Import, Machine, Module, ModuleStatement, SymbolDefinition,
        SymbolValue,
    },
    EnumDeclaration, Expression,
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
                    SymbolValue::Expression(e) => Ok(SymbolValue::Expression(e)),
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
