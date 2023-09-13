use super::asm::{
    ASMModule, ASMProgram, Import, Machine, Module, ModuleStatement, SymbolDefinition, SymbolValue,
};

pub trait Folder<T> {
    type Error;

    fn fold_program(&mut self, p: ASMProgram<T>) -> Result<ASMProgram<T>, Self::Error> {
        let main = self.fold_module_value(p.main)?;

        Ok(ASMProgram { main })
    }

    fn fold_module_value(&mut self, module: ASMModule<T>) -> Result<ASMModule<T>, Self::Error> {
        let statements = module
            .statements
            .into_iter()
            .map(|s| match s {
                ModuleStatement::SymbolDefinition(d) => match d.value {
                    SymbolValue::Machine(machine) => self.fold_machine(machine).map(From::from),
                    SymbolValue::Import(import) => self.fold_import(import).map(From::from),
                    SymbolValue::Module(module) => self.fold_module(module).map(From::from),
                }
                .map(|value| ModuleStatement::SymbolDefinition(SymbolDefinition { value, ..d })),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ASMModule { statements })
    }

    fn fold_module(&mut self, m: Module<T>) -> Result<Module<T>, Self::Error> {
        Ok(match m {
            Module::External(e) => Module::External(e),
            Module::Local(m) => Module::Local(self.fold_module_value(m)?),
        })
    }

    fn fold_machine(&mut self, machine: Machine<T>) -> Result<Machine<T>, Self::Error> {
        Ok(machine)
    }

    fn fold_import(&mut self, import: Import) -> Result<Import, Self::Error> {
        Ok(import)
    }
}
