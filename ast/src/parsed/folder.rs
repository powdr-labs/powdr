use super::{
    asm::{ASMModule, ASMProgram, Import, Machine, Module, SymbolDefinition, SymbolValue, Symbols},
    EnumDeclaration, Expression, TraitDeclaration, TraitImplementation,
};

pub fn fold_module_value<S0: Symbols, S1: Symbols, F: Folder<S0, S1>>(
    f: &mut F,
    module: ASMModule<S0>,
) -> Result<ASMModule<S1>, F::Error> {
    let symbols = module
        .symbols
        .into_iter()
        .map(|d| {
            match d.value {
                SymbolValue::Machine(machine) => f.fold_machine(machine).map(From::from),
                SymbolValue::Import(import) => f.fold_import(import).map(From::from),
                SymbolValue::Module(module) => f.fold_module(module).map(From::from),
                SymbolValue::Expression(e) => Ok(SymbolValue::Expression(e)),
                SymbolValue::TypeDeclaration(ty) => f.fold_type_declaration(ty).map(From::from),
                SymbolValue::TraitDeclaration(trait_decl) => {
                    f.fold_trait_declaration(trait_decl).map(From::from)
                }
            }
            .map(|value| SymbolDefinition {
                value,
                name: d.name,
            })
        })
        .collect::<Result<_, _>>()?;
    let implementations = module
        .implementations
        .into_iter()
        .map(|i| f.fold_trait_implementation(i))
        .collect::<Result<_, _>>()?;

    Ok(ASMModule {
        symbols,
        implementations,
    })
}

pub trait Folder<S0: Symbols, S1: Symbols>: Sized {
    type Error;

    fn fold_program(&mut self, p: ASMProgram<S0>) -> Result<ASMProgram<S1>, Self::Error> {
        let main = self.fold_module_value(p.main)?;

        Ok(ASMProgram { main })
    }

    fn fold_module_value(&mut self, module: ASMModule<S0>) -> Result<ASMModule<S1>, Self::Error> {
        fold_module_value(self, module)
    }

    fn fold_module(&mut self, m: Module<S0>) -> Result<Module<S1>, Self::Error> {
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

    fn fold_trait_implementation(
        &mut self,
        trait_impl: TraitImplementation<Expression>,
    ) -> Result<TraitImplementation<Expression>, Self::Error> {
        Ok(trait_impl)
    }

    fn fold_trait_declaration(
        &mut self,
        trait_decl: TraitDeclaration<Expression>,
    ) -> Result<TraitDeclaration<Expression>, Self::Error> {
        Ok(trait_decl)
    }
}
