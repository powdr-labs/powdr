pub use std::collections::BTreeMap;

pub use super::generic::*;

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct UniqueSymbols {
    map: BTreeMap<String, SymbolValue>,
}

impl Extend<SymbolDefinition> for UniqueSymbols {
    fn extend<T: IntoIterator<Item = SymbolDefinition>>(&mut self, iter: T) {
        self.map.extend(iter.into_iter().map(|d| (d.name, d.value)));
    }
}

impl FromIterator<SymbolDefinition> for UniqueSymbols {
    fn from_iter<T: IntoIterator<Item = SymbolDefinition>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().map(|d| (d.name, d.value)).collect(),
        }
    }
}

impl IntoIterator for UniqueSymbols {
    type Item = SymbolDefinition;

    type IntoIter = std::iter::Map<
        std::collections::btree_map::IntoIter<String, SymbolValue>,
        fn((String, SymbolValue)) -> SymbolDefinition,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.map
            .into_iter()
            .map(|(name, value)| SymbolDefinition { name, value })
    }
}

impl Symbols for UniqueSymbols {
    fn iter_mut(&mut self) -> impl Iterator<Item = SymbolDefinitionMut<Self>> {
        self.map
            .iter_mut()
            .map(|(name, value)| SymbolDefinitionMut { name, value })
    }

    fn iter(&self) -> impl Iterator<Item = SymbolDefinitionRef<Self>> {
        self.map
            .iter()
            .map(|(name, value)| SymbolDefinitionRef { name, value })
    }
}

pub type Module = super::generic::Module<UniqueSymbols>;
pub type ModuleStatement = super::generic::ModuleStatement<UniqueSymbols>;
pub type SymbolDefinition = super::generic::SymbolDefinition<UniqueSymbols>;
pub type ASMProgram = super::generic::ASMProgram<UniqueSymbols>;
pub type ASMModule = super::generic::ASMModule<UniqueSymbols>;
pub type SymbolValue = super::generic::SymbolValue<UniqueSymbols>;
pub type SymbolValueRef<'a> = super::generic::SymbolValueRef<'a, UniqueSymbols>;
pub type ModuleRef<'a> = super::generic::ModuleRef<'a, UniqueSymbols>;
