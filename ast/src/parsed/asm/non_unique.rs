pub use super::generic::*;

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct NonUniqueSymbols {
    map: Vec<(String, SymbolValue)>,
}

impl FromIterator<SymbolDefinition> for NonUniqueSymbols {
    fn from_iter<T: IntoIterator<Item = SymbolDefinition>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().map(|d| (d.name, d.value)).collect(),
        }
    }
}

impl Extend<SymbolDefinition> for NonUniqueSymbols {
    fn extend<T: IntoIterator<Item = SymbolDefinition>>(&mut self, iter: T) {
        self.map.extend(iter.into_iter().map(|d| (d.name, d.value)));
    }
}

impl IntoIterator for NonUniqueSymbols {
    type Item = SymbolDefinition;

    type IntoIter = std::iter::Map<
        std::vec::IntoIter<(String, SymbolValue)>,
        fn((String, SymbolValue)) -> SymbolDefinition,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.map
            .into_iter()
            .map(|(name, value)| SymbolDefinition { name, value })
    }
}

impl Symbols for NonUniqueSymbols {
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

pub type Module = super::generic::Module<NonUniqueSymbols>;
pub type ModuleStatement = super::generic::ModuleStatement<NonUniqueSymbols>;
pub type SymbolDefinition = super::generic::SymbolDefinition<NonUniqueSymbols>;
pub type ASMProgram = super::generic::ASMProgram<NonUniqueSymbols>;
pub type ASMModule = super::generic::ASMModule<NonUniqueSymbols>;
pub type SymbolValue = super::generic::SymbolValue<NonUniqueSymbols>;
pub type SymbolValueRef<'a> = super::generic::SymbolValueRef<'a, NonUniqueSymbols>;
pub type ModuleRef<'a> = super::generic::ModuleRef<'a, NonUniqueSymbols>;
