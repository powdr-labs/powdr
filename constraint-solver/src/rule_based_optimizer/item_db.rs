use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Index;

/// A database of items that are assigned consecutive identifiers
/// and which can translate back and forth between identifiers
/// and items.
pub struct ItemDB<Item, Ident> {
    items: Vec<Item>,
    reverse: HashMap<Item, usize>,
    _phantom: std::marker::PhantomData<Ident>,
}

impl<Item, Ident> Default for ItemDB<Item, Ident> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            reverse: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Item, Ident> FromIterator<Item> for ItemDB<Item, Ident>
where
    Item: Clone + Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = Item>>(iter: T) -> Self {
        let items = iter.into_iter().collect::<Vec<_>>();
        let reverse = items
            .iter()
            .enumerate()
            .map(|(i, v)| (v.clone(), i))
            .collect();
        Self {
            items,
            reverse,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Item, Ident> Index<Ident> for ItemDB<Item, Ident>
where
    Ident: Into<usize>,
{
    type Output = Item;
    fn index(&self, index: Ident) -> &Self::Output {
        &self.items[index.into()]
    }
}

impl<Item, Ident> ItemDB<Item, Ident>
where
    Item: Clone + Hash + Eq,
    Ident: From<usize> + Copy,
{
    fn insert_owned_new(&mut self, item: Item) -> Ident {
        let id = self.items.len();
        self.items.push(item.clone());
        self.reverse.insert(item, id);
        Ident::from(id)
    }

    /// Inserts the item if not already present, returning its identifier.
    /// Use this function over `insert_owned` when you only have a
    /// reference to the item.
    pub fn insert(&mut self, item: &Item) -> Ident {
        if let Some(&id) = self.reverse.get(item) {
            Ident::from(id)
        } else {
            self.insert_owned_new(item.clone())
        }
    }

    /// Inserts the item if not already present, returning its identifier.
    /// Use this function over `insert` when you have ownership of the item.
    pub fn insert_owned(&mut self, item: Item) -> Ident {
        if let Some(&id) = self.reverse.get(&item) {
            Ident::from(id)
        } else {
            self.insert_owned_new(item)
        }
    }

    pub fn id(&self, item: &Item) -> Ident {
        self.reverse.get(item).map(|&id| Ident::from(id)).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Ident, &Item)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (Ident::from(i), item))
    }

    // TODO avoid using this (as pub)
    pub fn next_free_id(&self) -> usize {
        self.items.len()
    }
}
