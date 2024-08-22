use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub fn topo_sort<'a, E, F, I>(symbols: I, get_dependencies: F) -> Vec<E>
where
    F: Fn(&E) -> HashSet<E>,
    E: Clone + PartialEq + Eq + Hash + 'a,
    I: Iterator<Item = &'a E>,
{
    let graph: HashMap<_, HashSet<_>> = symbols.map(|e| (e, get_dependencies(e))).collect();
    let mut visited = Default::default();
    let mut result = Default::default();
    for (name, _) in graph.iter() {
        topo_sort_visit(*name, &graph, &mut visited, &mut result);
    }
    assert_eq!(graph.len(), result.len());
    result
}

fn topo_sort_visit<'a, 'b: 'a, E: Clone + PartialEq + Eq + Hash>(
    element: &'a E,
    graph: &'b HashMap<&'a E, HashSet<E>>,
    visited: &'b mut HashSet<E>,
    result: &'b mut Vec<E>,
) {
    if !visited.insert(element.clone()) {
        return;
    }
    if let Some(called) = graph.get(element) {
        for c in called {
            topo_sort_visit(c, graph, visited, result);
        }
    }
    result.push(element.clone());
}
