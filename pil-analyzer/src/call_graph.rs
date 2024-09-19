use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{Expression, Reference},
    parsed::visitor::AllChildren,
};

/// Returns a sorted list of symbols such that called symbols appear before the symbols that reference them.
/// Circular dependencies appear in an arbitrary order.
pub fn sort_called_first<'a, I: Iterator<Item = (&'a str, Option<&'a Expression>)>>(
    symbols: I,
) -> Vec<String> {
    let graph = call_graph(symbols);
    let mut visited: HashSet<&str> = HashSet::new();
    let mut result: Vec<String> = Vec::new();
    for (name, _) in graph.iter() {
        topo_sort_visit(name, &graph, &mut visited, &mut result);
    }
    assert_eq!(graph.len(), result.len());
    result
}

fn topo_sort_visit<'a, 'b>(
    name: &'a str,
    graph: &'b HashMap<&'a str, HashSet<&'a str>>,
    visited: &'b mut HashSet<&'a str>,
    result: &'b mut Vec<String>,
) {
    if !visited.insert(name) {
        return;
    }
    if let Some(called) = graph.get(name) {
        for c in called {
            topo_sort_visit(c, graph, visited, result);
        }
    }
    result.push(name.to_string());
}

fn call_graph<'a, I: Iterator<Item = (&'a str, Option<&'a Expression>)>>(
    symbols: I,
) -> HashMap<&'a str, HashSet<&'a str>> {
    symbols
        .map(|(name, expr)| {
            let mut called: HashSet<&str> = HashSet::new();
            if let Some(e) = expr {
                e.all_children().for_each(|e| {
                    if let Expression::Reference(_, Reference::Poly(r)) = e {
                        called.insert(r.name.as_str());
                    }
                });
            }
            (name, called)
        })
        .collect()
}
