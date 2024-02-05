use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{Expression, Reference},
    parsed::visitor::ExpressionVisitable,
};
use powdr_number::FieldElement;

/// Returns a sorted list of symbols such that called symbols appear before the symbols that reference them.
/// Circular dependencies appear in an arbitray order.
pub fn sort_called_first<
    'a,
    T: FieldElement,
    I: Iterator<Item = (&'a str, Option<&'a Expression<T>>)>,
>(
    symbols: I,
) -> Vec<&'a str> {
    let graph = call_graph(symbols);
    let mut visited: HashSet<&str> = HashSet::new();
    let mut result: Vec<&str> = Vec::new();
    for (name, _) in graph.iter() {
        topo_sort_visit(name, &graph, &mut visited, &mut result);
    }
    assert_eq!(graph.len(), result.len());
    result
}

fn topo_sort_visit<'a, 'b>(
    name: &'a str,
    graph: &'b HashMap<&'a str, HashSet<String>>,
    visited: &'b mut HashSet<&'a str>,
    result: &'b mut Vec<&'a str>,
) {
    if !visited.insert(name) {
        return;
    }
    if let Some(called) = graph.get(name) {
        for c in called {
            let n = graph.get_key_value(c.as_str()).unwrap().0;
            topo_sort_visit(n, graph, visited, result);
        }
    }
    result.push(name);
}

fn call_graph<'a, T: FieldElement, I: Iterator<Item = (&'a str, Option<&'a Expression<T>>)>>(
    symbols: I,
) -> HashMap<&'a str, HashSet<String>> {
    symbols
        .map(|(name, expr)| {
            let mut called: HashSet<String> = HashSet::new();
            expr.map(|e| {
                e.pre_visit_expressions(&mut |e: &Expression<T>| {
                    if let Expression::Reference(Reference::Poly(r)) = e {
                        // Tried with &'a str here, but it does not really work
                        // with the lambda.
                        called.insert(r.name.clone());
                    }
                })
            });
            (name, called)
        })
        .collect()
}
