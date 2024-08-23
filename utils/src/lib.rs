use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// Take a set of nodes and a function returning the dependencies of any node and return a topological order where any node always comes after its dependencies
/// Circular dependencies are returned in an arbitrary order
pub fn topo_sort<'a, E, F, I>(nodes: I, get_dependencies: F) -> Vec<&'a E>
where
    F: Fn(&E) -> HashSet<&'a E>,
    E: Eq + Hash,
    I: Iterator<Item = &'a E>,
{
    let graph: HashMap<&'a E, HashSet<&'a E>> = nodes.map(|e| (e, get_dependencies(e))).collect();
    let mut visited = HashSet::new();
    let mut result = Vec::new();

    for &node in graph.keys() {
        topo_sort_visit(node, &graph, &mut visited, &mut result);
    }

    assert_eq!(graph.len(), result.len());
    result
}

fn topo_sort_visit<'a, E>(
    node: &'a E,
    graph: &HashMap<&'a E, HashSet<&'a E>>,
    visited: &mut HashSet<&'a E>,
    result: &mut Vec<&'a E>,
) where
    E: Eq + Hash,
{
    if !visited.insert(node) {
        return;
    }

    if let Some(dependencies) = graph.get(node) {
        for &dependency in dependencies {
            topo_sort_visit(dependency, graph, visited, result);
        }
    }

    result.push(node);
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::topo_sort;

    #[test]
    fn sort() {
        let nodes = [0u32, 1, 2];
        let get_dependencies = |i: &u32| {
            match i {
                0u32 => vec![&nodes[1], &nodes[2]],
                1 => vec![&nodes[2]],
                2 => vec![],
                _ => unreachable!(),
            }
            .into_iter()
            .collect()
        };
        assert_eq!(
            topo_sort(nodes.iter(), get_dependencies),
            vec![&2u32, &1, &0]
        );
    }

    #[test]
    fn cycle() {
        let nodes = [0u32, 1, 2];
        let get_dependencies = |i: &u32| {
            match i {
                0u32 => vec![&nodes[1]],
                1 => vec![&nodes[2]],
                2 => vec![&nodes[0]],
                _ => unreachable!(),
            }
            .into_iter()
            .collect()
        };
        assert_eq!(
            topo_sort(nodes.iter(), get_dependencies)
                .into_iter()
                .collect::<HashSet<_>>(),
            nodes.iter().collect::<HashSet<_>>()
        );
    }
}
