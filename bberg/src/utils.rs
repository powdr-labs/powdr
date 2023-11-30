/// Get Relations Imports
///
/// We may have multiple relation files in the generated foler
/// This method will return all of the imports for the relation header files
pub fn get_relations_imports(name: &str, relations: &[String]) -> String {
    relations
        .iter()
        .map(|relation_name| {
            format!("#include \"barretenberg/relations/generated/{name}/{relation_name}.hpp\"",)
        })
        .collect::<Vec<_>>()
        .join("\n")
}
