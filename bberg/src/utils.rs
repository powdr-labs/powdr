use number::FieldElement;

/// Get Relations Imports
///
/// We may have multiple relation files in the generated foler
/// This method will return all of the imports for the relation header files
pub fn get_relations_imports(name: &str, relations: &[String]) -> String {
    let transformation = |relation_name: &_| {
        format!("#include \"barretenberg/relations/generated/{name}/{relation_name}.hpp\"")
    };

    map_with_newline(relations, transformation)
}

/// Sanitize Names
///
/// Column titles that we get from pil contain . to distinguish which pil namespace they belong to
/// We need to replace these with _ to make them valid C++ identifiers
pub fn sanitize_name(string: &str) -> String {
    string.replace(['.', '[', ']'], "_")
}

/// Capitalize
pub fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// Map With Newline
/// This utility function is used all over the codegen pipeline
/// It takes a list, usually the names of columns in an execution trace and applies a string transformation "op"
/// to each element in the list
pub fn map_with_newline<Func>(list: &[String], op: Func) -> String
where
    Func: Fn(&String) -> String,
{
    transform_map(list, op).join("\n")
}

/// Collect Col
///
/// Transforms columns from powdr representation ( where the witnesses are linked )
/// Into a version where we just keep the columns
/// As this is all we are about
pub fn collect_col<F, Func>(list: &[(String, Vec<F>)], op: Func) -> Vec<String>
where
    F: FieldElement,
    Func: Fn(&(String, Vec<F>)) -> String,
{
    list.iter().map(op).collect::<Vec<String>>()
}

/// Transform Map
///
/// Apply a transformation to a list of strings
pub fn transform_map<Func>(list: &[String], op: Func) -> Vec<String>
where
    Func: Fn(&String) -> String,
{
    list.iter().map(op).collect::<Vec<String>>()
}

/// Flatten
///
/// Returns a flattened concatenation of the input arrays
pub fn flatten(list: &[Vec<String>]) -> Vec<String> {
    let arr = list.iter().cloned();
    arr.into_iter().flatten().collect()
}
