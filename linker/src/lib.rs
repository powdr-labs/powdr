use powdr_analysis::utils::parse_pil_statement;
use powdr_ast::{
    object::{Link, Location, MachineInstanceGraph, Object},
    parsed::{asm::AbsoluteSymbolPath, PILFile, PilStatement},
};
use powdr_parser_util::SourceRef;
use std::{collections::BTreeMap, iter::once};
use strum::{Display, EnumString, EnumVariantNames};

/// A linker implementation using the bus
pub mod bus;
/// A linker implementation using native lookups and permutations
pub mod native;

const MAIN_OPERATION_NAME: &str = "main";

/// Link the objects into a single PIL file, using the specified parameters.
pub fn link(graph: MachineInstanceGraph, params: LinkerParams) -> Result<PILFile, Vec<String>> {
    match params.mode {
        LinkerMode::Native => native::NativeLinker::link(graph, params.degree_mode),
        LinkerMode::Bus => bus::BusLinker::link(graph, params.degree_mode),
    }
}

/// A trait for linker backends
pub trait LinkerBackend: Sized {
    /// Create a new linker instance from the given graph and degree mode.
    /// This can fail, for example if the linker does not support the given degree mode.
    fn try_new(graph: &MachineInstanceGraph, degree_mode: DegreeMode) -> Result<Self, Vec<String>>;

    /// Link the objects into a single PIL file.
    fn link(graph: MachineInstanceGraph, degree_mode: DegreeMode) -> Result<PILFile, Vec<String>> {
        let mut linker = Self::try_new(&graph, degree_mode)?;

        let common_definitions = process_definitions(graph.statements);

        for (location, object) in &graph.objects {
            linker.process_object(location, object.clone(), &graph.objects);

            if *location == Location::main() {
                let operation_id = object.operation_id.clone();
                let main_operation_id = object
                    .operations
                    .get(MAIN_OPERATION_NAME)
                    .and_then(|operation| operation.id.as_ref());

                if let (Some(main_operation_id), Some(operation_id)) =
                    (main_operation_id, operation_id)
                {
                    // call the main operation by initializing `operation_id` to that of the main operation
                    let linker_first_step = "_linker_first_step";
                    linker.add_to_namespace_links(
                        location,
                        parse_pil_statement(&format!(
                            "col fixed {linker_first_step}(i) {{ if i == 0 {{ 1 }} else {{ 0 }} }};"
                        )),
                    );
                    linker.add_to_namespace_links(
                        location,
                        parse_pil_statement(&format!(
                            "{linker_first_step} * ({operation_id} - {main_operation_id}) = 0;"
                        )),
                    );
                }
            }
        }

        Ok(PILFile(
            common_definitions
                .into_iter()
                .chain(linker.into_pil())
                .collect(),
        ))
    }

    /// Process an object.
    fn process_object(
        &mut self,
        location: &Location,
        object: Object,
        objects: &BTreeMap<Location, Object>,
    );

    fn add_to_namespace_links(&mut self, namespace: &Location, statement: PilStatement);

    /// Convert the linker's internal state into PIL statements.
    fn into_pil(self) -> Vec<PilStatement>;

    /// Process a link between two machines.
    fn process_link(
        &mut self,
        link: Link,
        from_namespace: &Location,
        objects: &BTreeMap<Location, Object>,
    );
}

#[derive(Clone, Copy, Default)]
pub struct LinkerParams {
    pub mode: LinkerMode,
    pub degree_mode: DegreeMode,
}

#[derive(Clone, EnumString, EnumVariantNames, Display, Copy, Default)]
/// Whether to link the machines natively or via a global bus.
pub enum LinkerMode {
    #[default]
    #[strum(serialize = "native")]
    Native,
    #[strum(serialize = "bus")]
    Bus,
}

#[derive(Clone, EnumString, EnumVariantNames, Display, Copy, Default)]
/// Whether to align the degrees of all machines to the main machine, or to use the degrees of the individual machines.
pub enum DegreeMode {
    #[strum(serialize = "monolithic")]
    Monolithic,
    #[default]
    #[strum(serialize = "vadcop")]
    Vadcop,
}

// Extract the utilities and sort them into namespaces where possible.
fn process_definitions(
    mut definitions: BTreeMap<AbsoluteSymbolPath, Vec<PilStatement>>,
) -> Vec<PilStatement> {
    // definitions at the root do not require a namespace statement, so we put them first
    let root = definitions.remove(&Default::default());

    root.into_iter()
        .flatten()
        .chain(
            definitions
                .into_iter()
                .flat_map(|(module_path, statements)| {
                    once(PilStatement::Namespace(
                        SourceRef::unknown(),
                        module_path.relative_to(&Default::default()),
                        None,
                    ))
                    .chain(statements)
                }),
        )
        .collect()
}
