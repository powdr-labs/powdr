use powdr_ast::{
    object::MachineInstanceGraph,
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

/// Link the objects into a single PIL file, using the specified parameters
pub fn link(graph: MachineInstanceGraph, params: LinkerParams) -> Result<PILFile, Vec<String>> {
    match params.mode {
        LinkerMode::Native => native::Linker::link(graph, params.degree_mode),
        LinkerMode::Bus => bus::Linker::link(graph, params.degree_mode),
    }
}

/// A trait for linker backends
pub trait LinkerBackend {
    fn link(graph: MachineInstanceGraph, degree_mode: DegreeMode) -> Result<PILFile, Vec<String>>;
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
