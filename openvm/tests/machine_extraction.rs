use std::{fs, io, path::Path};
mod common;

use itertools::Itertools;
use powdr_openvm::DEFAULT_DEGREE_BOUND;
use pretty_assertions::assert_eq;

use crate::common::original_vm_config;

#[test]
fn extract_machine() {
    let original_config = original_vm_config();
    let airs = original_config
        .airs(DEFAULT_DEGREE_BOUND.identities)
        .unwrap();
    let bus_map = original_config.bus_map();
    let rendered = airs
        .airs_by_name()
        .map(|(machine_name, air)| format!("# {machine_name}\n{}", air.render(&bus_map)))
        .join("\n\n\n");

    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("openvm_constraints.txt");
    match fs::read_to_string(&path) {
        // Snapshot exists, compare it with the extracted constraints
        Ok(expected) => {
            assert_eq!(rendered, expected)
        }

        // Snapshot does not exist, create it
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&path, &rendered).unwrap();
            panic!("Created new snapshot at {path:?}. Inspect it, then rerun the tests.");
        }

        Err(_) => panic!(),
    }
}
