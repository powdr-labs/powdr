use std::{fs, io, path::Path};

use powdr_leanvm::symbolic_machines::build_execution_machine;
use powdr_leanvm::leanvm_bus_map;
use pretty_assertions::assert_eq;

#[test]
fn extract_machine() {
    let machine = build_execution_machine();
    let bus_map = leanvm_bus_map();
    let rendered = format!("# Execution Table\n{}", machine.render(&bus_map));

    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("leanvm_constraints.txt");
    match fs::read_to_string(&path) {
        Ok(expected) => {
            assert_eq!(rendered, expected);
        }
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
