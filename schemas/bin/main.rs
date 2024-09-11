use std::{fs::File, path::Path};

use powdr_ast::analyzed::Analyzed;
use powdr_number::{Bn254Field, GoldilocksField};
use schemars::schema::RootSchema;

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

pub fn run() -> Result<(), String> {
    let current_bn254_schema = Analyzed::<Bn254Field>::get_struct_schema();
    let current_goldilocks_schema = Analyzed::<GoldilocksField>::get_struct_schema();

    let output_dir = Path::new("files");
    let bn254_path = output_dir.join("bn254.schema");
    let goldilocks_path = output_dir.join("goldilocks.schema");

    let old_bn254schema: RootSchema = serde_cbor::from_reader(
        File::open(&bn254_path).map_err(|e| format!("Failed to open bn254.schema: {e}"))?,
    )
    .map_err(|e| format!("Failed to deserialize bn254.schema: {e}"))?;

    let old_goldilocks_schema: RootSchema = serde_cbor::from_reader(
        File::open(&goldilocks_path)
            .map_err(|e| format!("Failed to open goldilocks.schema: {e}"))?,
    )
    .map_err(|e| format!("Failed to deserialize goldilocks.schema: {e}"))?;

    if old_bn254schema.schema != current_bn254_schema.schema
        || old_goldilocks_schema.schema != current_goldilocks_schema.schema
    {
        //bump the version number
        let version_path = Path::new("analyzed_type.version");
        let version = std::fs::read_to_string(version_path)
            .map_err(|e| format!("Failed to read version number: {e}"))?
            .trim()
            .parse::<u32>()
            .map_err(|e| format!("Failed to parse version number: {e}"))?;

        let new_version = version + 1;

        std::fs::write(version_path, new_version.to_string())
            .map_err(|e| format!("Failed to write version number: {e}"))?;

        serde_cbor::to_writer(
            &mut File::create(bn254_path)
                .map_err(|e| format!("Failed to open bn254.schema: {e}"))?,
            &current_bn254_schema,
        )
        .map_err(|e| format!("Failed to write bn254.schema: {e}"))?;
        serde_cbor::to_writer(
            &mut File::create(goldilocks_path)
                .map_err(|e| format!("Failed to open goldilocks.schema: {e}"))?,
            &current_goldilocks_schema,
        )
        .map_err(|e| format!("Failed to write goldilocks.schema: {e}"))?;

        println!("Updated schemas to version {new_version}");
    } else {
        println!("No changes to schemas");
    }

    Ok(())
}
