fn main() {
    vergen::EmitBuilder::builder().git_sha(true).emit().unwrap();

    // Define features to check and display
    let features_to_check = vec!["cuda", "tco"];

    // Check which of these features are enabled
    let mut enabled_features = Vec::new();
    for feature in features_to_check {
        let env_var = format!("CARGO_FEATURE_{}", feature.to_uppercase().replace('-', "_"));
        if std::env::var(&env_var).is_ok() {
            enabled_features.push(feature);
        }
    }

    if !enabled_features.is_empty() {
        let features_str = enabled_features.join(", ");
        println!("Building cargo-openvm with features: {features_str}");
    }
}
