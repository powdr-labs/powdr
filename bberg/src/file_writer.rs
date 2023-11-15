use std::fs::File;
use std::io::Write;

pub struct BBFiles {
    pub relation_hpp: Option<String>,
    pub arith_hpp: Option<String>,
    pub flavor_hpp: Option<String>,
    // trace
    pub trace_hpp: Option<String>,
    // composer
    pub composer_cpp: Option<String>,
    pub composer_hpp: Option<String>,

    // prover
    pub prover_cpp: Option<String>,
    pub prover_hpp: Option<String>,

    // verifier
    pub verifier_cpp: Option<String>,
    pub verifier_hpp: Option<String>,

    // Relative paths
    pub file_name: String,
    pub base: String,
    pub rel: String,
    pub arith: String,
    pub trace: String,
    pub flavor: String,
    pub composer: String,
    pub prover: String, // path for both prover and verifier files
}

impl BBFiles {
    pub fn default(file_name: String) -> Self {
        Self::new(file_name, None, None, None, None, None, None, None)
    }

    pub fn new(
        file_name: String,
        base: Option<String>,
        rel: Option<String>,
        arith: Option<String>,
        trace: Option<String>,
        flavor: Option<String>,
        composer: Option<String>,
        prover: Option<String>,
    ) -> Self {
        let base = base.unwrap_or("src/barretenberg".to_owned());
        let rel = rel.unwrap_or("proof_system/relations/generated".to_owned());
        let arith = arith.unwrap_or("proof_system/arithmetization/generated".to_owned());
        let trace = trace.unwrap_or("proof_system/circuit_builder/generated".to_owned());
        let flavor = flavor.unwrap_or("honk/flavor/generated".to_owned());
        let composer = composer.unwrap_or("honk/composer/generated".to_owned());
        let prover = prover.unwrap_or("honk/proof_system/generated".to_owned());

        Self {
            file_name,
            relation_hpp: None,
            arith_hpp: None,
            flavor_hpp: None,
            trace_hpp: None,
            composer_cpp: None,
            composer_hpp: None,
            prover_cpp: None,
            prover_hpp: None,
            verifier_cpp: None,
            verifier_hpp: None,

            base,
            rel,
            arith,
            trace,
            flavor,
            composer,
            prover,
        }
    }

    pub fn write(&self) {
        // Helper macro codegen using the classes' write_file method
        macro_rules! write_file {
            ($location:expr, $extension:expr, $content:expr) => {
                self.write_file(
                    &$location,
                    &format!("{}{}", self.file_name, $extension),
                    &$content.clone().unwrap(),
                );
            };
        }
        write_file!(self.rel, ".hpp", self.relation_hpp);
        write_file!(self.arith, "_arith.hpp", self.arith_hpp);

        // Trace
        write_file!(self.trace, "_trace.hpp", self.trace_hpp);

        write_file!(self.flavor, "_flavor.hpp", self.flavor_hpp);

        // Composer
        write_file!(self.composer, "_composer.hpp", self.composer_hpp);
        write_file!(self.composer, "_composer.cpp", self.composer_cpp);

        // Prover
        write_file!(self.prover, "_prover.hpp", self.prover_hpp);
        write_file!(self.prover, "_prover.cpp", self.prover_cpp);

        // Verifier
        write_file!(self.prover, "_verifier.hpp", self.verifier_hpp);
        write_file!(self.prover, "_verifier.cpp", self.verifier_cpp);
    }

    fn write_file(&self, folder: &str, filename: &str, contents: &String) {
        // attempt to create dir
        let base_path = format!("{}/{}", self.base, folder);
        let _ = std::fs::create_dir_all(&base_path);

        let joined = format!("{}/{}", base_path, filename);
        println!("Writing file: {}", joined);
        let mut file = File::create(joined).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }
}
