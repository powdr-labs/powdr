use crate::file_writer::BBFiles;

pub trait ArithmetizationBuilder {
    fn create_arith_hpp(&mut self, name: &str, num_cols: usize);
}

impl ArithmetizationBuilder for BBFiles {
    // We have no selectors so we can easily create a boilerplate file
    fn create_arith_hpp(&mut self, name: &str, num_cols: usize) {
        let arith = format!(
            "
#pragma once
#include \"barretenberg/proof_system/arithmetization/arithmetization.hpp\"
namespace arithmetization {{
    class {name}Arithmetization : public Arithmetization<{num_cols}, 0> {{
        public:
            using FF = barretenberg::fr;
            struct Selectors {{}};
    }};
}} // namespace arithmetization
"
        );
        self.arith_hpp = Some(arith);
    }
}
