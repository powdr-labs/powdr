use std::collections::HashSet;

use powdr_ast::asm_analysis::AnalysisASMFile;

pub fn optimize(mut analyzed_asm: AnalysisASMFile) -> AnalysisASMFile {
    remove_unused_instructions(&mut analyzed_asm);
    analyzed_asm
}

fn remove_unused_instructions(asm: &mut AnalysisASMFile) {
    for m in asm.modules.values_mut() {
        println!("{:?}", m);
    }
}
