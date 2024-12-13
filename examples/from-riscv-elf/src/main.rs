use powdr::Session;

fn main() {
    env_logger::init();

    let mut session = Session::builder()
        .riscv_elf_path("../../zeam-poc")
        // Replace the line above with the one below to re-use the powdr-asm file
        // and not recompile anything.
        //.asm_file("powdr-target/zeam-poc.asm")
        .out_path("powdr-target")
        .chunk_size_log2(18)
        .build();

    // Fast dry run to test execution.
    session.run();

    // Uncomment to compute the proof.
    session.prove();

    let publics = session.publics();
    assert_eq!(publics, [2241840989, 885481369, 237668841, 1131259817, 3884368753, 417381428, 1709797765, 1607761401]);
}
