use powdr_test::halo2_pipeline;

fn main() {
    env_logger::init();

    halo2_pipeline(
        "test_data/asm/book/hello_world.asm",
        vec![1.into()],
        vec![],
        8,
    );
}
