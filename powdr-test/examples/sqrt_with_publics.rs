use powdr_test::halo2_pipeline;

fn main() {
    env_logger::init();

    halo2_pipeline(
        "test_data/asm/sqrt_with_public.asm",
        vec![3.into()],
        vec![9.into()],
        8,
    );
}
