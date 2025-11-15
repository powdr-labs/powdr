use criterion::{black_box, criterion_group, criterion_main, Criterion};
use powdr_autoprecompiles::{optimizer::optimize, SymbolicMachine};
use powdr_number::BabyBearField;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
    BabyBearOpenVmApcAdapter, DEFAULT_DEGREE_BOUND,
};

/// Benching the `test_optimize` test
fn optimize_keccak_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimize-keccak");
    group.sample_size(10);

    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    group.bench_function("optimize", |b| {
        b.iter_batched(
            || machine.clone(),
            |machine| {
                optimize::<BabyBearOpenVmApcAdapter>(
                    black_box(machine),
                    OpenVmBusInteractionHandler::default(),
                    DEFAULT_DEGREE_BOUND,
                    &default_openvm_bus_map(),
                    BTreeMap::new(),
                )
                .unwrap()
            },
            criterion::BatchSize::SmallInput,
        );
    });
    group.finish();
}

criterion_group!(benches, optimize_keccak_benchmark);
criterion_main!(benches);
