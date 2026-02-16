use criterion::{black_box, criterion_group, criterion_main, Criterion};
use powdr_autoprecompiles::{
    bus_map::BusMap,
    export::{ApcWithBusMap, SimpleInstruction},
    optimizer::optimize,
    Apc, ColumnAllocator, DegreeBound,
};
use powdr_number::BabyBearField;

use powdr_openvm_bus_interaction_handler::{
    bus_map::OpenVmBusType, memory_bus_interaction::OpenVmMemoryBusInteraction,
    OpenVmBusInteractionHandler,
};

type TestApc = Apc<BabyBearField, SimpleInstruction<BabyBearField>, (), ()>;

const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: 3,
    bus_interactions: 2,
};

/// Benching the `test_optimize` test
fn optimize_keccak_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimize-keccak");
    group.sample_size(10);

    let file = std::fs::File::open("tests/keccak_apc_pre_opt.json.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let apc: ApcWithBusMap<TestApc, BusMap<OpenVmBusType>> =
        serde_json::from_reader(reader).unwrap();

    group.bench_function("optimize", |b| {
        b.iter_batched(
            || {
                (
                    apc.apc.machine.clone(),
                    ColumnAllocator::from_max_poly_id_of_machine(&apc.apc.machine),
                )
            },
            |(machine, column_allocator)| {
                optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
                    black_box(machine),
                    OpenVmBusInteractionHandler::default(),
                    DEFAULT_DEGREE_BOUND,
                    &apc.bus_map,
                    column_allocator,
                    &mut Default::default(),
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
