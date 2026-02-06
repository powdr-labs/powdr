use criterion::{black_box, criterion_group, criterion_main, Criterion};
use powdr_autoprecompiles::{
    bus_map::BusMap, export::ApcWithBusMap, optimizer::optimize, symbolic_machine::SymbolicMachine,
    ColumnAllocator,
};
use powdr_number::BabyBearField;
use powdr_openvm::{
    bus_map::default_openvm_bus_map, memory_bus_interaction::OpenVmMemoryBusInteraction,
    DEFAULT_DEGREE_BOUND,
};
use powdr_openvm_bus_interaction_handler::OpenVmBusInteractionHandler;

/// Benching the `test_optimize` test
fn optimize_keccak_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimize-keccak");
    group.sample_size(10);

    let file = std::fs::File::open("tests/keccak_apc_pre_opt.json.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    todo!();
    // let apc: ApcWithBusMap<TestApc, BusMap<TestBusType>> = serde_json::from_reader(reader).unwrap();

    // group.bench_function("optimize", |b| {
    //     b.iter_batched(
    //         || {
    //             (
    //                 apc.machine.clone(),
    //                 ColumnAllocator::from_max_poly_id_of_machine(&apc.machine),
    //             )
    //         },
    //         |(machine, column_allocator)| {
    //             optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
    //                 black_box(machine),
    //                 OpenVmBusInteractionHandler::default(),
    //                 DEFAULT_DEGREE_BOUND,
    //                 &default_openvm_bus_map(),
    //                 column_allocator,
    //                 &mut Default::default(),
    //             )
    //             .unwrap()
    //         },
    //         criterion::BatchSize::SmallInput,
    //     );
    // });
    // group.finish();
}

criterion_group!(benches, optimize_keccak_benchmark);
criterion_main!(benches);
