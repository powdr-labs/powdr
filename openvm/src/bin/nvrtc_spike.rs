// Phase 0 NVRTC spike: confirm an NVRTC-compiled kernel can launch against
// memory allocated by openvm-cuda-common's DeviceBuffer (cudaMallocAsync /
// VPMM) and that we can read back correct values from the host.
//
// Run with:
//     cargo run -p powdr-openvm --features cuda --bin nvrtc_spike --release

#[cfg(not(feature = "cuda"))]
fn main() {
    eprintln!("nvrtc_spike requires the `cuda` feature. Re-run with --features cuda.");
    std::process::exit(2);
}

#[cfg(feature = "cuda")]
fn main() {
    use openvm_cuda_common::{copy::MemCopyD2H, d_buffer::DeviceBuffer};
    use powdr_openvm::cuda_abi::powdr_nvrtc_spike_run_noop;

    const N: usize = 4096;

    let d_buf: DeviceBuffer<u32> = DeviceBuffer::with_capacity(N);
    d_buf.fill_zero().expect("fill_zero failed");

    let rc = unsafe { powdr_nvrtc_spike_run_noop(d_buf.as_mut_ptr(), N as i32) };
    assert_eq!(
        rc, 0,
        "powdr_nvrtc_spike_run_noop failed with code {} (see stderr for NVRTC/CUDA error)",
        rc
    );

    let host: Vec<u32> = d_buf.to_host().expect("D2H copy failed");
    assert_eq!(host.len(), N, "host vec length mismatch");
    let bad: Vec<(usize, u32)> = host
        .iter()
        .enumerate()
        .filter(|(_, v)| **v != 0x1234_5678u32)
        .take(8)
        .map(|(i, v)| (i, *v))
        .collect();
    assert!(
        bad.is_empty(),
        "NVRTC kernel produced wrong values; first mismatches: {:?}",
        bad
    );

    println!(
        "Phase 0 spike OK — NVRTC-compiled kernel wrote 0x12345678 into {} cells of a DeviceBuffer<u32>.",
        N
    );
}
