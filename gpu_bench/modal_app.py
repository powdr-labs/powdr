"""Modal app for the GPU portion of powdr's nightly reth benchmark.

The GitHub Actions CI job does CPU work locally — running
`./run.sh --apc N --mode compile` for each non-zero APC count, tarring
the resulting `apc-cache/`, and uploading the tars to a Modal Volume
under `/<run_id>/apc-cache-<apc>.tgz`. It then invokes `prove_block`
on a Modal L4 once per APC count. The function:

  - clones powdr at the requested SHA and openvm-eth at the pinned ref
  - applies the same `.cargo/config.toml` patch as
    `.github/actions/patch-openvm-eth`
  - restores the matching apc-cache from the Volume
  - runs `./run.sh --cuda --apc N --block B --mode prove-stark`
  - writes `metrics.json` to `/<run_id>/out-<apc>/` on the Volume

CI then `modal volume get`s the outputs and runs the existing
post-processing scripts (plot_trace_cells.py, basic_metrics.py) locally.
"""

from __future__ import annotations

import os

import modal

# Versions match .github/workflows/nightly-tests.yml.
HOST_RUST = "1.91.1"
GUEST_RUST = "nightly-2026-01-18"
OPENVM_TAG = "v2.0.0-beta.2-powdr"
# CUDA 12.6+ avoids an nvcc internal compiler error in
# openvm-rv32im-circuit's cuda/src/load_sign_extend.cu (gen_member_selector_for_builtin_offsetof
# assertion in cp_gen_be.c) seen on 12.4.1.
CUDA_IMAGE = "nvidia/cuda:12.6.3-devel-ubuntu22.04"

# Mirrors `.github/actions/patch-openvm-eth/action.yml`.
PATCH_CONFIG = """[patch."https://github.com/powdr-labs/powdr.git"]
powdr-openvm-riscv = { path = "../openvm-riscv" }
powdr-openvm = { path = "../openvm" }
powdr-riscv-elf = { path = "../riscv-elf" }
powdr-number = { path = "../number" }
powdr-autoprecompiles = { path = "../autoprecompiles" }
powdr-openvm-riscv-hints-circuit = { path = "../openvm-riscv/extensions/hints-circuit" }
"""


image = (
    modal.Image.from_registry(CUDA_IMAGE, add_python="3.11")
    .apt_install(
        "git",
        "curl",
        "build-essential",
        "pkg-config",
        "libssl-dev",
        "ca-certificates",
        "clang",
        "lld",
    )
    .run_commands(
        "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs "
        "| sh -s -- -y --default-toolchain none --no-modify-path",
    )
    .env(
        {
            "PATH": (
                "/root/.cargo/bin:/usr/local/cuda/bin:/usr/local/sbin:"
                "/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
            ),
        }
    )
    .run_commands(
        f"rustup toolchain install {HOST_RUST}",
        f"rustup toolchain install {GUEST_RUST} --component rust-src",
        (
            f"cargo +{HOST_RUST} install --git https://github.com/powdr-labs/openvm.git "
            f"--tag {OPENVM_TAG} cargo-openvm"
        ),
    )
)


app = modal.App("powdr-nightly-gpu", image=image)
vol = modal.Volume.from_name("powdr-nightly-gpu-cache", create_if_missing=True)


@app.function(
    # L40S = datacenter 4090: same Ada Lovelace, ~864 GB/s bandwidth
    # (vs L4's 300 GB/s and the 4090's 1008 GB/s). Best apples-to-apples
    # match for the existing self-hosted RTX 4090 numbers.
    gpu="L40S",
    # Memory in MiB. The default for Modal GPU containers (~32 GiB) is too
    # tight for the upfront PGO basic-block analysis, which holds ~110k
    # blocks in memory before proving even starts. 64 GiB gives headroom.
    memory=65536,
    volumes={"/cache": vol},
    timeout=3 * 3600,
)
def prove_block(
    apc: int,
    block: int,
    run_id: str,
    powdr_sha: str,
    openvm_eth_ref: str,
) -> None:
    import shutil
    import subprocess

    work = f"/tmp/work-{run_id}-{apc}"
    if os.path.exists(work):
        shutil.rmtree(work)
    os.makedirs(work, exist_ok=True)

    powdr = f"{work}/powdr"
    eth = f"{powdr}/openvm-eth"

    subprocess.run(
        ["git", "clone", "https://github.com/powdr-labs/powdr.git", powdr],
        check=True,
    )
    subprocess.run(["git", "checkout", powdr_sha], cwd=powdr, check=True)
    subprocess.run(
        ["git", "submodule", "update", "--init", "--recursive"],
        cwd=powdr,
        check=True,
    )

    subprocess.run(
        ["git", "clone", "https://github.com/powdr-labs/openvm-eth.git", eth],
        check=True,
    )
    subprocess.run(["git", "checkout", openvm_eth_ref], cwd=eth, check=True)

    cargo_dir = f"{eth}/.cargo"
    os.makedirs(cargo_dir, exist_ok=True)
    with open(f"{cargo_dir}/config.toml", "w") as f:
        f.write(PATCH_CONFIG)

    rpc_tgz = f"/cache/{run_id}/rpc-cache.tgz"
    if not os.path.exists(rpc_tgz):
        # CI prefetches the block on the CPU runner; missing here means the
        # upload step is broken. Fail loudly rather than try to live-fetch.
        raise FileNotFoundError(f"rpc-cache.tgz missing on the volume: {rpc_tgz}")
    subprocess.run(["tar", "-xzf", rpc_tgz, "-C", eth], check=True)
    print(f"[modal] restored rpc-cache from {rpc_tgz}")

    # One apc-cache.tgz covers every apc count: the compile step on CI ran
    # `--mode compile` for each apc back-to-back, accumulating apc-specific
    # bin files in the same dir.
    cache_tgz = f"/cache/{run_id}/apc-cache.tgz"
    if not os.path.exists(cache_tgz):
        raise FileNotFoundError(f"apc-cache.tgz missing on the volume: {cache_tgz}")
    subprocess.run(["tar", "-xzf", cache_tgz, "-C", eth], check=True)
    print(f"[modal] restored apc-cache from {cache_tgz}")

    # Patch run.sh so it never reaches for the RPC, since the cache covers
    # every block we'll ask for and the workspace has no real RPC URL set.
    # Two changes:
    #   1. Drop the upfront `if [[ -z "${RPC_1:-}" ]]; then exit 1` check —
    #      we don't set RPC_1 and don't want run.sh to bail.
    #   2. Replace `--rpc-url $RPC_1` with `--chain-id 1` in BIN_ARGS so the
    #      binary doesn't call provider.get_chain_id() at startup
    #      (cli.rs:45 in reth-benchmark) and doesn't get a malformed
    #      `--rpc-url` arg from the empty $RPC_1 expansion.
    runsh = f"{eth}/run.sh"
    with open(runsh) as f:
        contents = f.read()
    patched = contents.replace(
        'if [[ -z "${RPC_1:-}" ]]; then\n'
        '    echo "Missing RPC endpoint: set RPC_1 env var or create reth-bench/.env with RPC_1=..." >&2\n'
        '    exit 1\n'
        'fi\n',
        '',
    ).replace(
        '--rpc-url $RPC_1 \\\n--cache-dir rpc-cache"',
        '--cache-dir rpc-cache \\\n--chain-id 1"',
    )
    if patched == contents:
        raise RuntimeError("could not patch run.sh — has the upstream changed?")
    with open(runsh, "w") as f:
        f.write(patched)

    # Don't override CARGO_TARGET_DIR — run.sh hardcodes relative `target/...`
    # paths (e.g. `cp target/riscv32im-risc0-zkvm-elf/release/...` after the
    # guest build) that break when cargo's output is redirected. Each Modal
    # invocation does a full cargo build; the guest build is ~2 min and the
    # host build a few more, acceptable for a nightly cadence.
    subprocess.run(
        [
            "bash",
            "./run.sh",
            "--cuda",
            "--block",
            str(block),
            "--apc",
            str(apc),
            "--mode",
            "prove-stark",
        ],
        cwd=eth,
        check=True,
    )

    out_dir = f"/cache/{run_id}/out-{apc}"
    os.makedirs(out_dir, exist_ok=True)
    metrics_src = f"{eth}/metrics.json"
    if not os.path.exists(metrics_src):
        raise FileNotFoundError(f"metrics.json not found at {metrics_src}")
    shutil.copy(metrics_src, f"{out_dir}/metrics.json")

    vol.commit()
    print(f"[modal] saved {out_dir}/metrics.json")
