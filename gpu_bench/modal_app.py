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
CUDA_IMAGE = "nvidia/cuda:12.4.1-devel-ubuntu22.04"

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

# We deliberately do NOT forward the real RPC URL. The CPU side prefetches
# every block we'll need into rpc-cache.tgz, so prove-stark inside Modal
# should be a pure consumer of the cache. If something tries to fetch live,
# we want a loud connection error here instead of a silent re-download
# burning GPU minutes.
FAKE_RPC = "http://rpc-cache-must-be-populated.invalid"


@app.function(
    gpu="L4",
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
    if os.path.exists(rpc_tgz):
        subprocess.run(["tar", "-xzf", rpc_tgz, "-C", eth], check=True)
        print(f"[modal] restored rpc-cache from {rpc_tgz}")
    else:
        # No prefetch ⇒ the binary will hit the RPC inside Modal. Should
        # not happen for nightly, but harmless if it does.
        print("[modal] no rpc-cache — will fetch blocks from RPC")

    # One apc-cache.tgz covers every apc count: the compile step on CI ran
    # `--mode compile` for each non-zero apc back-to-back, accumulating
    # apc-specific bin files in the same dir. apc=0 needs nothing from it.
    cache_tgz = f"/cache/{run_id}/apc-cache.tgz"
    if os.path.exists(cache_tgz):
        subprocess.run(["tar", "-xzf", cache_tgz, "-C", eth], check=True)
        print(f"[modal] restored apc-cache from {cache_tgz}")
    elif apc != 0:
        # apc=0 doesn't need a cache; missing for non-zero apc is a CI bug.
        raise FileNotFoundError(f"apc-cache.tgz missing for apc={apc}")

    # run.sh requires RPC_1 to be set — write a sentinel that's syntactically
    # valid but unreachable, so cache misses surface as connection errors.
    with open(f"{eth}/.env", "a") as f:
        f.write(f"export RPC_1={FAKE_RPC}\n")

    # Re-use the cargo target dir across invocations on this Volume so the
    # second/third APC run skips the binary rebuild (sequential invocations
    # only — guarded by the GH workflow looping serially over apc counts).
    target_dir = f"/cache/cargo-target-{powdr_sha}"
    os.makedirs(target_dir, exist_ok=True)
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = target_dir

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
        env=env,
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
