#!/bin/bash
set -e

# Install the same OpenVM rev used by powdr.
cargo install --git 'http://github.com/powdr-labs/openvm.git' --rev 815b3cc cargo-openvm

cd bin/client-eth
RUSTFLAGS="-Clink-arg=--emit-relocs" cargo openvm build --no-transpile
mkdir -p ../host/elf
SRC="target/riscv32im-risc0-zkvm-elf/release/openvm-client-eth"
DEST="../host/elf/openvm-client-eth"

if [ ! -f "$DEST" ] || ! cmp -s "$SRC" "$DEST"; then
  cp "$SRC" "$DEST"
fi
cd ../..

mkdir -p rpc-cache
source .env
# MODE=execute # can be compile, execute, tracegen, prove-app, prove-stark, or prove-evm
PROFILE="release"
FEATURES="bench-metrics,nightly-features,jemalloc"
BLOCK_NUMBER=21882667

arch=$(uname -m)
case $arch in
arm64 | aarch64)
  RUSTFLAGS="-Ctarget-cpu=native"
  ;;
x86_64 | amd64)
  RUSTFLAGS="-Ctarget-cpu=native -C target-feature=+avx512f"
  ;;
*)
  echo "Unsupported architecture: $arch"
  exit 1
  ;;
esac
export JEMALLOC_SYS_WITH_MALLOC_CONF="retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:-1,muzzy_decay_ms:-1,abort_conf:true"
RUSTFLAGS=$RUSTFLAGS cargo build --bin openvm-reth-benchmark-bin --profile=$PROFILE --no-default-features --features=$FEATURES
PARAMS_DIR="params"

# Default options if not set
: "${MODE:=execute}"
: "${APC:=0}"
: "${APC_SKIP:=0}"
: "${PGO_TYPE:=cell}"

RUST_LOG="debug" OUTPUT_PATH="metrics.json" ./target/$PROFILE/openvm-reth-benchmark-bin \
  --kzg-params-dir "$PARAMS_DIR" \
  --mode "$MODE" \
  --block-number "$BLOCK_NUMBER" \
  --rpc-url "$RPC_1" \
  --cache-dir "rpc-cache" \
  --apc "$APC" \
  --apc-skip "$APC_SKIP" \
  --pgo-type "$PGO_TYPE"
