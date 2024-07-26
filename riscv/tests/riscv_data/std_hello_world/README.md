My best shot at compiling this so far is:

```
RUSTFLAGS="-C link-arg=-Tmemory.x -C link-arg=-Tlink.x" cargo build -Zbuild-std=std,panic_abort -r --target=riscv32im-risc0-zkvm-elf
```

But it still fails.
