RUSTFLAGS="-C target-cpu=native"
JEMALLOC_SYS_WITH_MALLOC_CONF="retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:10000,muzzy_decay_ms:10000,abort_conf:true"
POWDR_OPENVM_SEGMENT_DELTA="50000"