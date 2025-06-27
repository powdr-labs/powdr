use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct ExecutionReportData {
    chain_id: u64,
    block_number: u64,
    gas_used: u64,
    tx_count: usize,
    number_cycles: u64,
    number_syscalls: u64,
    bn_add_cycles: u64,
    bn_mul_cycles: u64,
    bn_pair_cycles: u64,
    kzg_point_eval_cycles: u64,
}
