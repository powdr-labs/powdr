#![no_std]

use revm::{
    db::{CacheDB, EmptyDB},
    primitives::{ruint::Uint, AccountInfo, Bytecode, Bytes, TransactTo, B160, B256, U256},
    EVM,
};
use runtime::print;

/*
mstore(0, 666)
return(0, 32)
*/
static BYTECODE: &[u8] = &[
    0x61, 0x02, 0x9a, 0x60, 0x00, 0x52, 0x60, 0x20, 0x60, 0x00, 0xf3,
];

static CODE_HASH: B256 = B256([
    0xe3, 0xc8, 0x4e, 0x69, 0xba, 0xc7, 0x1c, 0x15, 0x9b, 0x2f, 0xf0, 0xd6, 0x2b, 0x9a, 0x5c, 0x23,
    0x18, 0x87, 0xa8, 0x09, 0xa9, 0x6c, 0xb4, 0xa2, 0x62, 0xa4, 0xb9, 0x6e, 0xd7, 0x8a, 0x1d, 0xb2,
]);

static CONTRACT_ADDR: B160 = B160([
    0x0d, 0x4a, 0x11, 0xd5, 0xEE, 0xaa, 0xC2, 0x8E, 0xC3, 0xF6, 0x1d, 0x10, 0x0d, 0xaF, 0x4d, 0x40,
    0x47, 0x1f, 0x18, 0x52,
]);

#[no_mangle]
fn main() {
    let mut db = CacheDB::new(EmptyDB::default());

    // Fill database:
    let bytecode = Bytes::from_static(BYTECODE);
    let account = AccountInfo::new(Uint::from(10), 0, CODE_HASH, Bytecode::new_raw(bytecode));

    db.insert_account_info(CONTRACT_ADDR, account);

    let mut evm: EVM<CacheDB<EmptyDB>> = EVM::new();
    evm.database(db);

    let result = evm.transact().unwrap();

    // fill in missing bits of env struc
    // change that to whatever caller you want to be
    evm.env.tx.caller = B160::from_slice(&[0; 20]);
    // account you want to transact with
    evm.env.tx.transact_to = TransactTo::Call(CONTRACT_ADDR);
    // calldata formed via abigen
    evm.env.tx.data = Bytes::new();
    // transaction value in wei
    evm.env.tx.value = U256::try_from(0).unwrap();

    match result.result {
        revm::primitives::ExecutionResult::Success {
            reason,
            gas_used,
            gas_refunded,
            logs,
            output,
        } => print!("Success: {:#?}", output.into_data()),
        revm::primitives::ExecutionResult::Revert { gas_used, output } => panic!("Revert!"),
        revm::primitives::ExecutionResult::Halt { reason, gas_used } => panic!("Halt!"),
    };
}
