#![no_std]

use revm::{
    db::{CacheDB, EmptyDB},
    primitives::{
        address, b256, ruint::Uint, AccountInfo, Address, Bytecode, Bytes, TransactTo, B256, U256,
    },
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

#[no_mangle]
fn main() {
    const CONTRACT_ADDR: Address = address!("0d4a11d5EEaaC28EC3F61d100daF4d40471f1852");
    const CODE_HASH: B256 =
        b256!("e3c84e69bac71c159b2ff0d62b9a5c231887a809a96cb4a262a4b96ed78a1db2");
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
    evm.env.tx.caller = Address::from_slice(&[0; 20]);
    // account you want to transact with
    evm.env.tx.transact_to = TransactTo::Call(CONTRACT_ADDR);
    // calldata formed via abigen
    evm.env.tx.data = Bytes::new();
    // transaction value in wei
    evm.env.tx.value = U256::try_from(0).unwrap();

    match result.result {
        revm::primitives::ExecutionResult::Success {
            reason: _,
            gas_used: _,
            gas_refunded: _,
            logs: _,
            output,
        } => print!("Success: {:#?}", output.into_data()),
        revm::primitives::ExecutionResult::Revert {
            gas_used: _,
            output: _,
        } => panic!("Revert!"),
        revm::primitives::ExecutionResult::Halt {
            reason: _,
            gas_used: _,
        } => panic!("Halt!"),
    };
}
