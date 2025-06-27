/// Client program input data types.
pub mod io;
#[macro_use]
mod utils;

use std::{fmt::Debug, sync::Arc};

use alloy_consensus::TxReceipt;
use alloy_primitives::Bloom;
use io::ClientExecutorInput;
#[allow(unused_imports)]
pub use openvm_mpt;
use openvm_mpt::state::HashedPostState;
use openvm_primitives::chain_spec::mainnet;
use reth_consensus::{Consensus, HeaderValidator};
use reth_ethereum_consensus::{validate_block_post_execution, EthBeaconConsensus};
use reth_evm::execute::{BasicBlockExecutor, Executor};
use reth_evm_ethereum::EthEvmConfig;
use reth_execution_types::ExecutionOutcome;
#[allow(unused_imports)]
pub use reth_primitives;
use reth_primitives::Header;
use reth_primitives_traits::block::Block as _;
use reth_revm::db::CacheDB;

/// Chain ID for Ethereum Mainnet.
pub const CHAIN_ID_ETH_MAINNET: u64 = 0x1;

/// Chain ID for OP Mainnnet.
pub const CHAIN_ID_OP_MAINNET: u64 = 0xa;

/// Chain ID for Linea Mainnet.
pub const CHAIN_ID_LINEA_MAINNET: u64 = 0xe708;

/// An executor that executes a block inside a zkVM.
#[derive(Debug, Clone, Default)]
pub struct ClientExecutor;

/// Implementation for Ethereum-specific execution/validation logic.
#[derive(Debug)]
pub struct EthereumVariant;

/// EVM chain variants that implement different execution/validation rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChainVariant {
    /// Ethereum networks.
    Ethereum,
}

impl ClientExecutor {
    pub fn execute(&self, mut input: ClientExecutorInput) -> eyre::Result<Header> {
        // Initialize the witnessed database with verified storage proofs.
        let witness_db = input.witness_db()?;
        let cache_db = CacheDB::new(&witness_db);

        // Execute the block.
        let spec = Arc::new(mainnet());
        let current_block =
            profile!("recover senders", { input.current_block.clone().try_into_recovered() })?;

        // validate the block pre-execution
        profile!("validate block pre-execution", {
            let consensus = EthBeaconConsensus::new(spec.clone());

            consensus
                .validate_header(current_block.sealed_header())
                .expect("failed to validate header");

            consensus
                .validate_block_pre_execution(&current_block)
                .expect("failed to validate block pre-execution");
        });

        let block_executor = BasicBlockExecutor::new(EthEvmConfig::new(spec.clone()), cache_db);
        let executor_output = profile!("execute", { block_executor.execute(&current_block) })?;

        // Validate the block post execution.
        profile!("validate block post-execution", {
            validate_block_post_execution(
                &current_block,
                &spec,
                &executor_output.receipts,
                &executor_output.requests,
            )
        })?;

        // Accumulate the logs bloom.
        let mut logs_bloom = Bloom::default();
        profile!("accrue logs bloom", {
            executor_output.receipts.iter().for_each(|r| {
                logs_bloom.accrue_bloom(&r.bloom());
            })
        });

        // Convert the output to an execution outcome.
        let executor_outcome = ExecutionOutcome::new(
            executor_output.state,
            vec![executor_output.result.receipts],
            input.current_block.header.number,
            vec![executor_output.result.requests],
        );

        // Verify the state root.
        let state_root = profile!("compute state root", {
            let post_state = HashedPostState::from_bundle_state(&executor_outcome.bundle.state);
            // executor_outcome.hash_state_slow());
            println!("post state from bundle state: done");
            input.parent_state.update(&post_state);
            input.parent_state.state_root()
        });

        if state_root != input.current_block.state_root {
            eyre::bail!("mismatched state root");
        }

        // Derive the block header.
        //
        // Note: the receipts root and gas used are verified by `validate_block_post_execution`.
        let mut header = input.current_block.header.clone();
        header.parent_hash = input.parent_header().hash_slow();
        header.ommers_hash = input.current_block.body.calculate_ommers_root();
        header.state_root = input.current_block.state_root;
        header.transactions_root = input.current_block.transactions_root;
        header.receipts_root = input.current_block.header.receipts_root;
        header.withdrawals_root = input.current_block.body.calculate_withdrawals_root();
        header.logs_bloom = logs_bloom;
        header.requests_hash = input.current_block.requests_hash;

        Ok(header)
    }
}
