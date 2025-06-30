use reth_chainspec::{
    BaseFeeParams, BaseFeeParamsKind, Chain, ChainHardforks, ChainSpec, DepositContract,
};
use revm_primitives::{address, b256, U256};

/// Returns the [ChainSpec] for Ethereum mainnet.
/// It is much faster than using reth's MAINNET.clone()
pub fn mainnet() -> ChainSpec {
    // Spec extracted from:
    //
    // https://github.com/paradigmxyz/reth/blob/c228fe15808c3acbf18dc3af1a03ef5cbdcda07a/crates/chainspec/src/spec.rs#L35-L60
    let mut spec = ChainSpec {
        chain: Chain::mainnet(),
        // We don't need the genesis state. Using default to save cycles.
        genesis: Default::default(),
        paris_block_and_final_difficulty: Some((0, U256::ZERO)),
        hardforks: ChainHardforks::from(alloy_hardforks::EthereumHardfork::mainnet()),
        deposit_contract: Some(DepositContract::new(
            address!("00000000219ab540356cbb839cbe05303d7705fa"),
            11052984,
            b256!("649bbc62d0e31342afea4e5cd82d4049e7e1ee912fc0889aa790803be39038c5"),
        )),
        base_fee_params: BaseFeeParamsKind::Constant(BaseFeeParams::ethereum()),
        prune_delete_limit: 20000,
        genesis_header: Default::default(),
        blob_params: Default::default(),
    };
    spec.genesis.config.dao_fork_support = true;
    spec
}
