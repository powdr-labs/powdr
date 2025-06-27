use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

use alloy_provider::{network::Ethereum, Provider};
use alloy_rpc_types::BlockId;
use reth_revm::{
    primitives::{Address, HashMap, B256, U256},
    state::{AccountInfo, Bytecode},
    DatabaseRef,
};
use reth_storage_errors::{db::DatabaseError, provider::ProviderError};

/// A database that fetches data from a [Provider].
#[derive(Debug, Clone)]
pub struct RpcDb<P> {
    /// The provider which fetches data.
    pub provider: P,
    /// The block to fetch data from.
    pub block: BlockId,
    /// The cached accounts.
    pub accounts: RefCell<HashMap<Address, AccountInfo>>,
    /// The cached storage values.
    pub storage: RefCell<HashMap<Address, HashMap<U256, U256>>>,
    /// The oldest block whose header/hash has been requested.
    pub oldest_ancestor: RefCell<u64>,
}

/// Errors that can occur when interacting with the [RpcDb].
#[derive(Debug, Clone, thiserror::Error)]
pub enum RpcDbError {
    #[error("failed to fetch data: {0}")]
    RpcError(String),
    #[error("failed to find block")]
    BlockNotFound,
    #[error("failed to find trie node preimage")]
    PreimageNotFound,
}

impl<P: Provider<Ethereum> + Clone> RpcDb<P> {
    /// Create a new [`RpcDb`].
    pub fn new(provider: P, block: u64) -> Self {
        RpcDb {
            provider,
            block: block.into(),
            accounts: RefCell::new(HashMap::default()),
            storage: RefCell::new(HashMap::default()),
            oldest_ancestor: RefCell::new(block),
        }
    }

    /// Fetch the [AccountInfo] for an [Address].
    pub async fn fetch_account_info(&self, address: Address) -> Result<AccountInfo, RpcDbError> {
        tracing::info!("fetching account info for address: {}", address);

        // Fetch the proof for the account.
        let proof = self
            .provider
            .get_proof(address, vec![])
            .block_id(self.block)
            .await
            .map_err(|e| RpcDbError::RpcError(e.to_string()))?;

        // Fetch the code of the account.
        let code = self
            .provider
            .get_code_at(address)
            .block_id(self.block)
            .await
            .map_err(|e| RpcDbError::RpcError(e.to_string()))?;

        // Construct the account info & write it to the log.
        let bytecode = Bytecode::new_raw(code);
        let account_info = AccountInfo {
            nonce: proof.nonce,
            balance: proof.balance,
            code_hash: proof.code_hash,
            code: Some(bytecode.clone()),
        };

        // Record the account info to the state.
        self.accounts.borrow_mut().insert(address, account_info.clone());

        Ok(account_info)
    }

    /// Fetch the storage value at an [Address] and [U256] index.
    pub async fn fetch_storage_at(
        &self,
        address: Address,
        index: U256,
    ) -> Result<U256, RpcDbError> {
        tracing::info!("fetching storage value at address: {}, index: {}", address, index);

        // Fetch the storage value.
        let value = self
            .provider
            .get_storage_at(address, index)
            .block_id(self.block)
            .await
            .map_err(|e| RpcDbError::RpcError(e.to_string()))?;

        // Record the storage value to the state.
        let mut storage_values = self.storage.borrow_mut();
        let entry = storage_values.entry(address).or_default();
        entry.insert(index, value);

        Ok(value)
    }

    /// Fetch the block hash for a block number.
    pub async fn fetch_block_hash(&self, number: u64) -> Result<B256, RpcDbError> {
        tracing::info!("fetching block hash for block number: {}", number);

        // Fetch the block.
        let block = self
            .provider
            .get_block_by_number(number.into())
            .await
            .map_err(|e| RpcDbError::RpcError(e.to_string()))?;

        // Record the block hash to the state.
        let block = block.ok_or(RpcDbError::BlockNotFound)?;
        let hash = block.header.hash;

        let mut oldest_ancestor = self.oldest_ancestor.borrow_mut();
        *oldest_ancestor = number.min(*oldest_ancestor);

        Ok(hash)
    }

    /// Gets all the state keys used. The client uses this to read the actual state data from tries.
    pub fn get_state_requests(&self) -> HashMap<Address, Vec<U256>> {
        let accounts = self.accounts.borrow();
        let storage = self.storage.borrow();

        accounts
            .keys()
            .chain(storage.keys())
            .map(|&address| {
                let storage_keys_for_address: BTreeSet<U256> = storage
                    .get(&address)
                    .map(|storage_map| storage_map.keys().cloned().collect())
                    .unwrap_or_default();

                (address, storage_keys_for_address.into_iter().collect())
            })
            .collect()
    }

    /// Gets all account bytecodes.
    pub fn get_bytecodes(&self) -> Vec<Bytecode> {
        let accounts = self.accounts.borrow();

        accounts
            .values()
            .flat_map(|account| account.code.clone())
            .map(|code| (code.hash_slow(), code))
            .collect::<BTreeMap<_, _>>()
            .into_values()
            .collect::<Vec<_>>()
    }
}

impl<P: Provider<Ethereum> + Clone> DatabaseRef for RpcDb<P> {
    type Error = ProviderError;

    fn basic_ref(&self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let handle = tokio::runtime::Handle::try_current().map_err(|_| {
            ProviderError::Database(DatabaseError::Other("no tokio runtime found".to_string()))
        })?;
        let result =
            tokio::task::block_in_place(|| handle.block_on(self.fetch_account_info(address)));
        let account_info =
            result.map_err(|e| ProviderError::Database(DatabaseError::Other(e.to_string())))?;
        Ok(Some(account_info))
    }

    fn code_by_hash_ref(&self, _code_hash: B256) -> Result<Bytecode, Self::Error> {
        unimplemented!()
    }

    fn storage_ref(&self, address: Address, index: U256) -> Result<U256, Self::Error> {
        let handle = tokio::runtime::Handle::try_current().map_err(|_| {
            ProviderError::Database(DatabaseError::Other("no tokio runtime found".to_string()))
        })?;
        let result =
            tokio::task::block_in_place(|| handle.block_on(self.fetch_storage_at(address, index)));
        let value =
            result.map_err(|e| ProviderError::Database(DatabaseError::Other(e.to_string())))?;
        Ok(value)
    }

    fn block_hash_ref(&self, number: u64) -> Result<B256, Self::Error> {
        let handle = tokio::runtime::Handle::try_current().map_err(|_| {
            ProviderError::Database(DatabaseError::Other("no tokio runtime found".to_string()))
        })?;
        let result = tokio::task::block_in_place(|| handle.block_on(self.fetch_block_hash(number)));
        let value =
            result.map_err(|e| ProviderError::Database(DatabaseError::Other(e.to_string())))?;
        Ok(value)
    }
}
