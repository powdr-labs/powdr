// This code is modified from the original implementation of Zeth.
//
// Reference: https://github.com/risc0/zeth
//
// Copyright 2023 RISC Zero, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#![allow(unreachable_pub)]
#![allow(dead_code)]

use alloc::boxed::Box;
use alloy_primitives::{b256, B256};
use alloy_rlp::Encodable;
use core::{
    cell::RefCell,
    cmp,
    fmt::{Debug, Write},
    iter, mem,
};
use reth_trie::AccountProof;
use revm::primitives::HashMap;
use revm_primitives::{map::DefaultHashBuilder, Address};
use serde::{Deserialize, Serialize};

use rlp::{Decodable, DecoderError, Prototype, Rlp};
use thiserror::Error as ThisError;

use anyhow::{Context, Result};

use crate::StorageTries;

use super::EthereumState;

pub trait RlpBytes {
    /// Returns the RLP-encoding.
    fn to_rlp(&self) -> Vec<u8>;
}

impl<T> RlpBytes for T
where
    T: alloy_rlp::Encodable,
{
    #[inline]
    fn to_rlp(&self) -> Vec<u8> {
        let rlp_length = self.length();
        let mut out = Vec::with_capacity(rlp_length);
        self.encode(&mut out);
        debug_assert_eq!(out.len(), rlp_length);
        out
    }
}

/// Root hash of an empty trie.
pub const EMPTY_ROOT: B256 =
    b256!("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421");

extern crate alloc;

/// Represents the Keccak-256 hash of an empty byte slice.
///
/// This is a constant value and can be used as a default or placeholder
/// in various cryptographic operations.
pub const KECCAK_EMPTY: B256 =
    b256!("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

/// Computes the Keccak-256 hash of the provided data.
///
/// This function is a thin wrapper around the Keccak256 hashing algorithm
/// and is optimized for performance.
///
/// # TODO
/// - Consider switching the return type to `B256` for consistency with other parts of the codebase.
#[inline]
pub fn keccak(data: impl AsRef<[u8]>) -> [u8; 32] {
    // TODO: Remove this benchmarking code once performance testing is complete.
    // std::hint::black_box(sha2::Sha256::digest(&data));
    *alloy_primitives::utils::keccak256(data)
}

/// Represents the root node of a sparse Merkle Patricia Trie.
///
/// The "sparse" nature of this trie allows for truncation of certain unneeded parts,
/// representing them by their node hash. This design choice is particularly useful for
/// optimizing storage. However, operations targeting a truncated part will fail and
/// return an error. Another distinction of this implementation is that branches cannot
/// store values, aligning with the construction of MPTs in Ethereum.
#[derive(Clone, Debug, Default, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct MptNode {
    /// The type and data of the node.
    pub data: MptNodeData,
    /// Cache for a previously computed reference of this node. This is skipped during
    /// serialization.
    #[serde(skip)]
    pub cached_reference: RefCell<Option<MptNodeReference>>,
}

/// Represents custom error types for the sparse Merkle Patricia Trie (MPT).
///
/// These errors cover various scenarios that can occur during trie operations, such as
/// encountering unresolved nodes, finding values in branches where they shouldn't be, and
/// issues related to RLP (Recursive Length Prefix) encoding and decoding.
#[derive(Debug, ThisError)]
pub enum Error {
    /// Triggered when an operation reaches an unresolved node. The associated `B256`
    /// value provides details about the unresolved node.
    #[error("reached an unresolved node: {0:#}")]
    NodeNotResolved(B256),
    /// Occurs when a value is unexpectedly found in a branch node.
    #[error("branch node with value")]
    ValueInBranch,
    /// Represents errors related to the RLP encoding and decoding using the `alloy_rlp`
    /// library.
    #[error("RLP error")]
    Rlp(#[from] alloy_rlp::Error),
    /// Represents errors related to the RLP encoding and decoding, specifically legacy
    /// errors.
    #[error("RLP error")]
    LegacyRlp(#[from] DecoderError),
}

/// Represents the various types of data that can be stored within a node in the sparse
/// Merkle Patricia Trie (MPT).
///
/// Each node in the trie can be of one of several types, each with its own specific data
/// structure. This enum provides a clear and type-safe way to represent the data
/// associated with each node type.
#[derive(Clone, Debug, Default, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize)]
pub enum MptNodeData {
    /// Represents an empty trie node.
    #[default]
    Null,
    /// A node that can have up to 16 children. Each child is an optional boxed [MptNode].
    Branch([Option<Box<MptNode>>; 16]),
    /// A leaf node that contains a key and a value, both represented as byte vectors.
    Leaf(Vec<u8>, Vec<u8>),
    /// A node that has exactly one child and is used to represent a shared prefix of
    /// several keys.
    Extension(Vec<u8>, Box<MptNode>),
    /// Represents a sub-trie by its hash, allowing for efficient storage of large
    /// sub-tries without storing their entire content.
    Digest(B256),
}

/// Represents the ways in which one node can reference another node inside the sparse
/// Merkle Patricia Trie (MPT).
///
/// Nodes in the MPT can reference other nodes either directly through their byte
/// representation or indirectly through a hash of their encoding. This enum provides a
/// clear and type-safe way to represent these references.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum MptNodeReference {
    /// Represents a direct reference to another node using its byte encoding. Typically
    /// used for short encodings that are less than 32 bytes in length.
    Bytes(Vec<u8>),
    /// Represents an indirect reference to another node using the Keccak hash of its long
    /// encoding. Used for encodings that are not less than 32 bytes in length.
    Digest(B256),
}

/// Provides a conversion from [MptNodeData] to [MptNode].
///
/// This implementation allows for conversion from [MptNodeData] to [MptNode],
/// initializing the `data` field with the provided value and setting the
/// `cached_reference` field to `None`.
impl From<MptNodeData> for MptNode {
    fn from(value: MptNodeData) -> Self {
        Self { data: value, cached_reference: RefCell::new(None) }
    }
}

/// Provides encoding functionalities for the `MptNode` type.
///
/// This implementation allows for the serialization of an [MptNode] into its RLP-encoded
/// form. The encoding is done based on the type of node data ([MptNodeData]) it holds.
impl Encodable for MptNode {
    /// bincode::Encodes the node into the provided `out` buffer.
    ///
    /// The encoding is done using the Recursive Length Prefix (RLP) encoding scheme. The
    /// method handles different node data types and encodes them accordingly.
    #[inline]
    fn encode(&self, out: &mut dyn alloy_rlp::BufMut) {
        match &self.data {
            MptNodeData::Null => {
                out.put_u8(alloy_rlp::EMPTY_STRING_CODE);
            }
            MptNodeData::Branch(nodes) => {
                alloy_rlp::Header { list: true, payload_length: self.payload_length() }.encode(out);
                nodes.iter().for_each(|child| match child {
                    Some(node) => node.reference_encode(out),
                    None => out.put_u8(alloy_rlp::EMPTY_STRING_CODE),
                });
                // in the MPT reference, branches have values so always add empty value
                out.put_u8(alloy_rlp::EMPTY_STRING_CODE);
            }
            MptNodeData::Leaf(prefix, value) => {
                alloy_rlp::Header { list: true, payload_length: self.payload_length() }.encode(out);
                prefix.as_slice().encode(out);
                value.as_slice().encode(out);
            }
            MptNodeData::Extension(prefix, node) => {
                alloy_rlp::Header { list: true, payload_length: self.payload_length() }.encode(out);
                prefix.as_slice().encode(out);
                node.reference_encode(out);
            }
            MptNodeData::Digest(digest) => {
                digest.encode(out);
            }
        }
    }

    /// Returns the length of the encoded node in bytes.
    ///
    /// This method calculates the length of the RLP-encoded node. It's useful for
    /// determining the size requirements for storage or transmission.
    #[inline]
    fn length(&self) -> usize {
        let payload_length = self.payload_length();
        payload_length + alloy_rlp::length_of_length(payload_length)
    }
}

/// Provides decoding functionalities for the [MptNode] type.
///
/// This implementation allows for the deserialization of an RLP-encoded [MptNode] back
/// into its original form. The decoding is done based on the prototype of the RLP data,
/// ensuring that the node is reconstructed accurately.
///
/// **Note**: This implementation is still using the older RLP library and needs to be
/// migrated to `alloy_rlp` in the future.
// TODO: migrate to alloy_rlp
impl Decodable for MptNode {
    /// Decodes an RLP-encoded node from the provided `rlp` buffer.
    ///
    /// The method handles different RLP prototypes and reconstructs the `MptNode` based
    /// on the encoded data. If the RLP data does not match any known prototype or if
    /// there's an error during decoding, an error is returned.
    fn decode(rlp: &Rlp<'_>) -> Result<Self, DecoderError> {
        match rlp.prototype()? {
            Prototype::Null | Prototype::Data(0) => Ok(MptNodeData::Null.into()),
            Prototype::List(2) => {
                let path: Vec<u8> = rlp.val_at(0)?;
                let prefix = path[0];
                if (prefix & (2 << 4)) == 0 {
                    let node: MptNode = Decodable::decode(&rlp.at(1)?)?;
                    Ok(MptNodeData::Extension(path, Box::new(node)).into())
                } else {
                    Ok(MptNodeData::Leaf(path, rlp.val_at(1)?).into())
                }
            }
            Prototype::List(17) => {
                let mut node_list = Vec::with_capacity(16);
                for node_rlp in rlp.iter().take(16) {
                    match node_rlp.prototype()? {
                        Prototype::Null | Prototype::Data(0) => {
                            node_list.push(None);
                        }
                        _ => node_list.push(Some(Box::new(Decodable::decode(&node_rlp)?))),
                    }
                }
                let value: Vec<u8> = rlp.val_at(16)?;
                if value.is_empty() {
                    Ok(MptNodeData::Branch(node_list.try_into().unwrap()).into())
                } else {
                    Err(DecoderError::Custom("branch node with value"))
                }
            }
            Prototype::Data(32) => {
                let bytes: Vec<u8> = rlp.as_val()?;
                Ok(MptNodeData::Digest(B256::from_slice(&bytes)).into())
            }
            _ => Err(DecoderError::RlpIncorrectListLen),
        }
    }
}

/// Represents a node in the sparse Merkle Patricia Trie (MPT).
///
/// The [MptNode] type encapsulates the data and functionalities associated with a node in
/// the MPT. It provides methods for manipulating the trie, such as inserting, deleting,
/// and retrieving values, as well as utility methods for encoding, decoding, and
/// debugging.
impl MptNode {
    /// Clears the trie, replacing its data with an empty node, [MptNodeData::Null].
    ///
    /// This method effectively removes all key-value pairs from the trie.
    #[inline]
    pub fn clear(&mut self) {
        self.data = MptNodeData::Null;
        self.invalidate_ref_cache();
    }

    /// Decodes an RLP-encoded [MptNode] from the provided byte slice.
    ///
    /// This method allows for the deserialization of a previously serialized [MptNode].
    #[inline]
    pub fn decode(bytes: impl AsRef<[u8]>) -> Result<MptNode, Error> {
        rlp::decode(bytes.as_ref()).map_err(Error::from)
    }

    /// Retrieves the underlying data of the node.
    ///
    /// This method provides a reference to the node's data, allowing for inspection and
    /// manipulation.
    #[inline]
    pub fn as_data(&self) -> &MptNodeData {
        &self.data
    }

    /// Retrieves the [MptNodeReference] reference of the node when it's referenced inside
    /// another node.
    ///
    /// This method provides a way to obtain a compact representation of the node for
    /// storage or transmission purposes.
    #[inline]
    pub fn reference(&self) -> MptNodeReference {
        self.cached_reference.borrow_mut().get_or_insert_with(|| self.calc_reference()).clone()
    }

    /// Computes and returns the 256-bit hash of the node.
    ///
    /// This method provides a unique identifier for the node based on its content.
    #[inline]
    pub fn hash(&self) -> B256 {
        match self.data {
            MptNodeData::Null => EMPTY_ROOT,
            _ => match self
                .cached_reference
                .borrow_mut()
                .get_or_insert_with(|| self.calc_reference())
            {
                MptNodeReference::Digest(digest) => *digest,
                MptNodeReference::Bytes(bytes) => keccak(bytes).into(),
            },
        }
    }

    /// bincode::Encodes the [MptNodeReference] of this node into the `out` buffer.
    fn reference_encode(&self, out: &mut dyn alloy_rlp::BufMut) {
        match self.cached_reference.borrow_mut().get_or_insert_with(|| self.calc_reference()) {
            // if the reference is an RLP-encoded byte slice, copy it directly
            MptNodeReference::Bytes(bytes) => out.put_slice(bytes),
            // if the reference is a digest, RLP-encode it with its fixed known length
            MptNodeReference::Digest(digest) => {
                out.put_u8(alloy_rlp::EMPTY_STRING_CODE + 32);
                out.put_slice(digest.as_slice());
            }
        }
    }

    /// Returns the length of the encoded [MptNodeReference] of this node.
    fn reference_length(&self) -> usize {
        match self.cached_reference.borrow_mut().get_or_insert_with(|| self.calc_reference()) {
            MptNodeReference::Bytes(bytes) => bytes.len(),
            MptNodeReference::Digest(_) => 1 + 32,
        }
    }

    fn calc_reference(&self) -> MptNodeReference {
        match &self.data {
            MptNodeData::Null => MptNodeReference::Bytes(vec![alloy_rlp::EMPTY_STRING_CODE]),
            MptNodeData::Digest(digest) => MptNodeReference::Digest(*digest),
            _ => {
                let encoded = alloy_rlp::encode(self);
                if encoded.len() < 32 {
                    MptNodeReference::Bytes(encoded)
                } else {
                    MptNodeReference::Digest(keccak(encoded).into())
                }
            }
        }
    }

    /// Determines if the trie is empty.
    ///
    /// This method checks if the node represents an empty trie, i.e., it doesn't contain
    /// any key-value pairs.
    #[inline]
    pub fn is_empty(&self) -> bool {
        matches!(&self.data, MptNodeData::Null)
    }

    /// Determines if the node represents a digest.
    ///
    /// A digest is a compact representation of a sub-trie, represented by its hash.
    #[inline]
    pub fn is_digest(&self) -> bool {
        matches!(&self.data, MptNodeData::Digest(_))
    }

    /// Retrieves the nibbles corresponding to the node's prefix.
    ///
    /// Nibbles are half-bytes, and in the context of the MPT, they represent parts of
    /// keys.
    #[inline]
    pub fn nibs(&self) -> Vec<u8> {
        match &self.data {
            MptNodeData::Null | MptNodeData::Branch(_) | MptNodeData::Digest(_) => vec![],
            MptNodeData::Leaf(prefix, _) | MptNodeData::Extension(prefix, _) => prefix_nibs(prefix),
        }
    }

    /// Retrieves the value associated with a given key in the trie.
    ///
    /// If the key is not present in the trie, this method returns `None`. Otherwise, it
    /// returns a reference to the associated value. If [None] is returned, the key is
    /// provably not in the trie.
    #[inline]
    pub fn get(&self, key: &[u8]) -> Result<Option<&[u8]>, Error> {
        self.get_internal(&to_nibs(key))
    }

    /// Retrieves the RLP-decoded value corresponding to the key.
    ///
    /// If the key is not present in the trie, this method returns `None`. Otherwise, it
    /// returns the RLP-decoded value.
    #[inline]
    pub fn get_rlp<T: alloy_rlp::Decodable>(&self, key: &[u8]) -> Result<Option<T>, Error> {
        match self.get(key)? {
            Some(mut bytes) => Ok(Some(T::decode(&mut bytes)?)),
            None => Ok(None),
        }
    }

    fn get_internal(&self, key_nibs: &[u8]) -> Result<Option<&[u8]>, Error> {
        match &self.data {
            MptNodeData::Null => Ok(None),
            MptNodeData::Branch(nodes) => {
                if let Some((i, tail)) = key_nibs.split_first() {
                    match nodes[*i as usize] {
                        Some(ref node) => node.get_internal(tail),
                        None => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }
            MptNodeData::Leaf(prefix, value) => {
                if prefix_nibs(prefix) == key_nibs {
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
            MptNodeData::Extension(prefix, node) => {
                if let Some(tail) = key_nibs.strip_prefix(prefix_nibs(prefix).as_slice()) {
                    node.get_internal(tail)
                } else {
                    Ok(None)
                }
            }
            MptNodeData::Digest(digest) => Err(Error::NodeNotResolved(*digest)),
        }
    }

    /// Removes a key from the trie.
    ///
    /// This method attempts to remove a key-value pair from the trie. If the key is
    /// present, it returns `true`. Otherwise, it returns `false`.
    #[inline]
    pub fn delete(&mut self, key: &[u8]) -> Result<bool, Error> {
        self.delete_internal(&to_nibs(key))
    }

    fn delete_internal(&mut self, key_nibs: &[u8]) -> Result<bool, Error> {
        match &mut self.data {
            MptNodeData::Null => return Ok(false),
            MptNodeData::Branch(children) => {
                if let Some((i, tail)) = key_nibs.split_first() {
                    let child = &mut children[*i as usize];
                    match child {
                        Some(node) => {
                            if !node.delete_internal(tail)? {
                                return Ok(false);
                            }
                            // if the node is now empty, remove it
                            if node.is_empty() {
                                *child = None;
                            }
                        }
                        None => return Ok(false),
                    }
                } else {
                    return Err(Error::ValueInBranch);
                }

                let mut remaining = children.iter_mut().enumerate().filter(|(_, n)| n.is_some());
                // there will always be at least one remaining node
                let (index, node) = remaining.next().unwrap();
                // if there is only exactly one node left, we need to convert the branch
                if remaining.next().is_none() {
                    let mut orphan = node.take().unwrap();
                    match &mut orphan.data {
                        // if the orphan is a leaf, prepend the corresponding nib to it
                        MptNodeData::Leaf(prefix, orphan_value) => {
                            let new_nibs: Vec<_> =
                                iter::once(index as u8).chain(prefix_nibs(prefix)).collect();
                            self.data = MptNodeData::Leaf(
                                to_encoded_path(&new_nibs, true),
                                mem::take(orphan_value),
                            );
                        }
                        // if the orphan is an extension, prepend the corresponding nib to it
                        MptNodeData::Extension(prefix, orphan_child) => {
                            let new_nibs: Vec<_> =
                                iter::once(index as u8).chain(prefix_nibs(prefix)).collect();
                            self.data = MptNodeData::Extension(
                                to_encoded_path(&new_nibs, false),
                                mem::take(orphan_child),
                            );
                        }
                        // if the orphan is a branch or digest, convert to an extension
                        MptNodeData::Branch(_) | MptNodeData::Digest(_) => {
                            self.data = MptNodeData::Extension(
                                to_encoded_path(&[index as u8], false),
                                orphan,
                            );
                        }
                        MptNodeData::Null => unreachable!(),
                    }
                }
            }
            MptNodeData::Leaf(prefix, _) => {
                if prefix_nibs(prefix) != key_nibs {
                    return Ok(false);
                }
                self.data = MptNodeData::Null;
            }
            MptNodeData::Extension(prefix, child) => {
                let mut self_nibs = prefix_nibs(prefix);
                if let Some(tail) = key_nibs.strip_prefix(self_nibs.as_slice()) {
                    if !child.delete_internal(tail)? {
                        return Ok(false);
                    }
                } else {
                    return Ok(false);
                }

                // an extension can only point to a branch or a digest; since it's sub trie was
                // modified, we need to make sure that this property still holds
                match &mut child.data {
                    // if the child is empty, remove the extension
                    MptNodeData::Null => {
                        self.data = MptNodeData::Null;
                    }
                    // for a leaf, replace the extension with the extended leaf
                    MptNodeData::Leaf(prefix, value) => {
                        self_nibs.extend(prefix_nibs(prefix));
                        self.data =
                            MptNodeData::Leaf(to_encoded_path(&self_nibs, true), mem::take(value));
                    }
                    // for an extension, replace the extension with the extended extension
                    MptNodeData::Extension(prefix, node) => {
                        self_nibs.extend(prefix_nibs(prefix));
                        self.data = MptNodeData::Extension(
                            to_encoded_path(&self_nibs, false),
                            mem::take(node),
                        );
                    }
                    // for a branch or digest, the extension is still correct
                    MptNodeData::Branch(_) | MptNodeData::Digest(_) => {}
                }
            }
            MptNodeData::Digest(digest) => return Err(Error::NodeNotResolved(*digest)),
        };

        self.invalidate_ref_cache();
        Ok(true)
    }

    /// Inserts a key-value pair into the trie.
    ///
    /// This method attempts to insert a new key-value pair into the trie. If the
    /// insertion is successful, it returns `true`. If the key already exists, it updates
    /// the value and returns `false`.
    #[inline]
    pub fn insert(&mut self, key: &[u8], value: Vec<u8>) -> Result<bool, Error> {
        if value.is_empty() {
            panic!("value must not be empty");
        }
        self.insert_internal(&to_nibs(key), value)
    }

    /// Inserts an RLP-encoded value into the trie.
    ///
    /// This method inserts a value that's been encoded using RLP into the trie.
    #[inline]
    pub fn insert_rlp(&mut self, key: &[u8], value: impl Encodable) -> Result<bool, Error> {
        self.insert_internal(&to_nibs(key), value.to_rlp())
    }

    fn insert_internal(&mut self, key_nibs: &[u8], value: Vec<u8>) -> Result<bool, Error> {
        match &mut self.data {
            MptNodeData::Null => {
                self.data = MptNodeData::Leaf(to_encoded_path(key_nibs, true), value);
            }
            MptNodeData::Branch(children) => {
                if let Some((i, tail)) = key_nibs.split_first() {
                    let child = &mut children[*i as usize];
                    match child {
                        Some(node) => {
                            if !node.insert_internal(tail, value)? {
                                return Ok(false);
                            }
                        }
                        // if the corresponding child is empty, insert a new leaf
                        None => {
                            *child = Some(Box::new(
                                MptNodeData::Leaf(to_encoded_path(tail, true), value).into(),
                            ));
                        }
                    }
                } else {
                    return Err(Error::ValueInBranch);
                }
            }
            MptNodeData::Leaf(prefix, old_value) => {
                let self_nibs = prefix_nibs(prefix);
                let common_len = lcp(&self_nibs, key_nibs);
                if common_len == self_nibs.len() && common_len == key_nibs.len() {
                    // if self_nibs == key_nibs, update the value if it is different
                    if old_value == &value {
                        return Ok(false);
                    }
                    *old_value = value;
                } else if common_len == self_nibs.len() || common_len == key_nibs.len() {
                    return Err(Error::ValueInBranch);
                } else {
                    let split_point = common_len + 1;
                    // otherwise, create a branch with two children
                    let mut children: [Option<Box<MptNode>>; 16] = Default::default();

                    children[self_nibs[common_len] as usize] = Some(Box::new(
                        MptNodeData::Leaf(
                            to_encoded_path(&self_nibs[split_point..], true),
                            mem::take(old_value),
                        )
                        .into(),
                    ));
                    children[key_nibs[common_len] as usize] = Some(Box::new(
                        MptNodeData::Leaf(to_encoded_path(&key_nibs[split_point..], true), value)
                            .into(),
                    ));

                    let branch = MptNodeData::Branch(children);
                    if common_len > 0 {
                        // create parent extension for new branch
                        self.data = MptNodeData::Extension(
                            to_encoded_path(&self_nibs[..common_len], false),
                            Box::new(branch.into()),
                        );
                    } else {
                        self.data = branch;
                    }
                }
            }
            MptNodeData::Extension(prefix, existing_child) => {
                let self_nibs = prefix_nibs(prefix);
                let common_len = lcp(&self_nibs, key_nibs);
                if common_len == self_nibs.len() {
                    // traverse down for update
                    if !existing_child.insert_internal(&key_nibs[common_len..], value)? {
                        return Ok(false);
                    }
                } else if common_len == key_nibs.len() {
                    return Err(Error::ValueInBranch);
                } else {
                    let split_point = common_len + 1;
                    // otherwise, create a branch with two children
                    let mut children: [Option<Box<MptNode>>; 16] = Default::default();

                    children[self_nibs[common_len] as usize] = if split_point < self_nibs.len() {
                        Some(Box::new(
                            MptNodeData::Extension(
                                to_encoded_path(&self_nibs[split_point..], false),
                                mem::take(existing_child),
                            )
                            .into(),
                        ))
                    } else {
                        Some(mem::take(existing_child))
                    };
                    children[key_nibs[common_len] as usize] = Some(Box::new(
                        MptNodeData::Leaf(to_encoded_path(&key_nibs[split_point..], true), value)
                            .into(),
                    ));

                    let branch = MptNodeData::Branch(children);
                    if common_len > 0 {
                        // Create parent extension for new branch
                        self.data = MptNodeData::Extension(
                            to_encoded_path(&self_nibs[..common_len], false),
                            Box::new(branch.into()),
                        );
                    } else {
                        self.data = branch;
                    }
                }
            }
            MptNodeData::Digest(digest) => return Err(Error::NodeNotResolved(*digest)),
        };

        self.invalidate_ref_cache();
        Ok(true)
    }

    fn invalidate_ref_cache(&mut self) {
        self.cached_reference.borrow_mut().take();
    }

    /// Returns the number of traversable nodes in the trie.
    ///
    /// This method provides a count of all the nodes that can be traversed within the
    /// trie.
    pub fn size(&self) -> usize {
        match self.as_data() {
            MptNodeData::Null => 0,
            MptNodeData::Branch(children) => {
                children.iter().flatten().map(|n| n.size()).sum::<usize>() + 1
            }
            MptNodeData::Leaf(_, _) => 1,
            MptNodeData::Extension(_, child) => child.size() + 1,
            MptNodeData::Digest(_) => 0,
        }
    }

    /// Formats the trie as a string list, where each line corresponds to a trie leaf.
    ///
    /// This method is primarily used for debugging purposes, providing a visual
    /// representation of the trie's structure.
    pub fn debug_rlp<T: alloy_rlp::Decodable + Debug>(&self) -> Vec<String> {
        // convert the nibs to hex
        let nibs: String = self.nibs().iter().fold(String::new(), |mut output, n| {
            let _ = write!(output, "{:x}", n);
            output
        });

        match self.as_data() {
            MptNodeData::Null => vec![format!("{:?}", MptNodeData::Null)],
            MptNodeData::Branch(children) => children
                .iter()
                .enumerate()
                .flat_map(|(i, child)| {
                    match child {
                        Some(node) => node.debug_rlp::<T>(),
                        None => vec!["None".to_string()],
                    }
                    .into_iter()
                    .map(move |s| format!("{:x} {}", i, s))
                })
                .collect(),
            MptNodeData::Leaf(_, data) => {
                vec![format!("{} -> {:?}", nibs, T::decode(&mut &data[..]).unwrap())]
            }
            MptNodeData::Extension(_, node) => {
                node.debug_rlp::<T>().into_iter().map(|s| format!("{} {}", nibs, s)).collect()
            }
            MptNodeData::Digest(digest) => vec![format!("#{:#}", digest)],
        }
    }

    /// Returns the length of the RLP payload of the node.
    fn payload_length(&self) -> usize {
        match &self.data {
            MptNodeData::Null => 0,
            MptNodeData::Branch(nodes) => {
                1 + nodes
                    .iter()
                    .map(|child| child.as_ref().map_or(1, |node| node.reference_length()))
                    .sum::<usize>()
            }
            MptNodeData::Leaf(prefix, value) => {
                prefix.as_slice().length() + value.as_slice().length()
            }
            MptNodeData::Extension(prefix, node) => {
                prefix.as_slice().length() + node.reference_length()
            }
            MptNodeData::Digest(_) => 32,
        }
    }
}

/// Converts a byte slice into a vector of nibbles.
///
/// A nibble is 4 bits or half of an 8-bit byte. This function takes each byte from the
/// input slice, splits it into two nibbles, and appends them to the resulting vector.
pub fn to_nibs(slice: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(2 * slice.len());
    for byte in slice {
        result.push(byte >> 4);
        result.push(byte & 0xf);
    }
    result
}

/// bincode::Encodes a slice of nibbles into a vector of bytes, with an additional prefix to
/// indicate the type of node (leaf or extension).
///
/// The function starts by determining the type of node based on the `is_leaf` parameter.
/// If the node is a leaf, the prefix is set to `0x20`. If the length of the nibbles is
/// odd, the prefix is adjusted and the first nibble is incorporated into it.
///
/// The remaining nibbles are then combined into bytes, with each pair of nibbles forming
/// a single byte. The resulting vector starts with the prefix, followed by the encoded
/// bytes.
pub fn to_encoded_path(mut nibs: &[u8], is_leaf: bool) -> Vec<u8> {
    let mut prefix = (is_leaf as u8) * 0x20;
    if nibs.len() % 2 != 0 {
        prefix += 0x10 + nibs[0];
        nibs = &nibs[1..];
    }
    iter::once(prefix).chain(nibs.chunks_exact(2).map(|byte| (byte[0] << 4) + byte[1])).collect()
}

/// Returns the length of the common prefix.
fn lcp(a: &[u8], b: &[u8]) -> usize {
    for (i, (a, b)) in iter::zip(a, b).enumerate() {
        if a != b {
            return i;
        }
    }
    cmp::min(a.len(), b.len())
}

fn prefix_nibs(prefix: &[u8]) -> Vec<u8> {
    let (extension, tail) = prefix.split_first().unwrap();
    // the first bit of the first nibble denotes the parity
    let is_odd = extension & (1 << 4) != 0;

    let mut result = Vec::with_capacity(2 * tail.len() + is_odd as usize);
    // for odd lengths, the second nibble contains the first element
    if is_odd {
        result.push(extension & 0xf);
    }
    for nib in tail {
        result.push(nib >> 4);
        result.push(nib & 0xf);
    }
    result
}

/// Parses proof bytes into a vector of MPT nodes.
pub fn parse_proof(proof: &[impl AsRef<[u8]>]) -> Result<Vec<MptNode>> {
    Ok(proof.iter().map(MptNode::decode).collect::<Result<Vec<_>, _>>()?)
}

/// Creates a Merkle Patricia trie from an EIP-1186 proof.
/// For inclusion proofs the returned trie contains exactly one leaf with the value.
pub fn mpt_from_proof(proof_nodes: &[MptNode]) -> Result<MptNode> {
    let mut next: Option<MptNode> = None;
    for (i, node) in proof_nodes.iter().enumerate().rev() {
        // there is nothing to replace for the last node
        let Some(replacement) = next else {
            next = Some(node.clone());
            continue;
        };

        // the next node must have a digest reference
        let MptNodeReference::Digest(ref child_ref) = replacement.reference() else {
            panic!("node {} in proof is not referenced by hash", i + 1);
        };
        // find the child that references the next node
        let resolved: MptNode = match node.as_data().clone() {
            MptNodeData::Branch(mut children) => {
                if let Some(child) = children.iter_mut().flatten().find(
                    |child| matches!(child.as_data(), MptNodeData::Digest(d) if d == child_ref),
                ) {
                    *child = Box::new(replacement);
                } else {
                    panic!("node {} does not reference the successor", i);
                }
                MptNodeData::Branch(children).into()
            }
            MptNodeData::Extension(prefix, child) => {
                if !matches!(child.as_data(), MptNodeData::Digest(d) if d == child_ref) {
                    panic!("node {} does not reference the successor", i);
                }
                MptNodeData::Extension(prefix, Box::new(replacement)).into()
            }
            MptNodeData::Null | MptNodeData::Leaf(_, _) | MptNodeData::Digest(_) => {
                panic!("node {} has no children to replace", i);
            }
        };

        next = Some(resolved);
    }

    // the last node in the proof should be the root
    Ok(next.unwrap_or_default())
}

/// Verifies that the given proof is a valid proof of exclusion for the given key.
pub fn is_not_included(key: &[u8], proof_nodes: &[MptNode]) -> Result<bool> {
    let proof_trie = mpt_from_proof(proof_nodes).unwrap();
    // for valid proofs, the get must not fail
    let value = proof_trie.get(key).unwrap();

    Ok(value.is_none())
}

/// Creates a new MPT trie where all the digests contained in `node_store` are resolved.
pub fn resolve_nodes(root: &MptNode, node_store: &HashMap<MptNodeReference, MptNode>) -> MptNode {
    let trie = match root.as_data() {
        MptNodeData::Null | MptNodeData::Leaf(_, _) => root.clone(),
        MptNodeData::Branch(children) => {
            let children: Vec<_> = children
                .iter()
                .map(|child| child.as_ref().map(|node| Box::new(resolve_nodes(node, node_store))))
                .collect();
            MptNodeData::Branch(children.try_into().unwrap()).into()
        }
        MptNodeData::Extension(prefix, target) => {
            MptNodeData::Extension(prefix.clone(), Box::new(resolve_nodes(target, node_store)))
                .into()
        }
        MptNodeData::Digest(digest) => {
            if let Some(node) = node_store.get(&MptNodeReference::Digest(*digest)) {
                resolve_nodes(node, node_store)
            } else {
                root.clone()
            }
        }
    };
    // the root hash must not change
    debug_assert_eq!(root.hash(), trie.hash());

    trie
}

/// Returns a list of all possible nodes that can be created by shortening the path of the
/// given node.
/// When nodes in an MPT are deleted, leaves or extensions may be extended. To still be
/// able to identify the original nodes, we create all shortened versions of the node.
pub fn shorten_node_path(node: &MptNode) -> Vec<MptNode> {
    let mut res = Vec::new();
    let nibs = node.nibs();
    match node.as_data() {
        MptNodeData::Null | MptNodeData::Branch(_) | MptNodeData::Digest(_) => {}
        MptNodeData::Leaf(_, value) => {
            for i in 0..=nibs.len() {
                res.push(MptNodeData::Leaf(to_encoded_path(&nibs[i..], true), value.clone()).into())
            }
        }
        MptNodeData::Extension(_, child) => {
            for i in 0..=nibs.len() {
                res.push(
                    MptNodeData::Extension(to_encoded_path(&nibs[i..], false), child.clone())
                        .into(),
                )
            }
        }
    };
    res
}

pub fn proofs_to_tries(
    state_root: B256,
    proofs: &HashMap<Address, AccountProof>,
) -> Result<EthereumState> {
    // if no addresses are provided, return the trie only consisting of the state root
    if proofs.is_empty() {
        return Ok(EthereumState {
            state_trie: node_from_digest(state_root),
            storage_tries: Default::default(),
        });
    }

    let mut storage: HashMap<B256, MptNode> =
        HashMap::with_capacity_and_hasher(proofs.len(), DefaultHashBuilder::default());

    let mut state_nodes = HashMap::<_, _>::default();
    let mut state_root_node = MptNode::default();
    for (address, proof) in proofs {
        let proof_nodes = parse_proof(&proof.proof).unwrap();
        mpt_from_proof(&proof_nodes).unwrap();

        // the first node in the proof is the root
        if let Some(node) = proof_nodes.first() {
            state_root_node = node.clone();
        }

        proof_nodes.into_iter().for_each(|node| {
            state_nodes.insert(node.reference(), node);
        });

        // if no slots are provided, return the trie only consisting of the storage root
        let storage_root = proof.storage_root;
        if proof.storage_proofs.is_empty() {
            let storage_root_node = node_from_digest(storage_root);
            storage.insert(B256::from(&keccak(address)), storage_root_node);
            continue;
        }

        let mut storage_nodes = HashMap::<_, _>::default();
        let mut storage_root_node = MptNode::default();
        for storage_proof in &proof.storage_proofs {
            let proof_nodes = parse_proof(&storage_proof.proof).unwrap();
            mpt_from_proof(&proof_nodes).unwrap();

            // the first node in the proof is the root
            if let Some(node) = proof_nodes.first() {
                storage_root_node = node.clone();
            }

            proof_nodes.into_iter().for_each(|node| {
                storage_nodes.insert(node.reference(), node);
            });
        }

        // create the storage trie, from all the relevant nodes
        let storage_trie = resolve_nodes(&storage_root_node, &storage_nodes);
        assert_eq!(storage_trie.hash(), storage_root);

        storage.insert(B256::from(&keccak(address)), storage_trie);
    }
    let state_trie = resolve_nodes(&state_root_node, &state_nodes);
    assert_eq!(state_trie.hash(), state_root);

    Ok(EthereumState { state_trie, storage_tries: StorageTries(storage) })
}

pub fn transition_proofs_to_tries(
    state_root: B256,
    parent_proofs: &HashMap<Address, AccountProof>,
    proofs: &HashMap<Address, AccountProof>,
) -> Result<EthereumState> {
    // if no addresses are provided, return the trie only consisting of the state root
    if parent_proofs.is_empty() {
        return Ok(EthereumState {
            state_trie: node_from_digest(state_root),
            storage_tries: Default::default(),
        });
    }

    let mut storage: HashMap<B256, MptNode, _> =
        HashMap::with_capacity_and_hasher(parent_proofs.len(), DefaultHashBuilder::default());

    let mut state_nodes = HashMap::<_, _, DefaultHashBuilder>::default();
    let mut state_root_node = MptNode::default();
    for (address, proof) in parent_proofs {
        let proof_nodes = parse_proof(&proof.proof).unwrap();
        mpt_from_proof(&proof_nodes).unwrap();

        // the first node in the proof is the root
        if let Some(node) = proof_nodes.first() {
            state_root_node = node.clone();
        }

        proof_nodes.into_iter().for_each(|node| {
            state_nodes.insert(node.reference(), node);
        });

        let fini_proofs = proofs.get(address).unwrap();

        // assure that addresses can be deleted from the state trie
        add_orphaned_leafs(address, &fini_proofs.proof, &mut state_nodes)?;

        // if no slots are provided, return the trie only consisting of the storage root
        let storage_root = proof.storage_root;
        if proof.storage_proofs.is_empty() {
            let storage_root_node = node_from_digest(storage_root);
            storage.insert(B256::from(&keccak(address)), storage_root_node);
            continue;
        }

        let mut storage_nodes = HashMap::<_, _>::default();
        let mut storage_root_node = MptNode::default();
        for storage_proof in &proof.storage_proofs {
            let proof_nodes = parse_proof(&storage_proof.proof).unwrap();
            mpt_from_proof(&proof_nodes).unwrap();

            // the first node in the proof is the root
            if let Some(node) = proof_nodes.first() {
                storage_root_node = node.clone();
            }

            proof_nodes.into_iter().for_each(|node| {
                storage_nodes.insert(node.reference(), node);
            });
        }

        // assure that slots can be deleted from the storage trie
        for storage_proof in &fini_proofs.storage_proofs {
            add_orphaned_leafs(storage_proof.key.0, &storage_proof.proof, &mut storage_nodes)?;
        }
        // create the storage trie, from all the relevant nodes
        let storage_trie = resolve_nodes(&storage_root_node, &storage_nodes);
        assert_eq!(storage_trie.hash(), storage_root);

        storage.insert(B256::from(&keccak(address)), storage_trie);
    }
    let state_trie = resolve_nodes(&state_root_node, &state_nodes);
    assert_eq!(state_trie.hash(), state_root);

    Ok(EthereumState { state_trie, storage_tries: StorageTries(storage) })
}

/// Adds all the leaf nodes of non-inclusion proofs to the nodes.
fn add_orphaned_leafs(
    key: impl AsRef<[u8]>,
    proof: &[impl AsRef<[u8]>],
    nodes_by_reference: &mut HashMap<MptNodeReference, MptNode>,
) -> Result<()> {
    if !proof.is_empty() {
        let proof_nodes = parse_proof(proof).context("invalid proof encoding")?;
        if is_not_included(&keccak(key), &proof_nodes)? {
            // add the leaf node to the nodes
            let leaf = proof_nodes.last().unwrap();
            shorten_node_path(leaf).into_iter().for_each(|node| {
                nodes_by_reference.insert(node.reference(), node);
            });
        }
    }

    Ok(())
}

/// Creates a new MPT node from a digest.
fn node_from_digest(digest: B256) -> MptNode {
    match digest {
        EMPTY_ROOT | B256::ZERO => MptNode::default(),
        _ => MptNodeData::Digest(digest).into(),
    }
}

#[cfg(test)]
mod tests {
    use hex_literal::hex;

    use super::*;

    #[test]
    pub fn test_trie_pointer_no_keccak() {
        let cases = [("do", "verb"), ("dog", "puppy"), ("doge", "coin"), ("horse", "stallion")];
        for (k, v) in cases {
            let node: MptNode =
                MptNodeData::Leaf(k.as_bytes().to_vec(), v.as_bytes().to_vec()).into();
            assert!(
                matches!(node.reference(),MptNodeReference::Bytes(bytes) if bytes == node.to_rlp().to_vec())
            );
        }
    }

    #[test]
    pub fn test_to_encoded_path() {
        // extension node with an even path length
        let nibbles = vec![0x0a, 0x0b, 0x0c, 0x0d];
        assert_eq!(to_encoded_path(&nibbles, false), vec![0x00, 0xab, 0xcd]);
        // extension node with an odd path length
        let nibbles = vec![0x0a, 0x0b, 0x0c];
        assert_eq!(to_encoded_path(&nibbles, false), vec![0x1a, 0xbc]);
        // leaf node with an even path length
        let nibbles = vec![0x0a, 0x0b, 0x0c, 0x0d];
        assert_eq!(to_encoded_path(&nibbles, true), vec![0x20, 0xab, 0xcd]);
        // leaf node with an odd path length
        let nibbles = vec![0x0a, 0x0b, 0x0c];
        assert_eq!(to_encoded_path(&nibbles, true), vec![0x3a, 0xbc]);
    }

    #[test]
    pub fn test_lcp() {
        let cases = [
            (vec![], vec![], 0),
            (vec![0xa], vec![0xa], 1),
            (vec![0xa, 0xb], vec![0xa, 0xc], 1),
            (vec![0xa, 0xb], vec![0xa, 0xb], 2),
            (vec![0xa, 0xb], vec![0xa, 0xb, 0xc], 2),
            (vec![0xa, 0xb, 0xc], vec![0xa, 0xb, 0xc], 3),
            (vec![0xa, 0xb, 0xc], vec![0xa, 0xb, 0xc, 0xd], 3),
            (vec![0xa, 0xb, 0xc, 0xd], vec![0xa, 0xb, 0xc, 0xd], 4),
        ];
        for (a, b, cpl) in cases {
            assert_eq!(lcp(&a, &b), cpl)
        }
    }

    #[test]
    pub fn test_empty() {
        let trie = MptNode::default();

        assert!(trie.is_empty());
        assert_eq!(trie.reference(), MptNodeReference::Bytes(vec![0x80]));
        let expected = hex!("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421");
        assert_eq!(expected, trie.hash().0);

        // test RLP encoding
        let mut out = Vec::new();
        trie.encode(&mut out);
        assert_eq!(out, vec![0x80]);
        assert_eq!(trie.length(), out.len());
        let decoded = MptNode::decode(out).unwrap();
        assert_eq!(trie.hash(), decoded.hash());
    }

    #[test]
    pub fn test_empty_key() {
        let mut trie = MptNode::default();

        trie.insert(&[], b"empty".to_vec()).unwrap();
        assert_eq!(trie.get(&[]).unwrap(), Some(b"empty".as_ref()));
        assert!(trie.delete(&[]).unwrap());
    }

    #[test]
    pub fn test_clear() {
        let mut trie = MptNode::default();
        trie.insert(b"dog", b"puppy".to_vec()).unwrap();
        assert!(!trie.is_empty());
        assert_ne!(trie.hash(), EMPTY_ROOT);

        trie.clear();
        assert!(trie.is_empty());
        assert_eq!(trie.hash(), EMPTY_ROOT);
    }

    #[test]
    pub fn test_tiny() {
        // trie consisting of an extension, a branch and two leafs
        let mut trie = MptNode::default();
        trie.insert_rlp(b"a", 0u8).unwrap();
        trie.insert_rlp(b"b", 1u8).unwrap();

        assert!(!trie.is_empty());
        let exp_rlp = hex!("d816d680c3208180c220018080808080808080808080808080");
        assert_eq!(trie.reference(), MptNodeReference::Bytes(exp_rlp.to_vec()));
        let exp_hash = hex!("6fbf23d6ec055dd143ff50d558559770005ff44ae1d41276f1bd83affab6dd3b");
        assert_eq!(trie.hash().0, exp_hash);

        // test RLP encoding
        let mut out = Vec::new();
        trie.encode(&mut out);
        assert_eq!(out, exp_rlp.to_vec());
        assert_eq!(trie.length(), out.len());
        let decoded = MptNode::decode(out).unwrap();
        assert_eq!(trie.hash(), decoded.hash());
    }

    #[test]
    pub fn test_partial() {
        let mut trie = MptNode::default();
        trie.insert_rlp(b"aa", 0u8).unwrap();
        trie.insert_rlp(b"ab", 1u8).unwrap();
        trie.insert_rlp(b"ba", 2u8).unwrap();

        let exp_hash = trie.hash();

        // replace one node with its digest
        let MptNodeData::Extension(_, node) = &mut trie.data else { panic!("extension expected") };
        **node = MptNodeData::Digest(node.hash()).into();
        assert!(node.is_digest());

        let trie = MptNode::decode(trie.to_rlp()).unwrap();
        assert_eq!(trie.hash(), exp_hash);

        // lookups should fail
        trie.get(b"aa").unwrap_err();
        trie.get(b"a0").unwrap_err();
    }

    #[test]
    pub fn test_branch_value() {
        let mut trie = MptNode::default();
        trie.insert(b"do", b"verb".to_vec()).unwrap();
        // leads to a branch with value which is not supported
        trie.insert(b"dog", b"puppy".to_vec()).unwrap_err();
    }

    #[test]
    pub fn test_insert() {
        let mut trie = MptNode::default();
        let vals = vec![
            ("painting", "place"),
            ("guest", "ship"),
            ("mud", "leave"),
            ("paper", "call"),
            ("gate", "boast"),
            ("tongue", "gain"),
            ("baseball", "wait"),
            ("tale", "lie"),
            ("mood", "cope"),
            ("menu", "fear"),
        ];
        for (key, val) in &vals {
            assert!(trie.insert(key.as_bytes(), val.as_bytes().to_vec()).unwrap());
        }

        let expected = hex!("2bab6cdf91a23ebf3af683728ea02403a98346f99ed668eec572d55c70a4b08f");
        assert_eq!(expected, trie.hash().0);

        for (key, value) in &vals {
            assert_eq!(trie.get(key.as_bytes()).unwrap(), Some(value.as_bytes()));
        }

        // check inserting duplicate keys
        assert!(trie.insert(vals[0].0.as_bytes(), b"new".to_vec()).unwrap());
        assert!(!trie.insert(vals[0].0.as_bytes(), b"new".to_vec()).unwrap());

        // try RLP roundtrip
        let decoded = MptNode::decode(trie.to_rlp()).unwrap();
        assert_eq!(trie.hash(), decoded.hash());
    }

    #[test]
    pub fn test_keccak_trie() {
        const N: usize = 512;

        // insert
        let mut trie = MptNode::default();
        for i in 0..N {
            assert!(trie.insert_rlp(&keccak(i.to_be_bytes()), i).unwrap());

            // check hash against trie build in reverse
            let mut reference = MptNode::default();
            for j in (0..=i).rev() {
                reference.insert_rlp(&keccak(j.to_be_bytes()), j).unwrap();
            }
            assert_eq!(trie.hash(), reference.hash());
        }

        let expected = hex!("7310027edebdd1f7c950a7fb3413d551e85dff150d45aca4198c2f6315f9b4a7");
        assert_eq!(trie.hash().0, expected);

        // get
        for i in 0..N {
            assert_eq!(trie.get_rlp(&keccak(i.to_be_bytes())).unwrap(), Some(i));
            assert!(trie.get(&keccak((i + N).to_be_bytes())).unwrap().is_none());
        }

        // delete
        for i in 0..N {
            assert!(trie.delete(&keccak(i.to_be_bytes())).unwrap());

            let mut reference = MptNode::default();
            for j in ((i + 1)..N).rev() {
                reference.insert_rlp(&keccak(j.to_be_bytes()), j).unwrap();
            }
            assert_eq!(trie.hash(), reference.hash());
        }
        assert!(trie.is_empty());
    }

    #[test]
    pub fn test_index_trie() {
        const N: usize = 512;

        // insert
        let mut trie = MptNode::default();
        for i in 0..N {
            assert!(trie.insert_rlp(&i.to_rlp(), i).unwrap());

            // check hash against trie build in reverse
            let mut reference = MptNode::default();
            for j in (0..=i).rev() {
                reference.insert_rlp(&j.to_rlp(), j).unwrap();
            }
            assert_eq!(trie.hash(), reference.hash());

            // try RLP roundtrip
            let decoded = MptNode::decode(trie.to_rlp()).unwrap();
            assert_eq!(trie.hash(), decoded.hash());
        }

        // get
        for i in 0..N {
            assert_eq!(trie.get_rlp(&i.to_rlp()).unwrap(), Some(i));
            assert!(trie.get(&(i + N).to_rlp()).unwrap().is_none());
        }

        // delete
        for i in 0..N {
            assert!(trie.delete(&i.to_rlp()).unwrap());

            let mut reference = MptNode::default();
            for j in ((i + 1)..N).rev() {
                reference.insert_rlp(&j.to_rlp(), j).unwrap();
            }
            assert_eq!(trie.hash(), reference.hash());
        }
        assert!(trie.is_empty());
    }
}
