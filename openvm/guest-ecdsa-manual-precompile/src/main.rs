#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_imports)]

use core::hint::black_box;

use hex_literal::hex;
use openvm_ecc_guest::{
    algebra::IntMod,
    ecdsa::{verify_prehashed, VerifyingKey},
    weierstrass::WeierstrassPoint,
};
use openvm_k256::{
    ecdsa::{self, signature::hazmat::PrehashVerifier, RecoveryId, Signature},
    Secp256k1, Secp256k1Point,
};
use openvm_keccak256::keccak256;

openvm::entry!(main);

openvm::init!("openvm_init_ecdsa_k256.rs");

// Ref: https://docs.rs/k256/latest/k256/ecdsa/index.html
pub fn main() {
    let msg = b"example message";

    let signature = hex!(
            "46c05b6368a44b8810d79859441d819b8e7cdc8bfd371e35c53196f4bcacdb5135c7facce2a97b95eacba8a586d87b7958aaf8368ab29cee481f76e871dbd9cb"
        );

    let recid = RecoveryId::try_from(1u8).unwrap();

    let prehash = keccak256(black_box(msg));

    let recovered_key =
        VerifyingKey::<Secp256k1>::recover_from_prehash_noverify(&prehash, &signature, recid)
            .unwrap();

    let expected_key = ecdsa::VerifyingKey::from_sec1_bytes(&hex!(
        "0200866db99873b09fc2fb1e3ba549b156e96d1a567e3284f5f0e859a83320cb8b"
    ))
    .unwrap();

    // Test sec1 encoding and decoding
    let expected_key_uncompressed = expected_key.to_encoded_point(false);
    let public_key_uncompressed = recovered_key.to_sec1_bytes(false);
    assert_eq!(
        &public_key_uncompressed,
        &expected_key_uncompressed.as_bytes()
    );

    let expected_key_compressed = expected_key.to_encoded_point(true);
    let public_key_compressed = recovered_key.to_sec1_bytes(true);
    assert_eq!(&public_key_compressed, &expected_key_compressed.as_bytes());

    let public_key_uncompressed_decoded =
        VerifyingKey::<Secp256k1>::from_sec1_bytes(&public_key_uncompressed).unwrap();
    let public_key_compressed_decoded =
        VerifyingKey::<Secp256k1>::from_sec1_bytes(&public_key_compressed).unwrap();

    assert_eq!(
        public_key_uncompressed_decoded.as_affine(),
        recovered_key.as_affine()
    );
    assert_eq!(
        public_key_compressed_decoded.as_affine(),
        recovered_key.as_affine()
    );

    // Test verification
    recovered_key
        .verify_prehash(&prehash, &Signature::from_slice(&signature).unwrap())
        .unwrap();

    // Test bad signature
    let mut bad_sig1 = signature;
    bad_sig1[..32].copy_from_slice(&[0u8; 32]);
    let mut bad_sig2 = signature;
    bad_sig2[32..].copy_from_slice(&[0u8; 32]);
    let mut bad_sig3 = signature;
    bad_sig3[..32].copy_from_slice(&[0xff; 32]);
    let mut bad_sig4 = signature;
    bad_sig4[32..].copy_from_slice(&[0xff; 32]);
    for bad_sig in [bad_sig1, bad_sig2, bad_sig3, bad_sig4] {
        assert!(VerifyingKey::<Secp256k1>::recover_from_prehash_noverify(
            &prehash, &bad_sig, recid
        )
        .is_err());
        assert!(verify_prehashed::<Secp256k1>(
            recovered_key.as_affine().clone(),
            &prehash,
            &bad_sig
        )
        .is_err());
    }
}