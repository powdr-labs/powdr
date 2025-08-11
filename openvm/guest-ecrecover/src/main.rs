#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]
openvm::entry!(main);

use hex_literal::hex;
use k256::{
    ecdsa::{K256VerifyExt, RecoveryId, Signature, VerifyingKey},
    EncodedPoint, Secp256k1,
};
use sha2::{Digest, Sha256};

// Signature recovery test vectors
struct RecoveryTestVector {
    pk: [u8; 33],
    msg: &'static [u8],
    sig: [u8; 64],
    recid: RecoveryId,
}

const RECOVERY_TEST_VECTORS: &[RecoveryTestVector] = &[
    // Recovery ID 0
    RecoveryTestVector {
        pk: hex!("021a7a569e91dbf60581509c7fc946d1003b60c7dee85299538db6353538d59574"),
        msg: b"example message",
        sig: hex!(
            "ce53abb3721bafc561408ce8ff99c909f7f0b18a2f788649d6470162ab1aa032
                 3971edc523a6d6453f3fb6128d318d9db1a5ff3386feb1047d9816e780039d52"
        ),
        recid: RecoveryId::new(false, false),
    },
    // Recovery ID 1
    RecoveryTestVector {
        pk: hex!("036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"),
        msg: b"example message",
        sig: hex!(
            "46c05b6368a44b8810d79859441d819b8e7cdc8bfd371e35c53196f4bcacdb51
                 35c7facce2a97b95eacba8a586d87b7958aaf8368ab29cee481f76e871dbd9cb"
        ),
        recid: RecoveryId::new(true, false),
    },
];

//Test public key recovery
pub fn main() {
    for vector in RECOVERY_TEST_VECTORS {
        let digest = Sha256::digest(vector.msg);
        let sig = Signature::try_from(vector.sig.as_slice()).unwrap();
        let recid = vector.recid;
        let pk = <VerifyingKey as K256VerifyExt>::recover_from_prehash::<Secp256k1>(
            digest.as_slice(),
            &sig,
            recid,
        )
        .unwrap();
        assert_eq!(&vector.pk[..], EncodedPoint::from(&pk).as_bytes());
    }
}
