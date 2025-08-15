//#![cfg_attr(not(feature = "std"), no_main)]
//#![cfg_attr(not(feature = "std"), no_std)]
openvm::entry!(main);

use ecdsa::{PowdrVerifyingKey, RecoveryId, Signature};
use hex_literal::hex;
use k256::EncodedPoint;

// Signature recovery test vectors
struct RecoveryTestVector {
    pk: [u8; 33],
    sig: [u8; 64],
    recid: RecoveryId,
}

const RECOVERY_TEST_VECTORS: &[RecoveryTestVector] = &[
    // Recovery ID 0
    RecoveryTestVector {
        pk: hex!("021a7a569e91dbf60581509c7fc946d1003b60c7dee85299538db6353538d59574"),
        sig: hex!(
            "ce53abb3721bafc561408ce8ff99c909f7f0b18a2f788649d6470162ab1aa032
                 3971edc523a6d6453f3fb6128d318d9db1a5ff3386feb1047d9816e780039d52"
        ),
        recid: RecoveryId::new(false, false),
    },
    // Recovery ID 1
    RecoveryTestVector {
        pk: hex!("036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"),
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
        let digest = [
            173, 132, 205, 11, 16, 252, 2, 135, 56, 151, 27, 7, 129, 36, 174, 194, 160, 231, 198,
            217, 134, 163, 129, 190, 11, 56, 111, 50, 190, 232, 135, 175,
        ];
        let sig = Signature::try_from(vector.sig.as_slice()).unwrap();
        let recid = vector.recid;
        let pk = PowdrVerifyingKey::recover_from_prehash(digest.as_slice(), &sig, recid).unwrap();
        assert_eq!(&vector.pk[..], EncodedPoint::from(&pk.0).as_bytes());
    }
}
