//! Property: deserialization cannot produce a non-canonical field element.
//!
//! For every byte string `b`, `serde` deserialization of a field element from `b`
//! either fails or yields a value whose integer representation is smaller than the
//! field modulus. Every arithmetic operator, `Ord`/`Hash` and the
//! `has_direct_repr()`-licensed transmutes presuppose that invariant, so a
//! non-canonical element read off disk silently corrupts everything downstream.
//!
//! The two fields whose deserializers return a `Result` are covered here:
//! `Bn254Field` (whose `ark_de` hook validates with `Validate::Yes`) and
//! `GoldilocksField` (whose derived newtype `Deserialize` does not validate at all).
//! The plonky3-backed fields (`BabyBearField`, `KoalaBearField`, `Mersenne31Field`)
//! are deliberately out of scope: their `#[serde(from = "u32")]` hook calls
//! `from_canonical_checked(..).unwrap()`, so an out-of-range encoding aborts the
//! process with a panic instead of producing a value that could be inspected.
//!
//! Reproduction:
//! * Counterexamples are persisted in
//!   `number/tests/deserialization_canonicality.proptest-regressions` and are
//!   replayed first on every subsequent run. (proptest prints a
//!   "FileFailurePersistence::SourceParallel set, but failed to find lib.rs or
//!   main.rs" note first: expected for an integration test, it then falls back to
//!   that file next to this source.)
//! * The case count can be raised with `PROPTEST_CASES=100000`.
//! * For a fully deterministic run with a fixed seed, drive the strategies through
//!   an explicit runner instead of the `proptest!` macro:
//!   ```ignore
//!   use proptest::test_runner::{RngAlgorithm, TestRng, TestRunner};
//!   let seed = [0u8; 32]; // any fixed 32 bytes reproduces the same sequence
//!   let mut runner = TestRunner::new_with_rng(
//!       ProptestConfig::default(),
//!       TestRng::from_seed(RngAlgorithm::ChaCha, &seed),
//!   );
//!   ```

use powdr_number::{BigUint, Bn254Field, FieldElement, GoldilocksField, LargeInt};
use proptest::prelude::*;

/// The Goldilocks modulus, spelled out independently of the implementation.
/// `regression_boundary_values` pins it against `GoldilocksField::modulus()`.
const GOLDILOCKS_MODULUS: u64 = 0xffff_ffff_0000_0001;

/// Number of bytes `ark-ff` reads for a 254-bit field element.
const BN254_BYTES: usize = 32;

/// A byte string handed to the deserializer, plus the value it is expected to
/// decode to if (and only if) it is the canonical serialization of an element.
#[derive(Clone, Debug)]
struct Candidate<T> {
    /// `Some(v)` if `bytes` was produced by serializing `v`, in which case
    /// deserialization must succeed and return `v` again.
    expected: Option<T>,
    bytes: Vec<u8>,
}

impl<T> Candidate<T> {
    /// A byte string with no round-trip expectation: it may legitimately be
    /// rejected, but if it is accepted the result must be canonical.
    fn unchecked(bytes: Vec<u8>) -> Self {
        Candidate {
            expected: None,
            bytes,
        }
    }
}

fn to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

/// Deserializes `candidate.bytes` as `T` and returns every way in which the
/// property was violated (empty vector means the property holds for this input).
fn violations<T: FieldElement>(name: &str, candidate: &Candidate<T>) -> Vec<String> {
    let mut found = Vec::new();
    let hex = to_hex(&candidate.bytes);

    match serde_cbor::from_slice::<T>(&candidate.bytes) {
        Ok(value) => {
            // The invariant under test, stated independently of how any field
            // implements deserialization.
            let repr = value.to_arbitrary_integer();
            let modulus = T::modulus().to_arbitrary_integer();
            if repr >= modulus {
                found.push(format!(
                    "{name}: deserializing 0x{hex} produced a non-canonical element: \
                     representation {repr} >= modulus {modulus}"
                ));
            }
            // Positive control: canonical serializations must round-trip, so the
            // property cannot be satisfied by a deserializer that rejects
            // everything.
            if let Some(expected) = &candidate.expected {
                if value != *expected {
                    found.push(format!(
                        "{name}: round-trip of {expected} through 0x{hex} returned {value}"
                    ));
                }
            }
        }
        Err(e) => {
            if let Some(expected) = &candidate.expected {
                found.push(format!(
                    "{name}: canonical serialization 0x{hex} of {expected} was rejected: {e}"
                ));
            }
            // A rejected byte string satisfies the property vacuously.
        }
    }

    found
}

/// CBOR encoding of `GoldilocksField`, which serde derives as a `u64` newtype.
fn goldilocks_encoded(n: u64) -> Vec<u8> {
    serde_cbor::to_vec(&n).unwrap()
}

/// CBOR encoding of `Bn254Field`: a one-entry map whose `value` field holds the
/// byte string that `ark_de` feeds to `CanonicalDeserialize`.
fn bn254_encoded(payload: &[u8]) -> Vec<u8> {
    let map = std::iter::once((
        serde_cbor::Value::Text("value".to_string()),
        serde_cbor::Value::Bytes(payload.to_vec()),
    ))
    .collect();
    serde_cbor::to_vec(&serde_cbor::Value::Map(map)).unwrap()
}

/// Little-endian, zero-padded 32-byte payload for an arbitrary integer.
fn bn254_payload(value: BigUint) -> Vec<u8> {
    let mut bytes = value.to_le_bytes();
    assert!(bytes.len() <= BN254_BYTES, "value too large for Bn254Field");
    bytes.resize(BN254_BYTES, 0);
    bytes
}

/// Goldilocks byte strings: canonical values (which must round-trip),
/// out-of-range values, and the values right at the modulus boundary.
fn goldilocks_candidate() -> impl Strategy<Value = Candidate<GoldilocksField>> {
    prop_oneof![
        3 => 0u64..GOLDILOCKS_MODULUS,
        3 => GOLDILOCKS_MODULUS..=u64::MAX,
        1 => prop_oneof![
            Just(GOLDILOCKS_MODULUS - 1),
            Just(GOLDILOCKS_MODULUS),
            Just(GOLDILOCKS_MODULUS + 1),
            Just(u64::MAX),
        ],
    ]
    .prop_map(|n| Candidate {
        expected: (n < GOLDILOCKS_MODULUS).then(|| GoldilocksField::from(n)),
        bytes: goldilocks_encoded(n),
    })
}

/// Bn254 byte strings: canonical serializations (which must round-trip),
/// arbitrary 32-byte payloads, and payloads at or just above the modulus.
fn bn254_candidate() -> impl Strategy<Value = Candidate<Bn254Field>> {
    prop_oneof![
        3 => any::<u64>().prop_map(|n| {
            let value = Bn254Field::from(n);
            Candidate {
                expected: Some(value),
                bytes: serde_cbor::to_vec(&value).unwrap(),
            }
        }),
        3 => proptest::array::uniform32(any::<u8>())
            .prop_map(|raw| Candidate::unchecked(bn254_encoded(&raw))),
        1 => (0u32..8).prop_map(|offset| {
            let value = Bn254Field::modulus().to_arbitrary_integer() + BigUint::from(offset);
            Candidate::unchecked(bn254_encoded(&bn254_payload(value)))
        }),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 512,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_deserialization_preserves_canonicality(
        goldilocks in goldilocks_candidate(),
        bn254 in bn254_candidate(),
        raw in proptest::collection::vec(any::<u8>(), 0..40),
    ) {
        let mut found = violations("GoldilocksField", &goldilocks);
        found.extend(violations("Bn254Field", &bn254));
        // Wholly unstructured byte strings, to cover the "for every byte string"
        // part of the property.
        found.extend(violations::<GoldilocksField>(
            "GoldilocksField (unstructured bytes)",
            &Candidate::unchecked(raw.clone()),
        ));
        found.extend(violations::<Bn254Field>(
            "Bn254Field (unstructured bytes)",
            &Candidate::unchecked(raw),
        ));

        prop_assert!(found.is_empty(), "{}", found.join("\n"));
    }
}

/// Fixed regression case for the interesting boundary: the modulus itself, which
/// is the smallest non-canonical representation of every field.
#[test]
fn prop_deserialization_preserves_canonicality_regression() {
    assert_eq!(
        GoldilocksField::modulus().try_into_u64(),
        Some(GOLDILOCKS_MODULUS)
    );

    let bn254_modulus = Bn254Field::modulus().to_arbitrary_integer();
    let mut found = Vec::new();

    for n in [
        GOLDILOCKS_MODULUS - 1,
        GOLDILOCKS_MODULUS,
        GOLDILOCKS_MODULUS + 1,
        u64::MAX,
    ] {
        found.extend(violations(
            "GoldilocksField",
            &Candidate {
                expected: (n < GOLDILOCKS_MODULUS).then(|| GoldilocksField::from(n)),
                bytes: goldilocks_encoded(n),
            },
        ));
    }

    for offset in [0u32, 1] {
        let value = bn254_modulus.clone() + BigUint::from(offset);
        found.extend(violations::<Bn254Field>(
            "Bn254Field",
            &Candidate::unchecked(bn254_encoded(&bn254_payload(value))),
        ));
    }

    // Positive control: the largest canonical element of each field round-trips.
    found.extend(violations(
        "GoldilocksField",
        &Candidate {
            expected: Some(GoldilocksField::from(GOLDILOCKS_MODULUS - 1)),
            bytes: goldilocks_encoded(GOLDILOCKS_MODULUS - 1),
        },
    ));
    let bn254_max = Bn254Field::from(0) - Bn254Field::from(1);
    found.extend(violations(
        "Bn254Field",
        &Candidate {
            expected: Some(bn254_max),
            bytes: serde_cbor::to_vec(&bn254_max).unwrap(),
        },
    ));

    assert!(found.is_empty(), "{}", found.join("\n"));
}
