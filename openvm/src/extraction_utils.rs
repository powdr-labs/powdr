#[cfg(test)]
mod tests {
    use crate::{ExtendedVmConfig, RiscvISA, DEFAULT_DEGREE_BOUND, DEFAULT_OPENVM_DEGREE_BOUND};

    use openvm_algebra_circuit::{Fp2Extension, ModularExtension};
    use openvm_bigint_circuit::Int256;
    use openvm_circuit::arch::SystemConfig;
    use openvm_ecc_circuit::{WeierstrassExtension, SECP256K1_CONFIG};
    use openvm_pairing_circuit::{PairingCurve, PairingExtension};
    use openvm_rv32im_circuit::Rv32M;
    use openvm_sdk::config::SdkVmConfig;
    use powdr_openvm_common::{
        extraction_utils::{export_pil, OriginalVmConfig},
        SpecializedConfig,
    };
    use powdr_openvm_hints_circuit::HintsExtension;

    #[test]
    fn test_get_bus_map() {
        let use_kzg_intrinsics = true;

        let system_config = SystemConfig::default()
            .with_continuations()
            .with_max_constraint_degree(DEFAULT_OPENVM_DEGREE_BOUND)
            .with_public_values(32);
        let int256 = Int256::default();
        let bn_config = PairingCurve::Bn254.curve_config();
        let bls_config = PairingCurve::Bls12_381.curve_config();
        let rv32m = Rv32M {
            range_tuple_checker_sizes: int256.range_tuple_checker_sizes,
        };
        let mut supported_moduli = vec![
            bn_config.modulus.clone(),
            bn_config.scalar.clone(),
            SECP256K1_CONFIG.modulus.clone(),
            SECP256K1_CONFIG.scalar.clone(),
        ];
        let mut supported_complex_moduli =
            vec![("Bn254Fp2".to_string(), bn_config.modulus.clone())];
        let mut supported_curves = vec![bn_config.clone(), SECP256K1_CONFIG.clone()];
        let mut supported_pairing_curves = vec![PairingCurve::Bn254];
        if use_kzg_intrinsics {
            supported_moduli.push(bls_config.modulus.clone());
            supported_moduli.push(bls_config.scalar.clone());
            supported_complex_moduli.push(("Bls12_381Fp2".to_string(), bls_config.modulus.clone()));
            supported_curves.push(bls_config.clone());
            supported_pairing_curves.push(PairingCurve::Bls12_381);
        }
        let sdk_vm_config = SdkVmConfig::builder()
            .system(system_config.into())
            .rv32i(Default::default())
            .rv32m(rv32m)
            .io(Default::default())
            .keccak(Default::default())
            .sha256(Default::default())
            .bigint(int256)
            .modular(ModularExtension::new(supported_moduli))
            .fp2(Fp2Extension::new(supported_complex_moduli))
            .ecc(WeierstrassExtension::new(supported_curves))
            .pairing(PairingExtension::new(supported_pairing_curves))
            .build();

        let _ = OriginalVmConfig::<RiscvISA>::new(ExtendedVmConfig {
            sdk: sdk_vm_config,
            hints: HintsExtension,
        })
        .bus_map();
    }

    #[test]
    fn test_export_pil() {
        let writer = &mut Vec::new();
        let ext_config = ExtendedVmConfig {
            sdk: SdkVmConfig::riscv32(),
            hints: HintsExtension,
        };
        let base_config = OriginalVmConfig::<RiscvISA>::new(ext_config);
        let specialized_config = SpecializedConfig::new(base_config, vec![], DEFAULT_DEGREE_BOUND);
        export_pil(writer, &specialized_config);
        let output = String::from_utf8(writer.clone()).unwrap();
        assert!(!output.is_empty(), "PIL output should not be empty");
    }
}
