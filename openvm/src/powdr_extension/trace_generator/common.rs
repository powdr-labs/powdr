use openvm_circuit::arch::{AirInventory, AirInventoryError, VmCircuitConfig, VmCircuitExtension};
use openvm_sdk::config::SdkVmConfig;

use crate::BabyBearSC;

pub fn create_dummy_airs<E: VmCircuitExtension<BabyBearSC>>(
    config: &SdkVmConfig,
    shared_chips: E,
) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
    let config = config.to_inner();
    let mut inventory = config.system.create_airs()?;

    // CHANGE: add dummy periphery
    inventory.start_new_extension();
    VmCircuitExtension::extend_circuit(&shared_chips, &mut inventory)?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmCircuitExtension::extend_circuit(rv32i, &mut inventory)?;
    }
    if let Some(io) = &config.io {
        VmCircuitExtension::extend_circuit(io, &mut inventory)?;
    }
    if let Some(keccak) = &config.keccak {
        VmCircuitExtension::extend_circuit(keccak, &mut inventory)?;
    }
    if let Some(sha256) = &config.sha256 {
        VmCircuitExtension::extend_circuit(sha256, &mut inventory)?;
    }
    if let Some(native) = &config.native {
        VmCircuitExtension::extend_circuit(native, &mut inventory)?;
    }
    if let Some(castf) = &config.castf {
        VmCircuitExtension::extend_circuit(castf, &mut inventory)?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmCircuitExtension::extend_circuit(rv32m, &mut inventory)?;
    }
    if let Some(bigint) = &config.bigint {
        VmCircuitExtension::extend_circuit(bigint, &mut inventory)?;
    }
    if let Some(modular) = &config.modular {
        VmCircuitExtension::extend_circuit(modular, &mut inventory)?;
    }
    if let Some(fp2) = &config.fp2 {
        VmCircuitExtension::extend_circuit(fp2, &mut inventory)?;
    }
    if let Some(pairing) = &config.pairing {
        VmCircuitExtension::extend_circuit(pairing, &mut inventory)?;
    }
    if let Some(ecc) = &config.ecc {
        VmCircuitExtension::extend_circuit(ecc, &mut inventory)?;
    }
    Ok(inventory)
}