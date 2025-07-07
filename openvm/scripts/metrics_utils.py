#!/usr/bin/env python3

import sys
import json
import re
import pandas as pd

def load_metrics_dataframes(filename):
    """Load metrics JSON file and return app, leaf, and internal dataframes.
    
    Each dataframe has a "metric" and "value" column, along with optional columns
    like "air_name", or "segment".
    """
    with open(filename) as f:
        metrics_json = json.load(f)

    entries = [
        dict(c["labels"]) | { "metric": c["metric"], "value": c["value"] }
        for c in metrics_json["counter"] + metrics_json["gauge"]
    ]

    df = pd.DataFrame(entries)

    # "group" has different values if coming from reth benchmark or the powdr cli
    app = df[df["group"].fillna('').str.startswith("app_proof")]
    if len(app) == 0:
        app = df[df["group"].fillna('').str.startswith("reth")]
    if len(app) == 0:
        print("Invalid metrics.json", file=sys.stderr)
        exit(1)

    leaf = df[df["group"].fillna('').str.startswith("leaf")]
    internal = df[df["group"].fillna('').str.startswith("internal")]
    
    return app, leaf, internal

def is_normal_instruction_air(air_name):
    """Check if an AIR name represents a normal RISC-V instruction.
    
    Rules:
    - Must be a VmAirWrapper<Adapter, Core>
    - If the core chip is FieldExpressionCoreAir, return False
    - If the core chip has numeric parameters and first one (number of limbs) is not 4, return False
    - Otherwise return True
    """
    # Match VmAirWrapper<Adapter, Core> pattern
    match = re.match(r'^VmAirWrapper<[^,]+,\s*([^>]+?)(?:<(\d+)(?:,\s*\d+)*>)?\s*>$', air_name)
    
    if not match:
        return False
    
    core_name = match.group(1)
    num_limbs = match.group(2)
    
    if "FieldExpressionCoreAir" == core_name:
        return False
    if num_limbs and int(num_limbs) != 4:
        return False
    
    return True

def test_is_normal_instruction_air():
    # Test cases from the reth benchmark
    assert is_normal_instruction_air("VmAirWrapper<Rv32LoadStoreAdapterAir, LoadStoreCoreAir<4>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32BaseAluAdapterAir, BaseAluCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32BaseAluAdapterAir, ShiftCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32BranchAdapterAir, BranchEqualCoreAir<4>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32JalrAdapterAir, Rv32JalrCoreAir>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32BaseAluAdapterAir, LessThanCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32CondRdWriteAdapterAir, Rv32JalLuiCoreAir>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32BranchAdapterAir, BranchLessThanCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32RdWriteAdapterAir, Rv32AuipcCoreAir>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32MultAdapterAir, MultiplicationCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32MultAdapterAir, MulHCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32LoadStoreAdapterAir, LoadSignExtendCoreAir<4, 8>>")
    assert is_normal_instruction_air("VmAirWrapper<Rv32MultAdapterAir, DivRemCoreAir<4, 8>>")
    
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<1, 2, 2, 32, 32>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<2, 2, 2, 32, 32>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<2, 6, 6, 16, 16>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<2, 1, 1, 32, 32>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<2, 3, 3, 16, 16>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32VecHeapAdapterAir<1, 6, 6, 16, 16>, FieldExpressionCoreAir>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32IsEqualModAdapterAir<2, 1, 32, 32>, ModularIsEqualCoreAir<32, 4, 8>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32HeapAdapterAir<2, 32, 32>, BaseAluCoreAir<32, 8>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32HeapBranchAdapterAir<2, 32>, BranchEqualCoreAir<32>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32HeapAdapterAir<2, 32, 32>, ShiftCoreAir<32, 8>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32HeapAdapterAir<2, 32, 32>, MultiplicationCoreAir<32, 8>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32HeapAdapterAir<2, 32, 32>, LessThanCoreAir<32, 8>>")
    assert not is_normal_instruction_air("VmAirWrapper<Rv32IsEqualModAdapterAir<2, 3, 16, 48>, ModularIsEqualCoreAir<48, 4, 8>>")
    assert not is_normal_instruction_air("KeccakVmAir")
    assert not is_normal_instruction_air("PowdrAir<BabyBearField>")
    assert not is_normal_instruction_air("Poseidon2PeripheryAir<BabyBearParameters>, 1>")
    assert not is_normal_instruction_air("MemoryMerkleAir<8>")
    assert not is_normal_instruction_air("AccessAdapterAir<8>")
    assert not is_normal_instruction_air("PersistentBoundaryAir<8>")
    assert not is_normal_instruction_air("Rv32HintStoreAir")
    assert not is_normal_instruction_air("AccessAdapterAir<16>")
    assert not is_normal_instruction_air("RangeTupleCheckerAir<2>")
    assert not is_normal_instruction_air("ProgramAir")
    assert not is_normal_instruction_air("AccessAdapterAir<32>")
    assert not is_normal_instruction_air("AccessAdapterAir<2>")
    assert not is_normal_instruction_air("AccessAdapterAir<4>")
    assert not is_normal_instruction_air("VariableRangeCheckerAir")
    assert not is_normal_instruction_air("BitwiseOperationLookupAir<8>")
    assert not is_normal_instruction_air("PhantomAir")
    assert not is_normal_instruction_air("VmConnectorAir")
