use powdr_autoprecompiles::{constraint_optimizer, SymbolicMachine};
use powdr_autoprecompiles::{optimizer::optimize, DegreeBound};
use powdr_constraint_solver::constraint_system::{BusInteraction, ConstraintSystem};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::solver::{new_solver, Solver};
use powdr_number::BabyBearField;
use powdr_openvm::BabyBearOpenVmApcAdapter;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};

use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.main_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [27194, 13167, 27689]
    );
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::new(default_openvm_bus_map()),
        DegreeBound {
            identities: 5,
            bus_interactions: 5,
        },
        &default_openvm_bus_map(),
    )
    .unwrap();

    println!(
        "Columns: {}, bus interactions: {}, constraints: {}",
        machine.main_columns().count(),
        machine.bus_interactions.len(),
        machine.constraints.len()
    );

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.main_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [2007, 1780, 165]
    );
}

#[test]
fn test_linearizer() {
    fn v(name: &'static str) -> GroupedExpression<BabyBearField, &'static str> {
        GroupedExpression::from_unknown_variable(name)
    }
    fn c(value: u32) -> GroupedExpression<BabyBearField, &'static str> {
        GroupedExpression::from_number(value.into())
    }
    // (opcode_beq_flag_0) * (opcode_beq_flag_0 - 1) = 0
    // (opcode_bne_flag_0) * (opcode_bne_flag_0 - 1) = 0
    // (opcode_beq_flag_0 + opcode_bne_flag_0) * (opcode_beq_flag_0 + opcode_bne_flag_0 - 1) = 0
    // (cmp_result_0) * (cmp_result_0 - 1) = 0
    // ((cmp_result_0) * (opcode_beq_flag_0) - (cmp_result_0 - 1) * (opcode_bne_flag_0)) * (a__0_0 - b__0_0) = 0
    // ((cmp_result_0) * (opcode_beq_flag_0) - (cmp_result_0 - 1) * (opcode_bne_flag_0)) * (a__1_0 - b__1_0) = 0
    // ((cmp_result_0) * (opcode_beq_flag_0) - (cmp_result_0 - 1) * (opcode_bne_flag_0)) * (a__2_0 - b__2_0) = 0
    // ((cmp_result_0) * (opcode_beq_flag_0) - (cmp_result_0 - 1) * (opcode_bne_flag_0)) * (a__3_0 - b__3_0) = 0
    // (opcode_beq_flag_0 + opcode_bne_flag_0) * ((cmp_result_0) * (opcode_beq_flag_0) - (cmp_result_0 - 1) * (opcode_bne_flag_0) + (a__0_0 - b__0_0) * (diff_inv_marker__0_0) + (a__1_0 - b__1_0) * (diff_inv_marker__1_0) + (a__2_0 - b__2_0) * (diff_inv_marker__2_0) + (a__3_0 - b__3_0) * (diff_inv_marker__3_0) - 1) = 0
    // (opcode_beq_flag_0 + opcode_bne_flag_0) * (from_state__timestamp_0 - reads_aux__0__base__prev_timestamp_0 - reads_aux__0__base__timestamp_lt_aux__lower_decomp__0_0 - 131072 * reads_aux__0__base__timestamp_lt_aux__lower_decomp__1_0 - 1) = 0
    // (opcode_beq_flag_0 + opcode_bne_flag_0) * (from_state__timestamp_0 - reads_aux__1__base__prev_timestamp_0 - reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_0 - 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_0) = 0
    // -(opcode_beq_flag_0 + opcode_bne_flag_0 - 1) = 0
    // from_state__pc_0 = 0
    // opcode_bne_flag_0 - 1 = 0
    // rs1_ptr_0 - 5 = 0
    // rs2_ptr_0 = 0
    // imm_0 - 8 = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(0, 1) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(1, 1) = 0
    // -(opcode_beq_flag_0 + opcode_bne_flag_0 + BusInteractionField(2, 1)) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(3, 1) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(4, 1) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(5, 1) = 0
    // -(opcode_beq_flag_0 + opcode_bne_flag_0 + BusInteractionField(6, 1)) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(7, 1) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(8, 1) = 0
    // -(opcode_beq_flag_0 + opcode_bne_flag_0 + BusInteractionField(9, 1)) = 0
    // opcode_beq_flag_0 + opcode_bne_flag_0 - BusInteractionField(10, 1) = 0
    // (cmp_result_0) * (imm_0) + from_state__pc_0 - 4 * cmp_result_0 - BusInteractionField(10, 2) + 4 = 0
    let algebraic_constraints = vec![
        v("cmp_result_0") * (v("cmp_result_0") - c(1)),
        (-(v("cmp_result_0") - c(1))) * (v("a__0_0") - v("b__0_0")),
        (-(v("cmp_result_0") - c(1))) * (v("a__1_0") - v("b__1_0")),
        (-(v("cmp_result_0") - c(1))) * (v("a__2_0") - v("b__2_0")),
        (-(v("cmp_result_0") - c(1))) * (v("a__3_0") - v("b__3_0")),
        (-(v("cmp_result_0") - c(1))
            + (v("a__0_0") - v("b__0_0")) * v("diff_inv_marker__0_0")
            + (v("a__1_0") - v("b__1_0")) * v("diff_inv_marker__1_0")
            + (v("a__2_0") - v("b__2_0")) * v("diff_inv_marker__2_0")
            + (v("a__3_0") - v("b__3_0")) * v("diff_inv_marker__3_0")
            - c(1)),
        v("imm_0") - c(8),
        v("cmp_result_0") * v("imm_0") - c(4) * v("cmp_result_0") - v("BusInteractionField(10, 2)")
            + c(4),
    ];
    let bus_interactions = vec![
        BusInteraction {
            bus_id: c(1),
            multiplicity: -c(1),
            payload: vec![
                c(1),
                v("rs2_ptr_0"),
                v("b__0_0"),
                v("b__1_0"),
                v("b__2_0"),
                v("b__3_0"),
                v("reads_aux__1__base__prev_timestamp_0"),
            ],
        },
        BusInteraction {
            bus_id: c(1),
            multiplicity: c(1),
            payload: vec![
                c(1),
                v("rs2_ptr_0"),
                v("b__0_0"),
                v("b__1_0"),
                v("b__2_0"),
                v("b__3_0"),
                v("from_state__timestamp_0") + c(1),
            ],
        },
        BusInteraction {
            bus_id: c(2),
            multiplicity: c(1),
            payload: vec![
                c(0),
                c(1) + c(544),
                v("rs1_ptr_0"),
                v("rs2_ptr_0"),
                v("imm_0"),
                c(1),
                c(1),
                c(0),
                c(0),
            ],
        },
        BusInteraction {
            bus_id: c(0),
            multiplicity: -c(1),
            payload: vec![c(0), v("from_state__timestamp_0")],
        },
        BusInteraction {
            bus_id: c(0),
            multiplicity: c(1),
            payload: vec![
                v("BusInteractionField(10, 2)"),
                v("from_state__timestamp_0") + c(2),
            ],
        },
    ];
    // BusInteraction { bus_id: 3, multiplicity: BusInteractionField(0, 1), payload: reads_aux__0__base__timestamp_lt_aux__lower_decomp__0_0, 17 }
    // BusInteraction { bus_id: 3, multiplicity: BusInteractionField(1, 1), payload: reads_aux__0__base__timestamp_lt_aux__lower_decomp__1_0, 12 }
    // BusInteraction { bus_id: 1, multiplicity: BusInteractionField(2, 1), payload: 1, rs1_ptr_0, a__0_0, a__1_0, a__2_0, a__3_0, reads_aux__0__base__prev_timestamp_0 }
    // BusInteraction { bus_id: 1, multiplicity: BusInteractionField(3, 1), payload: 1, rs1_ptr_0, a__0_0, a__1_0, a__2_0, a__3_0, from_state__timestamp_0 }
    // BusInteraction { bus_id: 3, multiplicity: BusInteractionField(4, 1), payload: reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_0, 17 }
    // BusInteraction { bus_id: 3, multiplicity: BusInteractionField(5, 1), payload: reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_0, 12 }
    // BusInteraction { bus_id: 1, multiplicity: BusInteractionField(6, 1), payload: 1, rs2_ptr_0, b__0_0, b__1_0, b__2_0, b__3_0, reads_aux__1__base__prev_timestamp_0 }
    // BusInteraction { bus_id: 1, multiplicity: BusInteractionField(7, 1), payload: 1, rs2_ptr_0, b__0_0, b__1_0, b__2_0, b__3_0, from_state__timestamp_0 + 1 }
    // BusInteraction { bus_id: 2, multiplicity: BusInteractionField(8, 1), payload: from_state__pc_0, opcode_bne_flag_0 + 544, rs1_ptr_0, rs2_ptr_0, imm_0, 1, 1, 0, 0 }
    // BusInteraction { bus_id: 0, multiplicity: BusInteractionField(9, 1), payload: from_state__pc_0, from_state__timestamp_0 }
    // BusInteraction { bus_id: 0, multiplicity: BusInteractionField(10, 1), payload: BusInteractionField(10, 2), from_state__timestamp_0 + 2 }

    let constraint_system = ConstraintSystem {
        algebraic_constraints,
        bus_interactions,
    };
    println!("Constraint system:\n{constraint_system}");
    let assignments = new_solver(
        constraint_system,
        OpenVmBusInteractionHandler::new(default_openvm_bus_map()),
    )
    .solve()
    .unwrap();
    println!("\n===============\n");
    for (var, value) in assignments {
        //if var == "cmp_result_0" {
        println!("{var}: {value}");
        //}
    }
}
