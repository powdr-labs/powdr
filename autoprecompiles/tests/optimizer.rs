use itertools::Itertools;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;
use powdr_pilopt::qse_opt::algebraic_to_quadratic_symbolic_expression;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/machine_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(machine.constraints.len(), 6770);
    assert_eq!(machine.bus_interactions.len(), 3573);
    println!(
        "{}",
        format!(
            "{}",
            machine
                .constraints
                .iter()
                .map(|c| { format!("{}", algebraic_to_quadratic_symbolic_expression(&c.expr)) })
                .format(",\n")
        )
    );
    println!("Machine: {}", machine);
}

// ok, and this horrible big constraint (is_valid) * ((-943718400 * mem_ptr_limbs__0_1 + -7864320 * rs1_data__3_1 + 30720 * mem_ptr_limbs__1_1 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -503316529) * (-943718400 * mem_ptr_limbs__0_1 + -7864320 * rs1_data__3_1 + 30720 * mem_ptr_limbs__1_1 + 943718400 * rs1_data__0_661 + -120 * rs1_data__1_661 + -30720 * rs1_data__2_661 + -503316530)) essentially says mem_ptr_limbs__0_1 + mem_ptr_limbs__1_1 * 65536 = some rs1_data thing modulo 2**32
// ok, I think I'm slowly catching up again
// and what we want to know in this analysis in the end is the differences in these mem pointers
// and the thing is that the expression that appears in the memory bus send is mem_ptr_limbs__0_1 + mem_ptr_limbs__1_1 * 65536
// so what we could do is find the memory bus send, take the expression as a QSE (essentially define a temporary variable x that equals that QSE), find the other constraints where this QSE appears and "solve" them for x.
// this way, we know what gets sent to the memory bus
// and if we solve for x, we get some conditional assignment - it is conditional because of the wrapping.
// oh wait, it's not what we write, it's the address we write to
// and then as soon as we have done this analysis, the next step is to go through the program step by step, looking at all memory reads and writes. And we keep track of what we currently know about memory. If we see a load from an address and we know the value (v) at that address, we remove the load and instead add an equality constraint v = where_to_put_the_value
// If we see a store at an address a (which is a conditional assignment as above), we first need to compare with all the addresses we know about and compute the difference. If the difference is a fixed number larger than the word size, we can keep what we know. Otherwise, we need to delete the knowledge. Finally, we store the new knowledge.
// So what to figure out next: How to compute differences of these conditional assignments
// I think it should work by just computing all 4 combinations and verifying that they are not zero. We should get two different values, a "small" number and that number plus 2**32

//
