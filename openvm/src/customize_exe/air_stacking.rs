use std::collections::HashMap;

use powdr_autoprecompiles::{legacy_expression::{AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType}, SymbolicBusInteraction, SymbolicMachine};

use powdr_autoprecompiles::powdr::UniqueColumns;

use powdr_expression::visitors::ExpressionVisitable;

use crate::{powdr_extension::{PowdrPrecompile, PowdrStackedPrecompile}, IntoOpenVm};

use openvm_instructions::LocalOpcode;

use itertools::Itertools;

// perform air stacking of multiple precompiles into stacked precompiles.
pub fn air_stacking<P: IntoOpenVm>(
    mut extensions: Vec<PowdrPrecompile<P>>,
) -> Vec<PowdrStackedPrecompile<P>> {
    assert!(!extensions.is_empty());

    extensions.iter_mut().for_each(compact_ids);

    // create apc groups by number of columns
    let mut groups: HashMap<usize, Vec<PowdrPrecompile<P>>> = Default::default();
    for pcp in extensions {
        let idx = f64::log(pcp.machine.unique_columns().count() as f64, 2.0).floor() as usize;
        groups.entry(idx).or_default().push(pcp);
    }

    let mut result = vec![];

    for mut extensions in groups.into_values() {
        // if there is only one precompile in the group, we can just return it as a stacked precompile
        if extensions.len() == 1 {
            println!("not stacking precompile");
            result.push(PowdrStackedPrecompile {
                machine: SymbolicMachine {
                    constraints: extensions[0].machine.constraints.clone(),
                    bus_interactions: extensions[0].machine.bus_interactions.clone(),
                },
                precompiles: extensions
                    .into_iter()
                    .map(|p| (p.opcode.clone(), p))
                    .collect(),
            });
            continue;
        }

        let mut stacked_constraints = vec![];
        let mut interactions_by_machine = vec![];

        println!("stacking {} precompiles", extensions.len());

        // take the max id in all pcps and add 1.
        let is_valid_start = 1 + extensions.iter()
            .flat_map(|pcp| pcp.original_instructions.iter().flat_map(|instr| instr.subs.iter()))
            .max()
            .unwrap();


        let mut is_valid_sum: Option<AlgebraicExpression<P>> = None;

        for (idx, pcp) in extensions.iter_mut().enumerate() {
            // is_valid columns cannot be shared between precompiles. Here we do
            // their remapping into exclusive columns.
            let is_valid_new_id = is_valid_start + idx as u64;

            println!("\tpcp width: {}", pcp.machine.unique_columns().count());

            // remap is_valid column in constraints and interactions
            let mut remapped = pcp.machine.clone();
            remapped.pre_visit_expressions_mut(&mut |expr| {
                if let AlgebraicExpression::Reference(r) = expr {
                    assert!(r.poly_id.id <= is_valid_start);
                    if r.poly_id == pcp.is_valid_column.id {
                        // we assume each pcp to have a specific column named "is_valid"
                        assert!(r.name == "is_valid");
                        r.poly_id.id = is_valid_new_id;
                        r.name = format!("is_valid_{}", pcp.opcode.global_opcode().as_usize());
                    } else {
                        // we need to rename columns here because `unique_columns` relies on both `Column::name` and `Column::id`
                        r.name = format!("col_{}", r.poly_id.id);
                    }
                }
            });

            // set the is valid column in the original precompile
            pcp.is_valid_column.id.id = is_valid_new_id;

            let is_valid = AlgebraicExpression::Reference(AlgebraicReference {
                name: format!("is_valid_{}", pcp.opcode.global_opcode().as_usize()),
                poly_id: PolyID {
                    id: is_valid_new_id,
                    ptype: PolynomialType::Committed,
                },
                next: false,
            });

            // guard interaction payloads so they can be merged later
            remapped.bus_interactions.iter_mut().for_each(|interaction| {
                interaction.args.iter_mut().for_each(|arg| {
                    *arg = arg.clone() * is_valid.clone();
                });
            });


            is_valid_sum = is_valid_sum
                .map(|sum| sum + is_valid.clone())
                .or_else(|| Some(is_valid.clone()));

            stacked_constraints.extend(remapped.constraints);
            interactions_by_machine.push(remapped.bus_interactions);
        }

        // enforce only one is_valid is active
        let one = AlgebraicExpression::Number(P::ONE);
        let one_hot_is_valid = (one - is_valid_sum.clone().unwrap()) * is_valid_sum.unwrap();

        stacked_constraints.push(one_hot_is_valid.into());

        println!("interaction count before: {}", interactions_by_machine.iter().flatten().count());
        // let stacked_interactions = merge_bus_interactions_simple(interactions_by_machine);
        let stacked_interactions = merge_bus_interactions(interactions_by_machine);

        let machine = SymbolicMachine {
            constraints: stacked_constraints,
            bus_interactions: stacked_interactions,
        };

        println!("interaction count: {}", machine.bus_interactions.len());

        println!("stacked width: {}", machine.unique_columns().count());

        result.push(PowdrStackedPrecompile {
            precompiles: extensions
                .into_iter()
                .map(|p| (p.opcode.clone(), p))
                .collect(),
            machine,
        });
    }

    result
}


/// make PolyIDs start from zero, sequential and compact
fn compact_ids<P: IntoOpenVm>(pcp: &mut PowdrPrecompile<P>) {
    let mut curr_id = 0;
    let mut id_map: HashMap<u64, u64> = Default::default();
    let mut new_poly_id = |old_id: u64| {
        *id_map.entry(old_id).or_insert_with(|| {
            let id = curr_id;
            // println!("remapping poly id {} to {}", old_id, curr_id);
            curr_id += 1;
            id
        })
    };
    pcp.machine.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::Reference(r) = expr {
            if r.poly_id.ptype == PolynomialType::Committed {
                r.poly_id.id = new_poly_id(r.poly_id.id);
            }
        }
    });
    pcp.original_instructions.iter_mut().for_each(|instr| {
        instr.subs.iter_mut().for_each(|sub| {
            *sub = new_poly_id(*sub);
        });
    });

    pcp.is_valid_column.id.id = new_poly_id(pcp.is_valid_column.id.id);
}

fn merge_bus_interactions_simple<P: IntoOpenVm>(interactions: Vec<Vec<SymbolicBusInteraction<P>>>) -> Vec<SymbolicBusInteraction<P>> {
    interactions.into_iter().flatten().collect_vec()
}

fn merge_bus_interactions<P: IntoOpenVm>(interactions_by_machine: Vec<Vec<SymbolicBusInteraction<P>>>) -> Vec<SymbolicBusInteraction<P>> {
    let mut interactions_by_bus: HashMap<_, Vec<Vec<SymbolicBusInteraction<P>>>> = Default::default();

    for interactions in interactions_by_machine {
        let interactions_by_bus_this_machine = interactions.into_iter()
            // we group by bus id and number of args
            .into_group_map_by(|interaction| (interaction.id, interaction.args.len()));
        for (k,v) in interactions_by_bus_this_machine {
            let e = interactions_by_bus.entry(k).or_default();
            e.push(v);
        }
    }
    let mut to_merge = vec![];
    for interactions_per_machine in interactions_by_bus.into_values() {
        // this is doing a "zip longest" among the interactions from each machine
        // (for a given bus), and saving the elements that were picked
        // together to be merged later
        let mut idx = 0;
        loop {
            let mut items = vec![];
            for machine_interactions in interactions_per_machine.iter() {
                if let Some(item) = machine_interactions.get(idx) {
                    items.push(item.clone());
                }
            }
            if items.is_empty() {
                break;
            }
            idx += 1;
            to_merge.push(items);
        }
    }

    to_merge.into_iter().map(|interactions| {
        // assert_eq!(interactions.iter().map(|i| &i.kind).unique().count(), 1);

        // add multiplicities together
        let mut mult = interactions[0].mult.clone();
        for mult2 in interactions.iter().skip(1).map(|i| i.mult.clone()) {
            mult = mult + mult2;
        }

        // add args together
        let mut args = interactions[0].args.clone();
        for args2 in interactions.iter().skip(1).map(|i| i.args.clone()) {
            args = args.into_iter().zip_eq(args2).map(|(a1, a2)| a1 + a2).collect();
        }

        SymbolicBusInteraction {
            id: interactions[0].id,
            mult,
            args,
        }
    }).collect_vec()
}
