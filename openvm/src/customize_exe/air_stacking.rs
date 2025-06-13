use std::collections::{BTreeSet, HashMap};

use powdr_autoprecompiles::{legacy_expression::{AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType}, simplify_expression, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine};

use powdr_autoprecompiles::powdr::UniqueColumns;

use powdr_expression::{visitors::ExpressionVisitable, AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation};

use crate::{powdr_extension::{PowdrPrecompile, PowdrStackedPrecompile}, IntoOpenVm};

use openvm_instructions::LocalOpcode;

use itertools::Itertools;

use bimap::BiMap;

// perform air stacking of multiple precompiles into stacked precompiles.
pub fn air_stacking<P: IntoOpenVm>(
    mut extensions: Vec<PowdrPrecompile<P>>,
) -> Vec<PowdrStackedPrecompile<P>> {
    assert!(!extensions.is_empty());

    extensions.iter_mut().for_each(|ext| {
        ext.machine.constraints.iter_mut().for_each(|c| canonicalize_expression(&mut c.expr));
        // ext.machine.constraints.sort();
        // compact_ids(ext)
    });

    // create apc groups by number of columns
    let mut groups: HashMap<usize, Vec<PowdrPrecompile<P>>> = Default::default();
    for pcp in extensions {
        let idx = f64::log(pcp.machine.unique_columns().count() as f64, 1.1).floor() as usize;
        groups.entry(idx).or_default().push(pcp);
    }

    // sort each group by number of columns
    groups.values_mut().for_each(|g| {
        // assign largest pcp first
        g.sort_by(|pcp1, pcp2| pcp2.machine.unique_columns().count().cmp(&pcp1.machine.unique_columns().count()));
        let mut column_assigner = ColumnAssigner::default();
        g.iter_mut().for_each(|ext| {
            column_assigner.assign_pcp(ext);
        });
    });

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

        println!("before joining constraints: {}", stacked_constraints.len());
        let mut stacked_constraints = join_constraints(stacked_constraints);
        stacked_constraints.sort();
        println!("after joining constraints: {}", stacked_constraints.len());
        println!("max degree constraints: {}", stacked_constraints.iter().map(|c| c.expr.degree()).max().unwrap_or(0));

        // enforce only one is_valid is active
        let one = AlgebraicExpression::Number(P::ONE);
        let one_hot_is_valid = (one - is_valid_sum.clone().unwrap()) * is_valid_sum.unwrap();

        stacked_constraints.push(one_hot_is_valid.into());

        println!("interaction count before: {}", interactions_by_machine.iter().flatten().count());
        // let mut stacked_interactions = merge_bus_interactions_simple(interactions_by_machine);
        // let mut stacked_interactions = merge_bus_interactions(interactions_by_machine);
        let mut stacked_interactions = merge_bus_interactions2(interactions_by_machine);
        stacked_interactions.sort();

        let machine = SymbolicMachine {
            constraints: stacked_constraints,
            bus_interactions: stacked_interactions,
        };

        println!("interaction count: {}", machine.bus_interactions.len());
        println!("max degree interaction args: {}", machine.bus_interactions.iter()
            .map(|i| i.args.iter().map(|a| a.degree()).max().unwrap_or(0))
            .max()
                 .unwrap_or(0));
        println!("max degree interaction multiplicity:{}", machine.bus_interactions.iter()
            .map(|i| i.mult.degree())
            .max()
                 .unwrap_or(0));

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

fn merge_bus_interactions2<P: IntoOpenVm>(
    interactions_by_machine: Vec<Vec<SymbolicBusInteraction<P>>>
) -> Vec<SymbolicBusInteraction<P>> {
    // split interactions by bus/args len
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
    interactions_by_bus.values_mut().for_each(|interactions_by_machine| {
        interactions_by_machine.sort_by_key(|interactions| interactions.len());
    });

    let mut result = vec![];
    for mut interactions_by_machine in interactions_by_bus.into_values() {
        // to_merge is a vec of vecs, each inner vec is a set of interactions to be merged
        let mut to_merge = interactions_by_machine.pop().unwrap().into_iter().map(|i| vec![i]).collect_vec();
        for machine_interactions in interactions_by_machine {
            let mut used = BTreeSet::new();
            let mut try_partial_match = vec![];
            'outer: for i in machine_interactions {
                // try to find a call with an exact match
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if used.contains(&idx) {
                        continue;
                    }
                    let i2 = to_merge_set.get(0).unwrap();
                    let all_args_same_structure = i.args.iter()
                        .zip_eq(i2.args.iter())
                        .all(|(a1, a2)| has_same_structure(strip_is_valid(&a1),
                                                           strip_is_valid(&a2)));
                    if all_args_same_structure {
                        println!("EXACT MATCH");
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                try_partial_match.push(i);
            }

            let mut no_match = vec![];
            'outer: for i in try_partial_match {
                // didn't find exact match, try one where some args match
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if used.contains(&idx) {
                        continue;
                    }
                    // check all args have the same structure
                    let i2 = to_merge_set.get(0).unwrap();
                    let some_args_same_structure = i.args.iter()
                        .zip_eq(i2.args.iter())
                        .any(|(a1, a2)| has_same_structure(strip_is_valid(&a1),
                                                           strip_is_valid(&a2)));
                    if some_args_same_structure {
                        println!("PARTIAL MATCH");
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                no_match.push(i);
            }

            'outer: for i in no_match {
                // just pick the first unused one
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if !used.contains(&idx) {
                        println!("NO MATCH");
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                unreachable!("could not find any bus interactions to merge with, but should");
            }
        }

        fn merge_args<P: IntoOpenVm>(arg1: AlgebraicExpression<P>, arg2: AlgebraicExpression<P>) -> AlgebraicExpression<P> {
            match arg1 {
                // expr * is_valid
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left: left1, op: AlgebraicBinaryOperator::Mul, right: is_valid1 }) => {
                    let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left: left2, op: AlgebraicBinaryOperator::Mul, right: is_valid2 }) = arg2 else {
                        panic!("Expected binary operation for arg2, got: {arg2}");
                    };
                    if has_same_structure(&left1, &left2) {
                        *left1 * (*is_valid1 + *is_valid2)
                    } else {
                        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                            left: left1,
                            op: AlgebraicBinaryOperator::Mul,
                            right: is_valid1,
                        })
                            +
                        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                            left: left2,
                            op: AlgebraicBinaryOperator::Mul,
                            right: is_valid2,
                        })
                    }
                }
                // exprA * is_validA + exprB * is_validB + ...
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left:_, op: AlgebraicBinaryOperator::Add, right:_ }) => {
                    arg1 + arg2
                }
                _ => unreachable!()
            }
        }

        fn simplify_arg<P: IntoOpenVm>(arg: AlgebraicExpression<P>) -> AlgebraicExpression<P> {
            // expr * is_valid_expr
            if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op: AlgebraicBinaryOperator::Mul, right: _is_valid_expr }) = arg {
                *left
            } else {
                // sum of expr * is_validN, can't simplify
                arg
            }
        }

        // merge each set of interactions
        for set in to_merge {
            let id = set[0].id;
            let mult = simplify_expression(set.iter().map(|i| i.mult.clone()).reduce(|a, b| a + b).unwrap());
            let args = set.into_iter().map(|i| i.args)
                .reduce(|a, b| a.into_iter().zip_eq(b).map(|(a1, a2)| merge_args(a1, a2)).collect())
                .unwrap();
            let args = args.into_iter().map(|arg| simplify_expression(simplify_arg(arg))).collect_vec();
            result.push(SymbolicBusInteraction {
                id,
                args,
                mult,
            });
        }
    }

    result
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

fn canonicalize_expression<P: IntoOpenVm>(expr: &mut AlgebraicExpression<P>) {
    expr.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) = expr {
            if *op == AlgebraicBinaryOperator::Mul || *op == AlgebraicBinaryOperator::Add && left > right {
                std::mem::swap(left, right);
            }
        }
    });
}

/// assumes ALL constraints are of the form `Y * is_valid_expr`
fn join_constraints<P: IntoOpenVm>(mut constraints: Vec<SymbolicConstraint<P>>) -> Vec<SymbolicConstraint<P>> {
    constraints.sort();
    let mut result: Vec<SymbolicConstraint<P>> = vec![];
    'outer: for c1 in constraints {
        for c2 in result.iter_mut() {
            if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {left: l1, op: AlgebraicBinaryOperator::Mul, right: is_valid1}) = &c1.expr {
                if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {left: l2, op: AlgebraicBinaryOperator::Mul, right: is_valid2}) = &mut c2.expr {
                    if l1 == l2 {
                        *is_valid2 = Box::new(*is_valid1.clone() + *is_valid2.clone());
                        continue 'outer;
                    }
                }
            }
        }
        result.push(c1);
    }
    result
}

fn has_same_structure<P: IntoOpenVm>(expr1: &AlgebraicExpression<P>, expr2: &AlgebraicExpression<P>) -> bool {
    match (expr1, expr2) {
        (AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation{ left: left1, op: op1, right: right1 }),
         AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation{ left: left2, op: op2, right: right2 })) => {
            op1 == op2 && has_same_structure(left1, left2) && has_same_structure(right1, right2)
        }
        (AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op: op1, expr: expr1 }),
         AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op: op2, expr: expr2 })) => {
            op1 == op2 && has_same_structure(expr1, expr2)
        }
        (AlgebraicExpression::Reference(r1), AlgebraicExpression::Reference(r2)) => {
            r1.poly_id.id == r2.poly_id.id
        },
        (AlgebraicExpression::Number(n1), AlgebraicExpression::Number(n2)) => n1 == n2,
        _ => false
    }
}

// assumes expression is of the form `some_expr * is_valid_expr`.
fn strip_is_valid<P: IntoOpenVm>(expr: &AlgebraicExpression<P>) -> &AlgebraicExpression<P> {
    let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op: AlgebraicBinaryOperator::Mul, right: _ }) = expr else {
        panic!("Expression is not a binary operation with Mul operator: {expr}");
    };
    left.as_ref()
}


fn create_mapping<P: IntoOpenVm>(from: &AlgebraicExpression<P>, to: &AlgebraicExpression<P>) -> BiMap<u64, u64> {
    fn create_mapping_inner<P: IntoOpenVm>(from: &AlgebraicExpression<P>, to: &AlgebraicExpression<P>) -> BiMap<u64, u64> {
        match (from, to) {
            (AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left: left1, op: op1, right: right1 }),
             AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left: left2, op: op2, right: right2 })) => {
                assert_eq!(op1, op2);
                let mut left = create_mapping_inner(left1, left2);
                let right = create_mapping_inner(right1, right2);
                // assert both sides don't conflict
                assert!(extend_if_no_conflicts(&mut left, right), "conflict in mapping: {from} => {to}");
                left
            }
            (AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op: op1, expr: expr1 }),
             AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op: op2, expr: expr2 })) => {
                assert_eq!(op1, op2);
                create_mapping_inner(&expr1, &expr2)
            }
            (AlgebraicExpression::Reference(from), AlgebraicExpression::Reference(to)) => {
                let mut mappings = BiMap::new();
                mappings.insert(from.poly_id.id, to.poly_id.id);
                mappings
            }
            _ => BiMap::new(),
        }
    }

    create_mapping_inner(from, to)
}

fn extend_if_no_conflicts(
    mappings: &mut BiMap<u64, u64>,
    new_mappings: BiMap<u64, u64>,
) -> bool {
    for (k, v) in &new_mappings {
        if let Some(existing_v) = mappings.get_by_left(k) {
            if existing_v != v {
                return false; // conflict found
            }
        }
        if let Some(existing_k) = mappings.get_by_right(v) {
            if existing_k != k {
                return false; // conflict found
            }
        }
    }
    mappings.extend(new_mappings);
    true
}

/// changes poly_id to match the order in which they are encountered in the expression.
/// This allows us to compare two expressions for the same "structure".
/// e.g., a+a == b+b and a+a != a+b
fn expr_poly_id_by_order<P: IntoOpenVm>(mut expr: AlgebraicExpression<P>) -> AlgebraicExpression<P> {
    let mut curr_id = 0;
    let mut id_map: HashMap<u64, u64> = Default::default();
    let mut new_poly_id = |old_id: u64| {
        *id_map.entry(old_id).or_insert_with(|| {
            let id = curr_id;
            curr_id += 1;
            id
        })
    };
    expr.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            r.poly_id.id = new_poly_id(r.poly_id.id);
            r.name = format!("col_{}", r.poly_id.id); // this one not needed, just for printing
        }
    });
    expr
}

fn expr_orig_poly_ids<P: IntoOpenVm>(mut expr: AlgebraicExpression<P>) -> AlgebraicExpression<P> {
    expr.pre_visit_expressions_mut(&mut |e| {
        if let AlgebraicExpression::Reference(r) = e {
            r.name = format!("col_{}", r.poly_id.id); // this one not needed, just for printing
        }
    });
    expr
}

#[derive(Default)]
struct ColumnAssigner<P: IntoOpenVm> {
    // original PCPs
    pcps: Vec<PowdrPrecompile<P>>,
}

impl<P: IntoOpenVm> ColumnAssigner<P> {
    fn assign_pcp(
        &mut self,
        pcp: &mut PowdrPrecompile<P>,
    ) {
        let idx = self.pcps.len();
        println!("Assinging PCP {}", idx);
        let mut mappings = BiMap::new();
        // for each constraint, we want to find if there is a constraint in
        // another machine with the same structure that we can assign the same
        // ids.
        // There may be multiple such constraints, so we try them all (as some
        // of the ids in the current constraint may already be assigned)

        'outer: for c in &pcp.machine.constraints {
            for pcp2 in self.pcps.iter() {
                for c2 in &pcp2.machine.constraints {
                    if has_same_structure(&expr_poly_id_by_order(c.expr.clone()),
                                          &expr_poly_id_by_order(c2.expr.clone())) {
                        println!("Found same structure: \n\t{}\n\n\t{}",
                                 &expr_poly_id_by_order(c.expr.clone()),
                                 &expr_poly_id_by_order(c2.expr.clone()));

                        // println!("Found same structure (ids): \n\t{}\n\n\t{}",
                        //          &expr_orig_poly_ids(c.expr.clone()),
                        //          &expr_orig_poly_ids(c2.expr.clone()));

                        // assign the same ids
                        let new_mappings = create_mapping(&c.expr, &c2.expr);
                        if extend_if_no_conflicts(
                            &mut mappings,
                            new_mappings,
                        ) {
                            // println!("\tMappings: {:#?}", mappings);
                            continue 'outer;
                        }
                        println!("\tCould not extend due to conflicts!");
                    }
                }
            }
        }

        // do the same for bus interactions
        'outer: for b in &pcp.machine.bus_interactions {
            for pcp2 in self.pcps.iter() {
                for b2 in &pcp2.machine.bus_interactions {
                    if b.id == b2.id && b.args.len() == b2.args.len() {
                        let all_args_same_structure = b.args.iter()
                            .zip_eq(b2.args.iter())
                            .all(|(a1, a2)| has_same_structure(&expr_poly_id_by_order(a1.clone()), &expr_poly_id_by_order(a2.clone())));
                        if all_args_same_structure {
                            for (arg1, arg2) in b.args.iter().zip_eq(b2.args.iter()) {
                                let new_mappings = create_mapping(&arg1, &arg2);
                                extend_if_no_conflicts(
                                    &mut mappings,
                                    new_mappings,
                                );
                            }
                            continue 'outer;
                        }
                    }
                }
            }
        }

        println!("Mappings for PCP {}: {:#?}", idx, mappings);

        // now assign new ids for all references in the PCP
        let mut curr_id = 0;
        let mut new_poly_id = |old_id: u64| {
            if let Some(id) = mappings.get_by_left(&old_id) {
                return *id;
            }
            // if the new id is a target in the mappings, find a new id not yet in the mapping
            while mappings.get_by_right(&curr_id).is_some() {
                curr_id += 1;
            }
            assert!(mappings.get_by_right(&curr_id).is_none(), "New id {} already exists in mappings: {:?}", curr_id, mappings);
            assert!(!mappings.contains_left(&old_id));
            // println!("Assigning new id {} for old id {}", curr_id, old_id);
            let id = curr_id;
            mappings.insert(old_id, id);
            curr_id += 1;
            id
        };

        pcp.machine.pre_visit_expressions_mut(&mut |expr| {
            if let AlgebraicExpression::Reference(r) = expr {
                if r.poly_id.ptype == PolynomialType::Committed {
                    r.poly_id.id = new_poly_id(r.poly_id.id);
                }
            }
        });
        // println!("----- assigning instr subs ------");
        pcp.original_instructions.iter_mut().for_each(|instr| {
            instr.subs.iter_mut().for_each(|sub| {
                *sub = new_poly_id(*sub);
            });
        });

        // println!("----- assigning is valid ------");
        pcp.is_valid_column.id.id = new_poly_id(pcp.is_valid_column.id.id);

        self.pcps.push(pcp.clone());
    }
}
