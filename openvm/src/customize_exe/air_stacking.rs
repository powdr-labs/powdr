use std::collections::{BTreeSet, HashMap};

use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    powdr::make_bool,
    simplify_expression, SymbolicBusInteraction, SymbolicConstraint, SymbolicMachine,
};

use powdr_autoprecompiles::powdr::UniqueReferences;

use powdr_expression::{
    visitors::ExpressionVisitable, AlgebraicBinaryOperation, AlgebraicBinaryOperator,
    AlgebraicUnaryOperation,
};

use crate::{
    powdr_extension::{PowdrPrecompile, PowdrStackedPrecompile},
    IntoOpenVm,
};

use openvm_instructions::LocalOpcode;

use itertools::Itertools;

use bimap::BiMap;

/// Perform air stacking on the set of precompiles, grouping them by the log of
/// their number of columns.
pub fn air_stacking<P: IntoOpenVm>(
    mut extensions: Vec<PowdrPrecompile<P>>,
    chip_stacking_log: f32,
) -> Vec<PowdrStackedPrecompile<P>> {
    assert!(!extensions.is_empty());

    // we normalize expressions in constraints and bus arguments so that they
    // can later be compared for having the same structure
    extensions.iter_mut().for_each(|ext| {
        ext.machine.constraints.iter_mut().for_each(|c| {
            normalize_guarded(&mut c.expr);
        });

        ext.machine.bus_interactions.iter_mut().for_each(|i| {
            i.args.iter_mut().for_each(|arg| {
                *arg = arg.clone().normalize();
            });
        });
    });

    // group precompiles by number of columns
    let mut groups: HashMap<usize, Vec<PowdrPrecompile<P>>> = Default::default();
    for pcp in extensions {
        let idx = f32::log(
            pcp.machine.unique_references().count() as f32,
            chip_stacking_log,
        )
        .floor() as usize;
        groups.entry(idx).or_default().push(pcp);
    }

    groups.into_values().map(group_into_stacked).collect_vec()
}

/// Takes a group of precompiles and stacks them into a single stacked precompile.
fn group_into_stacked<P: IntoOpenVm>(
    mut group: Vec<PowdrPrecompile<P>>,
) -> PowdrStackedPrecompile<P> {
    // group has a single precompile, no stacking
    if group.len() == 1 {
        let precompile = group.pop().unwrap();
        tracing::debug!(
            "Precompile {} not stacked",
            &precompile.opcode.global_opcode().as_usize()
        );
        return PowdrStackedPrecompile::new_single(precompile);
    }

    // handle largest precompile first
    group.sort_by(|pcp1, pcp2| {
        pcp2.machine
            .unique_references()
            .count()
            .cmp(&pcp1.machine.unique_references().count())
    });

    // assign columns to the precompiles in this group
    let mut column_assigner = ColumnAssigner::default();
    group.iter_mut().for_each(|ext| {
        column_assigner.assign_precompile(ext);
    });

    tracing::debug!("Stacking {} precompiles", group.len());

    // take the max id in all pcps and add 1.
    let is_valid_start = 1 + group
        .iter()
        .flat_map(|pcp| {
            pcp.original_instructions
                .iter()
                .flat_map(|instr| instr.subs.iter())
        })
        .max()
        .unwrap();

    let mut is_valid_sum: Option<AlgebraicExpression<P>> = None;
    let mut stacked_constraints = vec![];
    let mut interactions_by_machine = vec![];

    for (idx, pcp) in group.iter_mut().enumerate() {
        // is_valid columns cannot be shared between precompiles. Here we do
        // their remapping into exclusive columns.
        let is_valid_new_id = is_valid_start + idx as u64;

        // remap is_valid column in constraints and interactions
        let mut remapped = pcp.machine.clone();
        remapped.pre_visit_expressions_mut(&mut |expr| {
            if let AlgebraicExpression::Reference(r) = expr {
                assert!(r.id <= is_valid_start);
                if r.id == pcp.is_valid_column.id {
                    // we assume each pcp to have a specific column named "is_valid"
                    assert!(*r.name == "is_valid");
                    r.id = is_valid_new_id;
                    r.name = format!("is_valid_{}", pcp.opcode.global_opcode().as_usize()).into();
                } else {
                    r.name = format!("col_{}", r.id).into();
                }
            }
        });

        // set the is valid column in the original precompile
        pcp.is_valid_column.id = is_valid_new_id;

        let is_valid = AlgebraicExpression::Reference(AlgebraicReference {
            name: format!("is_valid_{}", pcp.opcode.global_opcode().as_usize()).into(),
            id: is_valid_new_id,
        });

        // guard interaction payloads so they can be merged later
        remapped
            .bus_interactions
            .iter_mut()
            .for_each(|interaction| {
                interaction.args.iter_mut().for_each(|arg| {
                    *arg = is_valid.clone() * arg.clone();
                });
            });

        is_valid_sum = is_valid_sum
            .map(|sum| sum + is_valid.clone())
            .or_else(|| Some(is_valid.clone()));

        stacked_constraints.extend(remapped.constraints);
        interactions_by_machine.push(remapped.bus_interactions);
    }

    tracing::debug!("Stacked chip has {} constraints", stacked_constraints.len());
    let mut stacked_constraints = join_constraints(stacked_constraints);
    tracing::debug!("After joining constraints: {}", stacked_constraints.len());

    // enforce only one is_valid is active
    stacked_constraints.push(make_bool(is_valid_sum.unwrap()).into());

    tracing::debug!(
        "Stacked chip has {} bus interactions",
        interactions_by_machine.iter().flatten().count()
    );
    let stacked_interactions = join_bus_interactions(interactions_by_machine);
    tracing::debug!("After merging interactions: {}", stacked_interactions.len());

    let machine = SymbolicMachine {
        constraints: stacked_constraints,
        bus_interactions: stacked_interactions,
    };

    PowdrStackedPrecompile {
        precompiles: group.into_iter().map(|p| (p.opcode.clone(), p)).collect(),
        machine,
    }
}

/// Merge bus interactions, taking into account args that have the same expression.
fn join_bus_interactions<P: IntoOpenVm>(
    interactions_by_machine: Vec<Vec<SymbolicBusInteraction<P>>>,
) -> Vec<SymbolicBusInteraction<P>> {
    // split interactions by bus/args len, and then by machine
    let mut interactions_by_bus: HashMap<_, Vec<Vec<SymbolicBusInteraction<P>>>> =
        Default::default();

    for interactions in interactions_by_machine {
        let interactions_by_bus_this_machine = interactions
            .into_iter()
            // we group by bus id and number of args
            .into_group_map_by(|interaction| (interaction.id, interaction.args.len()));
        for (k, v) in interactions_by_bus_this_machine {
            let e = interactions_by_bus.entry(k).or_default();
            e.push(v);
        }
    }

    // for each target bus, we sort the machines by number of interactions to that bus
    interactions_by_bus
        .values_mut()
        .for_each(|interactions_by_machine| {
            interactions_by_machine.sort_by_key(|interactions| interactions.len());
        });

    let mut result = vec![];
    // Go through each bus, merging interactions to that bus
    for mut interactions_by_machine in interactions_by_bus.into_values() {
        // to_merge is a vec of vecs, each inner vec is a set of interactions to be merged.
        // We initialize it using the machine with the largest number of interactions to that bus.
        // This is so each subsequent machine can find at least one interaction to merge with for each of its own.
        let mut to_merge = interactions_by_machine
            .pop()
            .unwrap()
            .into_iter()
            .map(|i| vec![i])
            .collect_vec();
        // In the following, we go through each machine and try to find for each
        // of its interactions an existing interaction to merge it with.
        for machine_interactions in interactions_by_machine {
            // merge sets already used by this machine
            let mut used = BTreeSet::new();
            // first: try to find an exact match, where all args have the same expression
            let mut try_partial_match = vec![];
            'outer: for i in machine_interactions {
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if used.contains(&idx) {
                        continue;
                    }
                    let i2 = to_merge_set.get(0).unwrap();
                    let all_args_same_structure = i
                        .args
                        .iter()
                        .zip_eq(i2.args.iter())
                        .all(|(a1, a2)| has_same_structure(strip_guard(&a1), strip_guard(&a2)));
                    if all_args_same_structure {
                        // found an exact match
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                // no exact match for the args found, try a partial match
                try_partial_match.push(i);
            }

            // then: try to find a partial match, where some of the args have the same expression
            let mut no_match = vec![];
            'outer: for i in try_partial_match {
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if used.contains(&idx) {
                        continue;
                    }
                    let i2 = to_merge_set.get(0).unwrap();
                    let some_args_same_structure = i
                        .args
                        .iter()
                        .zip_eq(i2.args.iter())
                        .any(|(a1, a2)| has_same_structure(strip_guard(&a1), strip_guard(&a2)));
                    if some_args_same_structure {
                        // found a partial match on some args
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                no_match.push(i);
            }

            // finally: just pick the first unused one to merge with
            'outer: for i in no_match {
                // just pick the first unused one
                for (idx, to_merge_set) in to_merge.iter_mut().enumerate() {
                    if !used.contains(&idx) {
                        to_merge_set.push(i);
                        used.insert(idx);
                        continue 'outer;
                    }
                }
                unreachable!("could not find any bus interactions to merge with, but should");
            }
        }

        // do the actual merging of the grouped interactions
        result.extend(to_merge.into_iter().map(combine_bus_interactions));
    }

    result
}

/// helper to combine a set of bus interactions into a single interaction.
/// When only compatible args are combined, the expression doesn't need an is_valid guard.
fn combine_bus_interactions<P: IntoOpenVm>(
    interactions: Vec<SymbolicBusInteraction<P>>,
) -> SymbolicBusInteraction<P> {
    assert!(
        interactions.iter().map(|i| i.id).unique().count() == 1,
        "grouped interactions should have the same bus"
    );
    let id = interactions[0].id;
    // multiplicites are just added
    let mult = simplify_expression(
        interactions
            .iter()
            .map(|i| i.mult.clone())
            .reduce(|a, b| a + b)
            .unwrap(),
    );
    let args = interactions
        .into_iter()
        .map(|i| i.args)
        .reduce(|a, b| {
            a.into_iter()
                .zip_eq(b)
                .map(|(a1, a2)| bus_arg_merge(a1, a2))
                .collect()
        })
        .unwrap();
    // remove the is_valid guard from the args when possible
    let args = args
        .into_iter()
        .map(|arg| simplify_expression(bus_arg_simplify(arg)))
        .collect_vec();
    SymbolicBusInteraction { id, args, mult }
}

/// helper to combine args taking into acount if they have the same guarded expression
fn bus_arg_merge<P: IntoOpenVm>(
    arg1: AlgebraicExpression<P>,
    arg2: AlgebraicExpression<P>,
) -> AlgebraicExpression<P> {
    match arg1 {
        // is_valid * expr
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: is_valid1,
            op: AlgebraicBinaryOperator::Mul,
            right: right1,
        }) => {
            let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: is_valid2,
                op: AlgebraicBinaryOperator::Mul,
                right: right2,
            }) = arg2
            else {
                panic!("Expected binary operation for arg2, got: {arg2}");
            };
            if has_same_structure(&right1, &right2) {
                // when the expressions match, we just add the new guard
                (*is_valid1 + *is_valid2) * *right1
            } else {
                (*is_valid1 * *right1) + (*is_valid2 * *right2)
            }
        }
        // is_validA * exprA + is_validB * exprB ... we can't remove the guards
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: _,
            op: AlgebraicBinaryOperator::Add,
            right: _,
        }) => arg1 + arg2,
        _ => unreachable!(),
    }
}

/// helper: if the merged bus arg is a multiplication, it is of the form `is_valid_expr * expr`, and we can remove the guard
fn bus_arg_simplify<P: IntoOpenVm>(arg: AlgebraicExpression<P>) -> AlgebraicExpression<P> {
    if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
        left: _is_valid_expr,
        op: AlgebraicBinaryOperator::Mul,
        right,
    }) = arg
    {
        *right
    } else {
        // sum of different is_valid * expr, can't simplify
        arg
    }
}

/// true if expression is of the form `is_valid_col * some_expr`.
fn is_valid_guarded<P: IntoOpenVm>(expr: &AlgebraicExpression<P>) -> bool {
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left,
            op: AlgebraicBinaryOperator::Mul,
            ..
        }) => match left.as_ref() {
            AlgebraicExpression::Reference(AlgebraicReference { name, .. }) => {
                name.starts_with("is_valid")
            }
            _ => false,
        },
        _ => false,
    }
}

fn normalize_guarded<P: IntoOpenVm>(expr: &mut AlgebraicExpression<P>) {
    assert!(
        is_valid_guarded(expr),
        "not left guarded by is_valid col: {expr}"
    );
    match expr {
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left: _is_valid,
            op: AlgebraicBinaryOperator::Mul,
            right,
        }) => {
            *right.as_mut() = (*right.clone()).normalize();
        }
        _ => unreachable!(),
    }
}

/// assumes constraints are of the form `is_valid_expr * some_expr`
fn join_constraints<P: IntoOpenVm>(
    mut constraints: Vec<SymbolicConstraint<P>>,
) -> Vec<SymbolicConstraint<P>> {
    constraints.sort();
    let mut result: Vec<SymbolicConstraint<P>> = vec![];
    for c1 in constraints {
        // try to find an existing constraint with the same expression but guarded by a different is valid to join with
        let mut found_match = false;
        for c2 in result.iter_mut() {
            if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: is_valid1,
                op: AlgebraicBinaryOperator::Mul,
                right: r1,
            }) = &c1.expr
            {
                if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: is_valid2,
                    op: AlgebraicBinaryOperator::Mul,
                    right: r2,
                }) = &mut c2.expr
                {
                    if r1 == r2 {
                        *is_valid2 = Box::new(*is_valid1.clone() + *is_valid2.clone());
                        found_match = true;
                        break;
                    }
                }
            }
        }
        if !found_match {
            // no matching constraint to join with, just add it as is
            result.push(c1);
        }
    }
    result
}

/// Compare two expressions for equality disregarding the reference name.
fn has_same_structure<P: IntoOpenVm>(
    expr1: &AlgebraicExpression<P>,
    expr2: &AlgebraicExpression<P>,
) -> bool {
    match (expr1, expr2) {
        (
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: left1,
                op: op1,
                right: right1,
            }),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: left2,
                op: op2,
                right: right2,
            }),
        ) => op1 == op2 && has_same_structure(left1, left2) && has_same_structure(right1, right2),
        (
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                op: op1,
                expr: expr1,
            }),
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                op: op2,
                expr: expr2,
            }),
        ) => op1 == op2 && has_same_structure(expr1, expr2),
        (AlgebraicExpression::Reference(r1), AlgebraicExpression::Reference(r2)) => r1.id == r2.id,
        (AlgebraicExpression::Number(n1), AlgebraicExpression::Number(n2)) => n1 == n2,
        _ => false,
    }
}

/// assumes expression is of the form `is_valid_expr * some_expr`.
/// Returns the right side.
fn strip_guard<P: IntoOpenVm>(expr: &AlgebraicExpression<P>) -> &AlgebraicExpression<P> {
    let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
        left: _,
        op: AlgebraicBinaryOperator::Mul,
        right,
    }) = expr
    else {
        panic!("Expression is not a binary operation with Mul operator: {expr}");
    };
    right.as_ref()
}

/// Given two expressions with the same structure, creates mapping of polynomial ids from one to the other.
fn create_mapping<P: IntoOpenVm>(
    from: &AlgebraicExpression<P>,
    to: &AlgebraicExpression<P>,
) -> BiMap<u64, u64> {
    fn create_mapping_inner<P: IntoOpenVm>(
        from: &AlgebraicExpression<P>,
        to: &AlgebraicExpression<P>,
    ) -> BiMap<u64, u64> {
        match (from, to) {
            (
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: left1,
                    op: op1,
                    right: right1,
                }),
                AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: left2,
                    op: op2,
                    right: right2,
                }),
            ) => {
                assert_eq!(op1, op2);
                let mut left = create_mapping_inner(left1, left2);
                let right = create_mapping_inner(right1, right2);
                // assert both sides don't conflict
                assert!(
                    extend_if_no_conflicts(&mut left, right),
                    "conflict in mapping: {from} => {to}"
                );
                left
            }
            (
                AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                    op: op1,
                    expr: expr1,
                }),
                AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                    op: op2,
                    expr: expr2,
                }),
            ) => {
                assert_eq!(op1, op2);
                create_mapping_inner(&expr1, &expr2)
            }
            (AlgebraicExpression::Reference(from), AlgebraicExpression::Reference(to)) => {
                let mut mappings = BiMap::new();
                mappings.insert(from.id, to.id);
                mappings
            }
            _ => BiMap::new(),
        }
    }

    create_mapping_inner(from, to)
}

/// If there are no key or value clashes, extends the bimap.
fn extend_if_no_conflicts(mappings: &mut BiMap<u64, u64>, new_mappings: BiMap<u64, u64>) -> bool {
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
fn expr_poly_id_by_order<P: IntoOpenVm>(
    mut expr: AlgebraicExpression<P>,
) -> AlgebraicExpression<P> {
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
            r.id = new_poly_id(r.id);
            // this is just useful for printing
            r.name = format!("col_{}", r.id).into();
        }
    });
    expr
}

#[derive(Default)]
/// Remaps the columns of precompiles trying to take into account the structure
/// of constraints and bus interactions of other precompiles
struct ColumnAssigner<P: IntoOpenVm> {
    pcps: Vec<PowdrPrecompile<P>>,
}

impl<P: IntoOpenVm> ColumnAssigner<P> {
    /// Assigns columns to the given precompile and updates the assigner
    fn assign_precompile(&mut self, pcp: &mut PowdrPrecompile<P>) {
        let mut mappings = BiMap::new();
        // for each constraint, we want to find if there is a constraint in
        // another machine with the same structure that we can assign the same
        // ids.
        // There may be multiple such constraints, so we try them all (as some
        // of the ids in the current constraint may already be assigned)
        for c in &pcp.machine.constraints {
            for c2 in self
                .pcps
                .iter()
                .map(|pcp| pcp.machine.constraints.iter())
                .flatten()
            {
                if has_same_structure(
                    &expr_poly_id_by_order(c.expr.clone()),
                    &expr_poly_id_by_order(c2.expr.clone()),
                ) {
                    // Found the same structure, try to extend the column mapping
                    let new_mappings = create_mapping(&c.expr, &c2.expr);
                    if extend_if_no_conflicts(&mut mappings, new_mappings) {
                        // we're done for this constraint
                        break;
                    }
                }
            }
        }

        // do the same for bus interactions
        for b in &pcp.machine.bus_interactions {
            for b2 in self
                .pcps
                .iter()
                .map(|pcp| pcp.machine.bus_interactions.iter())
                .flatten()
            {
                if b.id == b2.id && b.args.len() == b2.args.len() {
                    let all_args_same_structure =
                        b.args.iter().zip_eq(b2.args.iter()).all(|(a1, a2)| {
                            has_same_structure(
                                &expr_poly_id_by_order(a1.clone()),
                                &expr_poly_id_by_order(a2.clone()),
                            )
                        });
                    if all_args_same_structure {
                        for (arg1, arg2) in b.args.iter().zip_eq(b2.args.iter()) {
                            let new_mappings = create_mapping(&arg1, &arg2);
                            extend_if_no_conflicts(&mut mappings, new_mappings);
                        }
                        break;
                    }
                }
            }
        }

        assign_columns_from_mapping(pcp, mappings);
        self.pcps.push(pcp.clone());
    }
}

/// Reasign precompile columns taking into acount the given mapping.
/// When not present, use an unused column id (starting from 0).
fn assign_columns_from_mapping<P: IntoOpenVm>(
    pcp: &mut PowdrPrecompile<P>,
    mut mapping: BiMap<u64, u64>,
) {
    let mut curr_id = 0;
    let mut new_poly_id = |old_id: u64| {
        if let Some(id) = mapping.get_by_left(&old_id) {
            return *id;
        }
        // find a new column not yet used in the mapping
        while mapping.get_by_right(&curr_id).is_some() {
            curr_id += 1;
        }
        assert!(
            mapping.get_by_right(&curr_id).is_none(),
            "New id {} already exists in mapping: {:?}",
            curr_id,
            mapping
        );
        assert!(!mapping.contains_left(&old_id));
        let id = curr_id;
        mapping.insert(old_id, id);
        curr_id += 1;
        id
    };

    pcp.machine.pre_visit_expressions_mut(&mut |expr| {
        if let AlgebraicExpression::Reference(r) = expr {
            r.id = new_poly_id(r.id);
        }
    });
    pcp.original_instructions.iter_mut().for_each(|instr| {
        instr.subs.iter_mut().for_each(|sub| {
            *sub = new_poly_id(*sub);
        });
    });

    pcp.is_valid_column.id = new_poly_id(pcp.is_valid_column.id);
}
