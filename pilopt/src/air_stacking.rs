use std::collections::{BTreeMap, HashMap};
use itertools::Itertools;

use powdr_number::FieldElement;
use powdr_ast::{analyzed::{
    AlgebraicBinaryOperator, AlgebraicBinaryOperation, AlgebraicExpression, AlgebraicReference, AlgebraicUnaryOperator, AlgebraicUnaryOperation, Analyzed, Expression, Identity, PolyID, PolynomialType, Reference, StatementIdentifier, SymbolKind
}, parsed::{asm::{AbsoluteSymbolPath, SymbolPath}, visitor::ExpressionVisitable}};
use powdr_pil_analyzer::Counters;

/// AIR stacking PIL transformation. Groups similarly sized (in terms of witness
/// columns) submachines into a single namespace and have them share witness
/// columns. `latch_bus_id` is the id of the bus from which to take the selector
/// expression for each machine. Only machines with a call to this bus will be
/// considered for stacking.
pub fn air_stacking<T: FieldElement>(pil: &mut Analyzed<T>, latch_bus_id: u32) {
    // map statements to namespace
    let all_nss: BTreeMap<_,_> = powdr_backend_utils::split_by_namespace(pil);

    // map ns to selector/is_valid expression
    let ns_selector: BTreeMap<_,_> = all_nss.iter()
        .map(|(ns, stmts)| {
            let selector = find_ns_guard_expression(pil, stmts, latch_bus_id);
            (ns.to_string(), selector)
        })
        .collect();

    // ignore namespaces with unsupported features or no selector/is_valid
    let ns_stmts: BTreeMap<_,_> = all_nss.iter()
        .filter(|(ns, stmts)| {
            stmts.iter().all(|s| is_supported_stmt(pil, s)) && ns_selector[ns.as_str()].is_some()
        })
        .collect();

    // map ns to pow2 group
    let pow2_groups: HashMap<_,_> = ns_stmts.iter()
        .map(|(ns, stmts)| {
            let n = stmts.iter()
                .filter(|s| is_witness_def(pil, s))
                .count();
            let pow2 = n.next_power_of_two();
            (pow2, (n, ns.to_string()))
        })
        .into_group_map()
        .into_iter()
        // sort by witness count
        .map(|(pow2, nss)| {
            (pow2, nss.into_iter().sorted().map(|(_n, ns)| ns).collect::<Vec<_>>())
        })
        // ignore groups with a single element
        .filter(|(_, ns)| ns.len() > 1)
        .collect();

    // map pow2 group to degree

    // we'll create a new Analyzed with the pow2 groups merged into single namespaces
    let mut new_pil = Analyzed::<T>::default();
    new_pil.solved_impls = pil.solved_impls.clone();
    new_pil.trait_impls = pil.trait_impls.clone();
    new_pil.auto_added_symbols = pil.auto_added_symbols.clone();

    // start with statements from ungrouped/unsupported namespaces
    let mut ids = Counters::default();
    let mut symbol_replacements: HashMap<(String, PolyID), (String, PolyID)> = HashMap::new();
    all_nss.iter().filter(|(ns, _)| {
        pow2_groups.values().all(|group| !group.contains(ns))
    }).for_each(|(_ns, stmts)| {
        for stmt in stmts {
            match stmt {
                StatementIdentifier::Definition(def) => {
                    if let Some((symbol, value)) = pil.definitions.get(def) {
                        let mut new_symbol = symbol.clone();
                        new_symbol.id = ids.dispense_symbol_id(new_symbol.kind, new_symbol.length);
                        symbol_replacements.insert((def.to_string(), symbol.into()), (def.to_string(), (&new_symbol).into()));
                        new_pil.definitions.insert(def.to_string(), (new_symbol, value.clone()));
                    } else if let Some((symbol, value)) = pil.intermediate_columns.get(def) {
                        let mut new_symbol = symbol.clone();
                        new_symbol.id = ids.dispense_symbol_id(new_symbol.kind, new_symbol.length);
                        symbol_replacements.insert((def.to_string(), symbol.into()), (def.to_string(), (&new_symbol).into()));
                        new_pil.intermediate_columns.insert(def.to_string(), (new_symbol, value.clone()));
                    } else {
                        panic!("definition not found");
                    }
                    new_pil.source_order.push(StatementIdentifier::Definition(def.clone()));
                },
                StatementIdentifier::ProofItem(idx) => {
                    let mut identity = pil.identities.get(*idx).unwrap().clone();
                    identity.set_id(ids.dispense_identity_id());
                    new_pil.identities.push(identity);
                    new_pil.source_order.push(StatementIdentifier::ProofItem(new_pil.identities.len() - 1));
                },
                StatementIdentifier::ProverFunction(idx) => {
                    new_pil.prover_functions.push(pil.prover_functions.get(*idx).unwrap().clone());
                    new_pil.source_order.push(StatementIdentifier::ProverFunction(new_pil.prover_functions.len() - 1));
                },
                StatementIdentifier::TraitImplementation(idx) => {
                    new_pil.source_order.push(StatementIdentifier::TraitImplementation(*idx));
                },
            }
        }
    });

    // now add the pow2 stacked namespaces
    for (group, mut nss) in pow2_groups {
        let change_namespace = |abs_name: &str| {
            let name = abs_name.split("::").last().unwrap();
            format!("stacked{}::{}", group, name)
        };
        // add witness definitions from largest namespace, so we can map every
        // namespace's witness to these. nss is already sorted by witness count
        let mut group_wit = Vec::new();
        for stmt in ns_stmts[nss.last().unwrap()] {
            if let StatementIdentifier::Definition(def) = stmt {
                if let Some((symbol, value)) = pil.definitions.get(def) {
                    assert!(value.is_none());
                    assert!(matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Committed)));
                    let mut new_symbol = symbol.clone();
                    new_symbol.id = ids.dispense_symbol_id(new_symbol.kind, new_symbol.length);
                    new_symbol.absolute_name = change_namespace(&new_symbol.absolute_name);
                    let new_def = new_symbol.absolute_name.clone();
                    group_wit.push((new_def.to_string(), (&new_symbol).into()));
                    new_pil.definitions.insert(new_def.to_string(), (new_symbol, value.clone()));
                    new_pil.source_order.push(StatementIdentifier::Definition(new_def));
                }
            }
        }

        // go through each ns and map witnesses to the group witness
        for ns in nss {
            let mut wit_count = 0;
            for stmt in ns_stmts[&ns] {
                match stmt {
                    StatementIdentifier::Definition(def) => {
                        let Some((symbol, value)) = pil.definitions.get(def) else {
                            unreachable!("only witness cols should be present");
                        };
                        assert!(value.is_none());
                        assert!(matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Committed)));
                        // check if this column is part of the selector. If so,
                        // keep the column. These columns can't be shared,
                        // otherwise some random constraint could active the
                        // selector for a different machine.
                        let selector = ns_selector.get(ns.as_str()).unwrap().as_ref().unwrap();
                        let symbol_id: PolyID = symbol.into();
                        let should_keep_col = selector.expr_any(|expr| {
                            match expr {
                                AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) => {
                                    poly_id == &symbol_id
                                }
                                _ => false,
                            }
                        });
                        if should_keep_col {
                            let mut new_symbol = symbol.clone();
                            new_symbol.id = ids.dispense_symbol_id(new_symbol.kind, new_symbol.length);
                            // TODO: avoid name clashes before changing namespaces
                            // new_symbol.absolute_name = change_namespace(&new_symbol.absolute_name);
                            let new_def = new_symbol.absolute_name.clone();
                            symbol_replacements.insert((def.to_string(), symbol.into()), (new_def.to_string(), (&new_symbol).into()));
                            new_pil.definitions.insert(new_def.to_string(), (new_symbol, value.clone()));
                            new_pil.source_order.push(StatementIdentifier::Definition(new_def.clone()));
                        } else {
                            symbol_replacements.insert((def.to_string(), symbol.into()), group_wit[wit_count].clone());
                            wit_count += 1;
                        }
                    },
                    StatementIdentifier::ProofItem(idx) => {
                        let mut identity = pil.identities.get(*idx).unwrap().clone();
                        identity.set_id(ids.dispense_identity_id());
                        guard_identity(&mut identity, &ns_selector[ns.as_str()].as_ref().unwrap());
                        new_pil.identities.push(identity);
                        new_pil.source_order.push(StatementIdentifier::ProofItem(new_pil.identities.len() - 1));
                    },
                    _ => unreachable!(),
                }
            }
        }
    }

    let (subs_by_id, subs_by_name): (HashMap<_, _>, HashMap<_, _>) =
        symbol_replacements.iter().map(|(k, v)| ((k.1, v), (&k.0, v))).unzip();

    new_pil.post_visit_expressions_in_identities_mut(&mut |e: &mut AlgebraicExpression<_>| {
        if let AlgebraicExpression::Reference(ref mut reference) = e {
            if let Some((replacement_name, replacement_id)) = subs_by_id.get(&reference.poly_id) {
                reference.poly_id = *replacement_id;
                reference.name = replacement_name.clone();
            }
        }
    });

    new_pil.post_visit_expressions_mut(&mut |e: &mut Expression| {
        if let Expression::Reference(_, Reference::Poly(reference)) = e {
            if let Some((replacement_name, _)) = subs_by_name.get(&reference.name) {
                reference.name = replacement_name.clone();
            }
        }
    });

    println!("new_pil: \n{}", new_pil);
    *pil = new_pil;
}

/// returns true if the expression is already guarded by the selector. The
/// selector may be a sum of expressions, and any of the summands can be used as
/// a guard. For example:
/// ```
/// selector = (instr_add + instr_sub);
/// instr_add * (a + b) = instr_add * c;
/// instr_sub * (a - b) = instr_sub * c;
/// ```
fn is_guarded_by<T: FieldElement>(expr: &AlgebraicExpression<T>, selector: &AlgebraicExpression<T>) -> bool {

    // selector summands
    let mut selector_summands = vec![];
    let mut queue = vec![selector];
    while let Some(item) = queue.pop() {
        if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op: AlgebraicBinaryOperator::Add, right }) = item {
            queue.push(left);
            queue.push(right);
        }
        selector_summands.push(item);
    }

    match expr {
        // check if both sides are guarded by the selector
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left, op: AlgebraicBinaryOperator::Add | AlgebraicBinaryOperator::Sub, right
        }) => {
            is_guarded_by(left, selector) && is_guarded_by(right, selector)
        },
        // check if any side is guarded by the selector
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
            left, op: AlgebraicBinaryOperator::Mul, right
        }) => {
            is_guarded_by(left, selector) || is_guarded_by(right, selector)
        },
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op: AlgebraicUnaryOperator::Minus, expr }) => {
            is_guarded_by(expr, selector)
        },
        _ => selector_summands.iter().any(|s| s == &expr),
    }
}

/// ensure the identity is guarded by the selector
fn guard_identity<T: FieldElement>(
    identity: &mut Identity<T>,
    selector: &AlgebraicExpression<T>,
) {
    match identity {
        Identity::Polynomial(id) => if !is_guarded_by(&id.expression, selector) {
            id.expression = id.expression.clone() * selector.clone();
        },
        Identity::BusInteraction(id) => {
            if !is_guarded_by(&id.multiplicity, selector) {
                id.multiplicity = id.multiplicity.clone() * selector.clone();
            }
            if !is_guarded_by(&id.latch, selector) {
                id.latch = id.latch.clone() * selector.clone();
            }
        },
        _ => unreachable!(),
    }
}

/// get is_valid expression for a namespace
fn find_ns_guard_expression<T: FieldElement>(pil: &Analyzed<T>, stmts: &[StatementIdentifier], latch_bus_id: u32) -> Option<AlgebraicExpression<T>> {
    stmts.iter()
        .find_map(|stmt| {
            if let StatementIdentifier::ProofItem(idx) = stmt {
                if let Identity::BusInteraction(identity) = pil.identities.get(*idx).unwrap() {
                    // execution bridge latch
                    if identity.bus_id == AlgebraicExpression::Number(latch_bus_id.into()) {
                        return Some(identity.latch.clone())
                    }
                }
            }
            None
        })
}

fn is_supported_stmt<T: FieldElement>(pil: &Analyzed<T>, stmt: &StatementIdentifier) -> bool {
    match stmt {
        StatementIdentifier::Definition(name) => {
            if let Some((symbol, _value)) = pil.definitions.get(name) {
                // no arrays and only witness cols
                !symbol.is_array() &&
                    matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Committed))
            } else if pil.intermediate_columns.get(name).is_some() {
                // no intermediates
                false
            } else {
                unreachable!()
            }
        }
        StatementIdentifier::ProofItem(idx) => {
            match pil.identities.get(*idx).unwrap() {
                // only polynomial identities and native bus interactions
                Identity::Polynomial(_) | Identity::BusInteraction(_) => true,
                _ => false,
            }
        }
        StatementIdentifier::ProverFunction(_) => false,
        StatementIdentifier::TraitImplementation(_) => false,
    }
}

fn is_witness_def<T: FieldElement>(pil: &Analyzed<T>, stmt: &StatementIdentifier) -> bool {
    match stmt {
        StatementIdentifier::Definition(name) => {
            if let Some((symbol, _value)) = pil.definitions.get(name) {
                if matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Committed)) {
                    return true;
                }
            }
        }
        _ => (),
    }
    false
}
