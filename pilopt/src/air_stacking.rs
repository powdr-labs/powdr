use std::collections::{BTreeMap, HashMap};
use itertools::Itertools;

use powdr_number::FieldElement;
use powdr_ast::analyzed::{
    AlgebraicExpression, Analyzed, Identity, PolynomialType, StatementIdentifier, SymbolKind, PolyID,
};
use powdr_pil_analyzer::Counters;

pub fn air_stacking<T: FieldElement>(pil: &mut Analyzed<T>) {
    // map statements to namespace
    let all_nss: BTreeMap<_,_> = powdr_backend_utils::split_by_namespace(pil);

    // map ns to selector/is_valid expression
    let ns_selector: BTreeMap<_,_> = all_nss.iter()
        .map(|(ns, stmts)| {
            let selector = find_ns_guard_expression(pil, stmts);
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

    // now add the pow2 grouped namespaces
    for (group, mut nss) in pow2_groups {
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
                    group_wit.push((def.to_string(), (&new_symbol).into()));
                    new_pil.definitions.insert(def.to_string(), (new_symbol, value.clone()));
                    new_pil.source_order.push(StatementIdentifier::Definition(def.clone()));
                }
            }
        }

        // go through each ns and map witnesses to the group witness
        for ns in nss {
            let mut wit_count = 0;
            for stmt in ns_stmts[&ns] {
                match stmt {
                    StatementIdentifier::Definition(def) => {
                        if let Some((symbol, value)) = pil.definitions.get(def) {
                            assert!(value.is_none());
                            assert!(matches!(symbol.kind, SymbolKind::Poly(PolynomialType::Committed)));
                            // TODO: check if this is part of the selector. If
                            // so, keep the column. These columns can't be
                            // shared to avoid some random constraint activating
                            // the selector for a different machine.
                            symbol_replacements.insert((def.to_string(), symbol.into()), group_wit[wit_count].clone());
                            wit_count += 1;
                        } else {
                            unreachable!("only witness cols should be present");
                        }
                    },
                    StatementIdentifier::ProofItem(idx) => {
                        let mut identity = pil.identities.get(*idx).unwrap().clone();
                        identity.set_id(ids.dispense_identity_id());
                        // TODO:
                        // guard_identity(&ns_selector[ns.as_str()].as_ref().unwrap(), &mut identity);
                        new_pil.identities.push(identity);
                        new_pil.source_order.push(StatementIdentifier::ProofItem(new_pil.identities.len() - 1));
                    },
                    _ => unreachable!(),
                }
            }
        }
    }

    println!("new_pil: \n{}", new_pil);
}

const EXECUTION_BRIDGE_ID: u32 = 0;

fn guard_identity<T: FieldElement>(
    selector: &AlgebraicExpression<T>,
    identity: &mut Identity<T>,
) -> AlgebraicExpression<T> {
    // TODO: guard identity _if needed_
    match identity {
        Identity::Polynomial(id) => todo!(),
        Identity::BusInteraction(id) => todo!(),
        _ => unreachable!(),
    }
}

/// get is_valid expression for a namespace
fn find_ns_guard_expression<T: FieldElement>(pil: &Analyzed<T>, stmts: &[StatementIdentifier]) -> Option<AlgebraicExpression<T>> {
    stmts.iter()
        .find_map(|stmt| {
            if let StatementIdentifier::ProofItem(idx) = stmt {
                if let Identity::BusInteraction(identity) = pil.identities.get(*idx).unwrap() {
                    // execution bridge latch
                    if identity.bus_id == AlgebraicExpression::Number(EXECUTION_BRIDGE_ID.into()) {
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
