use std::collections::{HashMap, HashSet};
use std::iter::once;

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Machine},
    parsed::{asm::AbsoluteSymbolPath, types::Type, NamespacedPolynomialReference, PilStatement},
};
use powdr_pilopt::referenced_symbols::ReferencedSymbols;

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

const MAIN_MACHINE_PATH: &str = "::Main";
const PC_REGISTER: &str = "pc";

pub fn optimize(mut analyzed_asm: AnalysisASMFile) -> AnalysisASMFile {
    //hacky temporal way to detect if the asm file has a main machine
    if build_machine_dependencies(&analyzed_asm)
        .keys()
        .all(|m| m != MAIN_MACHINE_PATH)
    {
        return analyzed_asm;
    }

    println!(
        "Optimizing machine count: {:?}",
        analyzed_asm.machines().count()
    );
    remove_unreferenced_machines(&mut analyzed_asm);
    remove_unused_instructions(&mut analyzed_asm);
    remove_unreferenced_machines(&mut analyzed_asm);
    println!(
        "Optimized machines count: {:?}",
        analyzed_asm.machines().count()
    );
    analyzed_asm
}

fn remove_unreferenced_machines(asm_file: &mut AnalysisASMFile) {
    let deps = build_machine_dependencies(asm_file);
    let mut all_machines = collect_all_dependent_machines(&deps, MAIN_MACHINE_PATH);
    let constrained_machines = collect_constrained_machines(asm_file);
    all_machines.extend(constrained_machines);

    asm_file.modules.iter_mut().for_each(|(path, module)| {
        let machines_in_module = machines_in_module(all_machines.clone(), path);
        module.retain_machines(machines_in_module);
    });
}

fn collect_constrained_machines(asm_file: &AnalysisASMFile) -> impl Iterator<Item = String> + '_ {
    asm_file
        .machines()
        .filter_map(|(path, machine)| {
            machine.pil.iter().any(|pil| {
                matches!(pil, PilStatement::LetStatement(_, _, Some(args), _) if args.ty == Type::Col)
            })
            .then(|| path.to_string())
        })
}

fn machines_in_module(all_machines: HashSet<String>, path: &AbsoluteSymbolPath) -> HashSet<String> {
    let path_str = path.to_string();
    let path_prefix = if path_str == "::" {
        "::".to_string()
    } else {
        format!("{}{}", path_str, "::")
    };

    all_machines
        .into_iter()
        .filter(|machine_path| machine_path.starts_with(&path_prefix))
        .map(|machine_path| {
            machine_path
                .strip_prefix(&path_prefix)
                .unwrap_or(&machine_path)
                .to_string()
        })
        .collect()
}

fn build_machine_dependencies(asm_file: &AnalysisASMFile) -> HashMap<String, HashSet<String>> {
    let mut dependencies = HashMap::new();

    for (path, machine) in asm_file.machines() {
        let submachine_to_decl: HashMap<String, String> = machine
            .submachines
            .iter()
            .map(|sub| (sub.name.clone(), sub.ty.to_string()))
            .collect();

        let submachine_names: HashSet<String> = machine
            .submachines
            .iter()
            .map(|sub| sub.ty.to_string())
            .chain(machine.submachines.iter().flat_map(|sub| {
                sub.args.iter().filter_map(|expr| {
                    expr_to_ref(expr)
                        .and_then(|ref_name| submachine_to_decl.get(&ref_name).cloned())
                })
            }))
            .chain(
                machine
                    .params
                    .0
                    .iter()
                    .map(|param| param.ty.as_ref().unwrap().to_string()),
            )
            .chain(
                machine
                    .links
                    .iter()
                    .filter_map(|ld| submachine_to_decl.get(&ld.to.instance))
                    .cloned(),
            )
            .collect();
        dependencies.insert(path.to_string(), submachine_names);
    }

    dependencies
}

fn expr_to_ref(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Reference(_, NamespacedPolynomialReference { path, .. }) => {
            Some(path.to_string())
        }
        Expression::PublicReference(_, pref) => Some(pref.clone()),
        _ => None,
    }
}

fn collect_all_dependent_machines(
    dependencies: &HashMap<String, HashSet<String>>,
    start: &str,
) -> HashSet<String> {
    let mut result = HashSet::new();
    let mut to_visit = vec![start.to_string()];
    let mut visited = HashSet::new();

    while let Some(machine) = to_visit.pop() {
        if visited.insert(machine.clone()) {
            result.insert(machine.clone());

            if let Some(submachines) = dependencies.get(&machine) {
                to_visit.extend(submachines.iter().cloned());
            }
        }
    }

    result
}

fn remove_unused_instructions(asm_file: &mut AnalysisASMFile) {
    let constrained_machines: HashSet<String> = collect_constrained_machines(asm_file).collect();

    for (_, machine) in asm_file.machines_mut() {
        let symbols_in_callable: HashSet<_> = machine_callable_body_symbols(machine).collect();

        machine
            .instructions
            .retain(|ins| symbols_in_callable.contains(&ins.name));

        let submachine_to_decl: HashMap<String, String> = machine
            .submachines
            .iter()
            .map(|sub| (sub.name.clone(), sub.ty.to_string()))
            .collect();

        let visited_submachines = machine
            .instructions
            .iter()
            .filter(|ins| symbols_in_callable.contains(&ins.name))
            .flat_map(|ins| {
                ins.instruction
                    .links
                    .iter()
                    .filter_map(|link| submachine_to_decl.get(&link.link.instance))
            })
            .cloned();

        let mut used_submachines: HashSet<_> = visited_submachines
            .chain(machine_callable_body_symbols(machine))
            .chain(machine_in_links(machine, &submachine_to_decl))
            .chain(machine_in_args(machine, &submachine_to_decl))
            .collect();

        used_submachines.extend(constrained_machines.clone().into_iter());

        machine
            .submachines
            .retain(|sub| used_submachines.contains(&sub.ty.to_string()));

        let used_symbols: HashSet<_> = once(PC_REGISTER.to_string())
            .chain(machine_callable_body_symbols(machine))
            .chain(machine_in_links(machine, &submachine_to_decl))
            .chain(machine_instructions_symbols(machine))
            .collect();

        machine
            .registers
            .retain(|reg| used_symbols.contains(&reg.name));
    }
}

fn machine_callable_body_symbols(machine: &Machine) -> impl Iterator<Item = String> + '_ {
    machine.callable.function_definitions().flat_map(|def| {
        def.symbols()
            .map(|s| s.name.to_string())
            .collect::<Vec<_>>()
    })
}

fn machine_instructions_symbols(machine: &Machine) -> impl Iterator<Item = String> + '_ {
    machine
        .instructions
        .iter()
        .flat_map(|ins| ins.symbols().map(|s| s.name.to_string()))
}

fn machine_in_args<'a>(
    machine: &'a Machine,
    submachine_to_decl: &'a HashMap<String, String>,
) -> impl Iterator<Item = String> + 'a {
    machine
        .submachines
        .iter()
        .flat_map(|sm| sm.args.iter().filter_map(expr_to_ref))
        .filter_map(|ref_name| submachine_to_decl.get(&ref_name))
        .cloned()
}

fn machine_in_links<'a>(
    machine: &'a Machine,
    submachine_to_decl: &'a HashMap<String, String>,
) -> impl Iterator<Item = String> + 'a {
    machine
        .links
        .iter()
        .filter_map(move |ld| submachine_to_decl.get(&ld.to.instance))
        .cloned()
}
