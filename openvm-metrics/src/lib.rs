use std::collections::BTreeMap;

mod run_script;

const PLOT_TRACE_CELLS: &str = include_str!("../scripts/plot_trace_cells.py");

pub fn plot_trace_cells(trace: &str, output: Option<&str>, subtitle: Option<&str>) {
    let optional_args = output
        .into_iter()
        .map(|output| ("output", output))
        .chain(subtitle.into_iter().map(|subtitle| ("subtitle", subtitle)))
        .collect::<BTreeMap<_, _>>();
    run_script::run_script(PLOT_TRACE_CELLS, &[trace], optional_args)
}
