use clap::{Parser, Subcommand};
use powdr_openvm_metrics::plot_trace_cells;

#[derive(Parser)]
#[command(name = "openvm-metrics")]
#[command(about = "Run OpenVM metric scripts", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    PlotTraceCells {
        metrics_path: String,

        #[arg(short, long)]
        output: Option<String>,

        #[arg(short, long)]
        subtitle: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::PlotTraceCells {
            metrics_path,
            output,
            subtitle,
        } => plot_trace_cells(&metrics_path, output.as_deref(), subtitle.as_deref()),
    }
}
