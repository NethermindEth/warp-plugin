mod build;

use build::BuildArgs;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(
        about = "Builds the warp output, outputting smart contracts artifacts for deployment"
    )]
    Build(BuildArgs),
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build(args) => build::run(args),
    }?;
    Ok(())
}
