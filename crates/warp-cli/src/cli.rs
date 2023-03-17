mod build;

use build::BuildArgs;
use clap::{Args, Parser, Subcommand};

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

#[derive(Args)]
struct BindArgs {}

#[derive(Args)]
struct InspectArgs {
    #[clap(short, long, help = "Entity ID to retrieve state for")]
    id: String,
    #[clap(short, long, help = "World address to retrieve entity state from")]
    world_address: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build(args) => build::run(args),
    }?;
    Ok(())
}
