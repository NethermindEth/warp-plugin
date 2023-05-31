use std::env::{self, current_dir};

use anyhow::Result;
use camino::Utf8PathBuf;
use clap::Args;
use scarb::compiler::CompilerRepository;
use scarb::core::Config;
use scarb::ops;
use scarb::ui::Verbosity;
use warp_plugin::compiler::WarpCompiler;
use warp_plugin::plugin::CairoPluginRepository;

#[derive(Args, Debug)]
pub struct BuildArgs {
    #[clap(help = "Source directory")]
    path: Option<Utf8PathBuf>,
}

pub fn run(args: BuildArgs) -> Result<()> {
    let source_dir = match args.path {
        Some(path) => get_absolute_path(path),
        None => Utf8PathBuf::from_path_buf(current_dir().unwrap()).unwrap(),
    };

    let mut compilers = CompilerRepository::std();
    compilers.add(Box::new(WarpCompiler)).unwrap();

    let cairo_plugins = CairoPluginRepository::new();

    let manifest_path = source_dir.join("Scarb.toml");
    dbg!(&manifest_path);

    let config = Config::builder(manifest_path)
        .ui_verbosity(Verbosity::Verbose)
        .log_filter_directive(env::var_os("SCARB_LOG"))
        .compilers(compilers)
        .cairo_plugins(cairo_plugins.into())
        .build()
        .unwrap();

    let ws = ops::read_workspace(config.manifest_path(), &config).unwrap_or_else(|err| {
        eprintln!("error: {}", err);
        std::process::exit(1);
    });
    ops::compile(&ws)
}

fn get_absolute_path(path: Utf8PathBuf) -> Utf8PathBuf {
    if path.is_absolute() {
        path
    } else {
        relative_to_absolute_path(path)
    }
}

fn relative_to_absolute_path(path: Utf8PathBuf) -> Utf8PathBuf {
    let mut current_path = current_dir().unwrap();
    current_path.push(path);
    Utf8PathBuf::from_path_buf(current_path).unwrap()
}
