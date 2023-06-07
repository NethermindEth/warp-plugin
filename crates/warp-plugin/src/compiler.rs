use std::iter::zip;
use std::ops::DerefMut;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;

use cairo_lang_starknet::contract::find_contracts;
use cairo_lang_starknet::contract_class::compile_prepared_db;
use cairo_lang_utils::Upcast;
use scarb::compiler::helpers::{build_compiler_config, collect_main_crate_ids};
use scarb::compiler::{CompilationUnit, Compiler};
use scarb::core::Workspace;
use tracing::{trace, trace_span};

pub struct WarpCompiler;

impl Compiler for WarpCompiler {
    fn target_kind(&self) -> &str {
        "warp"
    }

    /// Compiles the given compilation unit and writes the resulting contracts to JSON files.
    ///
    /// # Arguments
    ///
    /// * `unit` - The compilation unit to be compiled.
    /// * `db` - The current database
    /// * `ws` - The workspace that contains the project configuration.
    ///
    /// # Returns
    ///
    /// Returns a `Result` with the unit's contracts compiled into JSON files, or an error if the compilation fails.
    fn compile(
        &self,
        unit: CompilationUnit,
        db: &mut RootDatabase,
        ws: &Workspace<'_>,
    ) -> Result<()> {
        let target_dir = unit.target_dir(ws.config());
        let compiler_config = build_compiler_config(&unit, ws);
        let main_crate_ids = collect_main_crate_ids(&unit, &db);

        let contracts = {
            let _ = trace_span!("find_contracts").enter();
            find_contracts(db, &main_crate_ids)
        };

        trace!(
            contracts = ?contracts
                .iter()
                .map(|decl| decl.module_id().full_path(*db.upcast()))
                .collect::<Vec<_>>()
        );

        let contracts = contracts.iter().collect::<Vec<_>>();

        let classes = {
            let _ = trace_span!("compile_starknet").enter();
            compile_prepared_db(db, &contracts, compiler_config)?
        };

        for (decl, class) in zip(contracts, classes) {
            let target_name = &unit.target().name;
            let contract_name = decl.submodule_id.name(*db.upcast());
            let mut file = target_dir.open_rw(
                format!("{target_name}_{contract_name}.json"),
                "output file",
                ws.config(),
            )?;
            serde_json::to_writer_pretty(file.deref_mut(), &class)
                .with_context(|| format!("failed to serialize contract: {contract_name}"))?;
        }

        Ok(())
    }
}

#[test]
fn test_compiler() {
    use crate::plugin::CairoPluginRepository;
    use assert_fs::TempDir;
    use camino::{Utf8Path, Utf8PathBuf};
    use scarb::compiler::CompilerRepository;
    use scarb::core::Config;
    use scarb::ops;
    use std::env;

    let mut compilers = CompilerRepository::std();
    compilers.add(Box::new(WarpCompiler)).unwrap();

    let cairo_plugins = CairoPluginRepository::new();

    let cache_dir = TempDir::new().unwrap();
    let config_dir = TempDir::new().unwrap();

    let path = Utf8PathBuf::from_path_buf("../../examples/Scarb.toml".into()).unwrap();
    let config = Config::builder(path.canonicalize_utf8().unwrap())
        .global_cache_dir_override(Some(Utf8Path::from_path(cache_dir.path()).unwrap()))
        .global_config_dir_override(Some(Utf8Path::from_path(config_dir.path()).unwrap()))
        .ui_verbosity(scarb::ui::Verbosity::Verbose)
        .log_filter_directive(env::var_os("SCARB_LOG"))
        .cairo_plugins(cairo_plugins.into())
        .compilers(compilers)
        .build()
        .unwrap();

    let ws = ops::read_workspace(config.manifest_path(), &config)
        .unwrap_or_else(|op| panic!("Error building workspace: {op:?}"));

    ops::compile(&ws).unwrap_or_else(|op| panic!("Error compiling: {op:?}"))
}
