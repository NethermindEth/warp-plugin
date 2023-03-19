use std::iter::zip;
use std::ops::DerefMut;

use anyhow::{ensure, Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_project::{ProjectConfig, ProjectConfigContent};
use cairo_lang_starknet::contract::find_contracts;
use cairo_lang_starknet::contract_class::compile_prepared_db;
use cairo_lang_utils::Upcast;
use scarb::compiler::helpers::{
    build_compiler_config, build_project_config, collect_main_crate_ids,
};
use scarb::compiler::{CompilationUnit, Compiler};
use scarb::core::{ExternalTargetKind, Workspace};
use tracing::{trace, trace_span};

use crate::db::WarpRootDatabaseBuilderEx;

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
    /// * `ws` - The workspace that contains the project configuration.
    ///
    /// # Returns
    ///
    /// Returns a `Result` with the unit's contracts compiled into JSON files, or an error if the compilation fails.
    fn compile(&self, unit: CompilationUnit, ws: &Workspace<'_>) -> Result<()> {
        let props = unit.target.kind.downcast::<ExternalTargetKind>();
        ensure!(
            props.params.is_empty(),
            "target `{}` does not accept any parameters",
            props.kind_name
        );

        let target_dir = unit.profile.target_dir(ws.config());

        // override config with local corelib
        let mut config = build_project_config(&unit)?;
        let core_dir = std::env::var("CAIRO_CORELIB_DIR")
            .unwrap_or_else(|e| panic!("Problem getting the corelib path: {e:?}"));
        config.corelib= Some(Directory(core_dir.into()));

        let mut db = RootDatabase::builder()
            .with_project_config(config)
            .with_warp().build()?;
        let compiler_config = build_compiler_config(&unit, ws);

        let main_crate_ids = collect_main_crate_ids(&unit, &db);

        let contracts = {
            let _ = trace_span!("find_contracts").enter();
            find_contracts(&db, &main_crate_ids)
        };

        trace!(
            contracts = ?contracts
                .iter()
                .map(|decl| decl.module_id().full_path(db.upcast()))
                .collect::<Vec<_>>()
        );

        let contracts = contracts.iter().collect::<Vec<_>>();

        let classes = {
            let _ = trace_span!("compile_starknet").enter();
            compile_prepared_db(&mut db, &contracts, compiler_config)?
        };

        for (decl, class) in zip(contracts, classes) {
            let target_name = &unit.target.name;
            let contract_name = decl.submodule_id.name(db.upcast());
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
