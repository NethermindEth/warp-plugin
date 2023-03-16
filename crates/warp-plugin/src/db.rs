use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use cairo_lang_compiler::db::{RootDatabase, RootDatabaseBuilder};
use cairo_lang_filesystem::db::init_dev_corelib;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_starknet::plugin::StarkNetPlugin;


use crate::plugin::WarpPlugin;

pub const WARPLIB_CRATE_NAME: &str = "warplib";

pub trait WarpRootDatabaseBuilderEx {
    fn build_language_server(
        &mut self,
        path: PathBuf,
        plugins: Vec<Arc<dyn SemanticPlugin>>,
    ) -> Result<RootDatabase>;

    /// Tunes a compiler database to Warp (e.g. Warp plugin).
    fn with_warp(&mut self) -> &mut Self;

    fn with_warp_config(&mut self, config: ProjectConfig) -> &mut Self;
}

impl WarpRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn build_language_server(
        &mut self,
        path: PathBuf,
        plugins: Vec<Arc<dyn SemanticPlugin>>,
    ) -> Result<RootDatabase> {
        let mut db = RootDatabase::default();
        init_dev_corelib(&mut db, path);
        db.set_semantic_plugins(plugins);
        Ok(db)
    }

    fn with_warp(&mut self) -> &mut Self {
        // Override implicit precedence for compatibility with Starknet.
        let precedence = ["Pedersen", "RangeCheck", "Bitwise", "EcOp", "GasBuiltin", "System"];

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(WarpPlugin::new()));
        plugins.push(Arc::new(StarkNetPlugin {}));

        self.with_implicit_precedence(&precedence).with_plugins(plugins)
    }

    fn with_warp_config(&mut self, config: ProjectConfig) -> &mut Self {
        let mut project_config: ProjectConfig = config;

        let dir = std::env::var("CAIRO_WARPLIB_DIR")
            .unwrap_or_else(|e| panic!("Problem getting the warplib path: {e:?}"));
        project_config.content.crate_roots.insert(WARPLIB_CRATE_NAME.into(), dir.into());

        let dir = std::env::var("CAIRO_CORELIB_DIR")
            .unwrap_or_else(|e| panic!("Problem getting the corelib path: {e:?}"));
        project_config.corelib = Some(Directory(dir.into()));
        self.with_project_config(project_config);
        self.with_warp()
    }
}
