use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use cairo_lang_compiler::db::{RootDatabase, RootDatabaseBuilder};
use cairo_lang_filesystem::db::init_dev_corelib;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_project::{ProjectConfig, ProjectConfigContent};
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

    fn with_warp_default(&mut self) -> &mut Self;
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

    /// Adds the Warp plugin to a Cairo RootDatabaseBuilder instance and sets the implicit precedence for
    /// compatibility with Starknet.
    ///
    /// # Returns
    ///
    /// A mutable reference to the modified `RootDatabaseBuilder` instance with the Warp plugin included and the
    /// implicit precedence set for compatibility with Starknet.
    ///
    fn with_warp(&mut self) -> &mut Self {
        self.with_semantic_plugin(Arc::new(WarpPlugin::new()));
        self.with_semantic_plugin(Arc::new(StarkNetPlugin::default()));
        self
    }

    /// Sets up the Warp default configuration for a Cairo RootDatabaseBuilder instance, overriding the config
    /// and crate paths.
    ///
    /// The function sets up the `corelib` and `crate_roots` paths for the `ProjectConfig` used by
    /// the Cairo compiler instance, and returns a mutable reference to the modified `RootDatabaseBuilder`
    /// instance with Warp plugin included.
    ///
    /// # Returns
    ///
    /// A mutable reference to the modified `RootDatabaseBuilder` instance with Warp included.
    ///
    /// # Panics
    ///
    /// The function will panic if the `CAIRO_CORELIB_DIR` or `CAIRO_WARPLIB_DIR` environment variables
    /// cannot be accessed.
    ///
    fn with_warp_default(&mut self) -> &mut Self {
        let core_dir = std::env::var("CAIRO_CORELIB_DIR")
            .unwrap_or_else(|e| panic!("Problem getting the corelib path: {e:?}"));
        let warplib_dir = std::env::var("CAIRO_WARPLIB_DIR")
            .unwrap_or_else(|e| panic!("Problem getting the warplib path: {e:?}"));
        // this overrides the config and the crate path is not found properly.
        let config = ProjectConfig {
            base_path: "".into(),
            content: ProjectConfigContent {
                crate_roots: HashMap::from([(WARPLIB_CRATE_NAME.into(), warplib_dir.into())]),
            },
            corelib: Some(Directory(core_dir.into())),
        };
        self.with_project_config(config);
        self.with_warp()
    }
}
