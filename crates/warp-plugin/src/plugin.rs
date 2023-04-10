use std::collections::HashMap;
use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginGeneratedFile, PluginResult,
};
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::patcher::{PatchBuilder, Patches};
use cairo_lang_semantic::plugin::{
    AsDynGeneratedFileAuxData, AsDynMacroPlugin, DynPluginAuxData, PluginAuxData,
    PluginMappedDiagnostic, SemanticPlugin,
};
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_syntax::node::ast::{FunctionWithBody, MaybeModuleBody};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};

use smol_str::SmolStr;

use crate::handling::{handle_function, handle_module, MaybeRewritten};

/// Warp related auxiliary data of the Warp plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct WarpAuxData {
    /// Patches of code that need translation in case they have diagnostics.
    pub patches: Patches,

    /// A list of elements that were processed by the plugin.
    pub elements: Vec<SmolStr>,
}

impl GeneratedFileAuxData for WarpAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() {
            self == other
        } else {
            false
        }
    }
}

impl AsDynGeneratedFileAuxData for WarpAuxData {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}

impl PluginAuxData for WarpAuxData {
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else { return None; };
        let span = self.patches.translate(
            db.upcast(),
            diag.stable_location.diagnostic_location(db.upcast()).span,
        )?;
        Some(PluginMappedDiagnostic {
            span,
            message: diag.format(db),
        })
    }
}

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug, Default)]
pub struct WarpPlugin {}

impl WarpPlugin {
    pub fn new() -> Self {
        Self {}
    }

    /// Handles a Cairo module's syntax nodes and returns a PluginResult containing the generated code
    /// and any diagnostic messages.
    ///
    /// # Arguments
    ///
    /// * `self` - A reference to the current instance of the plugin.
    /// * `db` - A reference to a `SyntaxGroup` trait object that contains the module's syntax tree.
    /// * `module_ast` - A reference to the `ast::ItemModule` representing the module's syntax tree.
    ///
    /// # Returns
    ///
    /// A `PluginResult` struct containing the generated code, any diagnostic messages, and a boolean
    /// indicating whether the original module should be removed.
    ///
    fn handle_mod(&self, db: &dyn SyntaxGroup, module_ast: &ast::ItemModule) -> PluginResult {
        let module_name = module_ast.name(db).text(db);
        // dbg!(format!("Handle module {module_name}"));
        if let MaybeModuleBody::Some(module_body) = module_ast.body(db) {
            let attributes = module_ast.attributes(db).as_syntax_node();
            let (maybe_module_rewritten, diagnostics) =
                handle_module(db, module_body, module_name.clone(), attributes);
            return if let MaybeRewritten::Some(module_rewritten) = maybe_module_rewritten {
                let mut builder = PatchBuilder::new(db);
                builder.add_modified(module_rewritten);
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: module_name.clone(),
                        content: builder.code,
                        aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(
                            WarpAuxData {
                                patches: builder.patches,
                                elements: vec![module_name],
                            },
                        )),
                    }),
                    diagnostics,
                    remove_original_item: true,
                }
            } else {
                PluginResult {
                    code: None,
                    diagnostics,
                    remove_original_item: false,
                }
            };
        }

        PluginResult::default()
    }

    /// Handles the syntax nodes of a Cairo function and returns a PluginResult containing the generated code
    /// and any diagnostic messages.
    ///
    /// Technically, this function is visited for every function in the file, but it only does anything
    /// with top-level functions, as functions inside modules have already been handled by `handle_mod()` since they're visited first.
    /// # Arguments
    ///
    /// * `self` - A reference to the current instance of the plugin.
    /// * `db` - A reference to a `SyntaxGroup` trait object that contains the function's syntax tree.
    /// * `func_ast` - A reference to the `FunctionWithBody` representing the function's syntax tree.
    ///
    /// # Returns
    ///
    /// A `PluginResult` struct containing the generated code, any diagnostic messages, and a boolean
    /// indicating whether the original function should be removed.
    ///
    fn handle_functions(&self, db: &dyn SyntaxGroup, func_ast: &FunctionWithBody) -> PluginResult {
        // dbg!(format!("Handling external function"));
        // TODO(Performance): Avoid this clone
        let (maybe_rewriten_func, implicit_diagnostics) =
            handle_function(db, &HashMap::new(), func_ast.clone());
        return if let MaybeRewritten::Some(rewritten_func) = maybe_rewriten_func {
            let func_name = func_ast.declaration(db).name(db).text(db);
            let mut builder = PatchBuilder::new(db);
            builder.add_modified(rewritten_func);
            PluginResult {
                code: Some(PluginGeneratedFile {
                    name: func_name.clone(),
                    content: builder.code,
                    aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(WarpAuxData {
                        patches: builder.patches,
                        elements: vec![func_name],
                    })),
                }),
                diagnostics: implicit_diagnostics,
                remove_original_item: true,
            }
        } else {
            PluginResult {
                code: None,
                diagnostics: implicit_diagnostics,
                remove_original_item: false,
            }
        };
    }
}

impl MacroPlugin for WarpPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        // dbg!("Generating code");
        match &item_ast {
            ast::Item::FreeFunction(func_ast) => self.handle_functions(db, func_ast),
            ast::Item::Module(module_ast) => self.handle_mod(db, module_ast),
            _ => PluginResult::default(),
        }
    }
}

impl AsDynMacroPlugin for WarpPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}

impl SemanticPlugin for WarpPlugin {}
