use std::collections::{HashMap, HashSet};
use std::sync::{Arc};

use cairo_lang_defs::plugin::{DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::patcher::{PatchBuilder, Patches, RewriteNode};
use cairo_lang_semantic::plugin::{AsDynGeneratedFileAuxData, AsDynMacroPlugin, DynPluginAuxData, PluginAuxData, PluginMappedDiagnostic, SemanticPlugin};
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;

use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use cairo_lang_syntax::node::ast::{AttributeList, FunctionWithBody, MaybeModuleBody, ModuleBody};

use smol_str::SmolStr;


use crate::implicits::handle_implicits;

/// Dojo related auxiliary data of the Dojo plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct WarpAuxData {
    /// Patches of code that need translation in case they have diagnostics.
    pub patches: Patches,

    /// A list of functions that were processed by the plugin.
    pub functions: Vec<smol_str::SmolStr>,
}

impl GeneratedFileAuxData for WarpAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
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
        let span = self
            .patches
            .translate(db.upcast(), diag.stable_location.diagnostic_location(db.upcast()).span)?;
        Some(PluginMappedDiagnostic { span, message: diag.format(db) })
    }
}

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug, Default)]
pub struct WarpPlugin {
}

impl WarpPlugin {
    pub fn new() -> Self {
        Self {}
    }

    /// Handles modules.
    /// Rewrites the inner functions annoted with `#[implicit]` by adding the implicits to the function signature.
    /// Nested modules are supported.
    fn handle_mod(&self, db: &dyn SyntaxGroup, module_ast: &ast::ItemModule) -> PluginResult {
        let name = module_ast.name(
            db).text(db);
        if let MaybeModuleBody::Some(body) = module_ast.body(db) {
            let attributes = module_ast.attributes(db).as_syntax_node();
            let (rewrite_nodes, diagnostics) = self.handle_module_nodes(db, body, name.clone(), attributes);
            return if let Some(module_rewrites) = rewrite_nodes {
                let module_attributes = module_ast.attributes(db).as_syntax_node();
                let mut builder = PatchBuilder::new(db);
                builder.add_modified(module_rewrites);
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: name.clone(),
                        content: builder.code,
                        aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(WarpAuxData {
                            patches: builder.patches,
                            functions: vec![name],
                        })),
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


    /// Handles top-level functions.
    fn handle_functions(&self, db: &dyn SyntaxGroup, func_ast: &FunctionWithBody) -> PluginResult {
        let (rewrite_nodes, imports, implicit_diagnostics) = handle_implicits(db, func_ast);
        return if let Some(implicit_functions_rewrite) = rewrite_nodes {
            let name = func_ast.declaration(db).name(db).text(db);
            let imports_rewrite_node = RewriteNode::Text(imports.join("\n"));
            let mut builder = PatchBuilder::new(db);
            builder.add_modified(implicit_functions_rewrite);
            PluginResult {
                code: Some(PluginGeneratedFile {
                    name: name.clone(),
                    content: builder.code,
                    aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(WarpAuxData {
                        patches: builder.patches,
                        functions: vec![name],
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


    /// Handles nodes inside a module.
    /// Rewrites the inner functions annoted with `#[implicit]` by adding the implicits to the function signature.
    /// Recursively handles nested modules.
    fn handle_module_nodes(&self, db: &dyn SyntaxGroup, module_body: ModuleBody, name: SmolStr, attributes: SyntaxNode) -> (Option<RewriteNode>, Vec<PluginDiagnostic>) {
        let mut kept_original_items = vec![];
        let mut modified_functions = vec![];
        let mut modified_modules = vec![];
        let mut diagnostics = vec![];
        let mut imports = HashSet::new();

        module_body
            .items(db)
            .elements(db)
            .iter()
            .for_each(|el| {
                if let ast::Item::FreeFunction(function_ast) = el {
                    let (rewrite_nodes, implicit_imports, implicit_diagnostics) = handle_implicits(db, &function_ast);
                    if let Some(implicit_functions_rewrite) = rewrite_nodes {
                        modified_functions.push(implicit_functions_rewrite);
                        imports.extend(implicit_imports);
                    }
                    diagnostics.extend(implicit_diagnostics);
                } else if let ast::Item::Module(module_ast) = el {
                    let name = module_ast.name(
                        db).text(db);

                    if let MaybeModuleBody::Some(body) = module_ast.body(db) {
                        let attributes = module_ast.attributes(db).as_syntax_node();
                        let (rewrite_nodes, module_diagnostics) = self.handle_module_nodes(db, body, name, attributes);
                        if let Some(module_rewrites) = rewrite_nodes {
                            modified_modules.push(module_rewrites);
                        }
                        diagnostics.extend(module_diagnostics);
                    }
                } else {
                    kept_original_items.push(RewriteNode::Copied(el.as_syntax_node()));
                }
            });

        if modified_functions.is_empty() && modified_modules.is_empty() {
            return (None, diagnostics);
        }


        let mut imports_joined_newlines = imports
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        (Some
             (RewriteNode::interpolate_patched(
                 "
                $attributes$
                mod $name$ {
                    $implicit_imports$
                    $original_items$
                    $modified_functions$
                    $modified_modules$
                }
                ",
                 HashMap::from([
                     ("attributes".to_string(), RewriteNode::new_trimmed(attributes)),
                      ("implicit_imports".to_string(), RewriteNode::Text(imports_joined_newlines)),
                      ("name".to_string(), RewriteNode::Text(name.to_string())),
                      ("original_items".to_string(), RewriteNode::new_modified(kept_original_items)),
                      ("modified_functions".to_string(), RewriteNode::new_modified(modified_functions)),
                      ("modified_modules".to_string(), RewriteNode::new_modified(modified_modules)),
                 ]),
             )), diagnostics)
    }
}

impl MacroPlugin for WarpPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        // match item_ast {
        //     ast::Item::FreeFunction(function_ast) => {
        //         // println!("Handling free function: {}", &function_ast.declaration(db).name(db).text(db));
        //         self.handle_function(db, &function_ast)
        //     }
        // //     ast::Item::Module(module_ast) => self.handle_mod(db, module_ast),
        //     _ => PluginResult::default(),
        // }
        match &item_ast {
            ast::Item::FreeFunction(func_ast) => {
                self.handle_functions(db, &func_ast)
            }
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
