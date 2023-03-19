use std::collections::{HashMap, HashSet};
use std::sync::{Arc};

use cairo_lang_defs::plugin::{DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::patcher::{PatchBuilder, Patches, RewriteNode};
use cairo_lang_semantic::plugin::{AsDynGeneratedFileAuxData, AsDynMacroPlugin, DynPluginAuxData, PluginAuxData, PluginMappedDiagnostic, SemanticPlugin};
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use regex::Regex;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use cairo_lang_syntax::node::ast::{AttributeList, FunctionWithBody, MaybeModuleBody, ModuleBody};

use smol_str::SmolStr;


use crate::implicits::{handle_implicits};

/// Warp related auxiliary data of the Warp plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct WarpAuxData {
    /// Patches of code that need translation in case they have diagnostics.
    pub patches: Patches,

    /// A list of functions that were processed by the plugin.
    pub functions: Vec<SmolStr>,
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
        let module_name = module_ast.name(
            db).text(db);
        if let MaybeModuleBody::Some(module_body) = module_ast.body(db) {
            let attributes = module_ast.attributes(db).as_syntax_node();
            let (rewrite_nodes, diagnostics) = self.handle_module_nodes(db, module_body, module_name.clone(), attributes);
            return if let Some(module_rewritten) = rewrite_nodes {
                let mut builder = PatchBuilder::new(db);
                builder.add_modified(module_rewritten);
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: module_name.clone(),
                        content: builder.code,
                        aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(WarpAuxData {
                            patches: builder.patches,
                            functions: vec![module_name],
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


    /// Handles the syntax nodes of a Cairo function and returns a PluginResult containing the generated code
    /// and any diagnostic messages.
    ///
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
    fn handle_top_level_functions(&self, db: &dyn SyntaxGroup, func_ast: &FunctionWithBody) -> PluginResult {
        let (rewrite_nodes, imports, implicit_diagnostics) = handle_implicits(db, func_ast);
        return if let Some(implicit_functions_rewrite) = rewrite_nodes {

            // Traverse the AST until you find the root node (the text file itself), which is the parent of the top-level function.
            //     Convert the syntax node to text to analyze the code.
            //     Search for all functions with the #[implicit] attribute using regex.
            //     Assign a unique ID to each function with the #[implicit] attribute, per parameter.
            //     If a function's ID is 0, add the necessary import(s) in the file.
            //     For all other functions with non-zero IDs, you can assume the required import(s) are already in the file.
            //     Because it would have been added by the first function with ID 0.

            let name = func_ast.declaration(db).name(db).text(db);
            let parent = func_ast.as_syntax_node().parent().unwrap();

            // Get the text of the parent syntax node which is the whole file itself.
            let parent_text = parent.get_text(db);

            // Filter the implicit imports to only those needed for the current function.
            let added_imports: Vec<String> = imports
                .iter()
                .filter(|(implicit_name, _)| {
                    let function_indices = get_file_function_indices(&parent_text, implicit_name);
                    return function_indices.get(name.as_str()).map(|i| *i == 0).unwrap_or(false);
                })
                .map(|(_, implicit_import)| implicit_import.clone())
                .collect();
            let imports_rewrite_node = RewriteNode::Text(added_imports.join("\n"));
            let mut builder = PatchBuilder::new(db);
            builder.add_modified(imports_rewrite_node);
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


    /// Recursively handles the syntax nodes of a Cairo module's items and returns a tuple containing a
    /// `RewriteNode` and any diagnostic messages. The `RewriteNode` represents the modified module,
    /// including any generated code, required implicit imports, and original items. If no modifications are
    /// made, `None` is returned.
    ///
    /// This function is recursive and will call itself for any nested modules in the module body.
    ///
    /// # Arguments
    ///
    /// * `self` - A reference to the current instance of the plugin.
    /// * `db` - A reference to a `SyntaxGroup` trait object that contains the module's syntax tree.
    /// * `module_body` - The `ModuleBody` representing the module's items.
    /// * `name` - The name of the module as a `SmolStr`.
    /// * `attributes` - The syntax node representing any attributes associated with the module.
    ///
    /// # Returns
    ///
    /// A tuple containing a `RewriteNode` representing the modified module, and any diagnostic messages.
    ///
    fn handle_module_nodes(&self, db: &dyn SyntaxGroup, module_body: ModuleBody, name: SmolStr, attributes: SyntaxNode) -> (Option<RewriteNode>, Vec<PluginDiagnostic>) {
        let mut kept_original_items = vec![];
        let mut modified_functions = vec![];
        let mut modified_modules = vec![];
        let mut diagnostics = vec![];
        let mut imports: HashSet<String> = HashSet::new();

        module_body
            .items(db)
            .elements(db)
            .iter()
            .for_each(|el| {
                if let ast::Item::FreeFunction(function_ast) = el {
                    let (rewrite_nodes, implicit_imports, implicit_diagnostics) = handle_implicits(db, &function_ast);
                    if let Some(implicit_functions_rewrite) = rewrite_nodes {
                        modified_functions.push(implicit_functions_rewrite);
                        let mapped_values: Vec<_> = implicit_imports.values().map(|v| v.clone()).collect();
                        imports.extend(mapped_values);
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
        match &item_ast {
            ast::Item::FreeFunction(func_ast) => {
                self.handle_top_level_functions(db, &func_ast)
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

/// Parses a Cairo code string and returns a `HashMap` containing the indices of all functions with an
/// #[implicit] attribute that match the given implicit function name.
///
/// # Arguments
///
/// * `code` - A reference to a `String` representing the Rust code to search.
/// * `implicit_name` - The name of the implicit function to search for.
///
/// # Returns
///
/// A `HashMap` containing the indices of all functions with an #[implicit] attribute that match
/// the given implicit function name.
///
fn get_file_function_indices(code: &String, implicit_name: &str) -> HashMap<String, usize> {
    let re = Regex::new(&format!(r"#\[implicit\([^\)]*{}[^\)]*\)\]\s*fn\s+([a-zA-Z_][a-zA-Z0-9_]*)", implicit_name)).unwrap();
    let mut function_indices = HashMap::new();
    let mut index = 0;

    for caps in re.captures_iter(code.as_str()) {
        if let Some(function_name) = caps.get(1) {
            function_indices.insert(function_name.as_str().to_string(), index);
            index += 1;
        }
    }

    function_indices
}