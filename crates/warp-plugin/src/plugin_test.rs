use std::collections::VecDeque;
use std::env::current_dir;
use std::sync::Arc;

use crate::db::WarpRootDatabaseBuilderEx;
use crate::plugin::WarpPlugin;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{FilesGroup, FilesGroupEx};
use cairo_lang_filesystem::ids::{CrateLongId, Directory, FileLongId, VirtualFile};
use cairo_lang_formatter::format_string;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_test_utils::parse_test_file::TestFileRunner;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

struct ExpandContractTestRunner {
    db: RootDatabase,
}

impl Default for ExpandContractTestRunner {
    fn default() -> Self {
        Self {
            db: RootDatabase::builder().with_warp_default().build().unwrap(),
        }
    }
}
impl TestFileRunner for ExpandContractTestRunner {
    fn run(&mut self, inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
        let (test_module, _semantic_diagnostics) =
            setup_test_module(&mut self.db, inputs["cairo_code"].as_str()).split();

        let file_id = self.db.module_main_file(test_module.module_id).unwrap();
        let syntax_file = self.db.file_syntax(file_id).unwrap();

        let mut current_path = current_dir().unwrap();
        current_path.push("../../warplib/src");

        let crate_id = self.db.intern_crate(CrateLongId("warp".into()));
        let root = Directory(current_path);
        self.db.set_crate_root(crate_id, Some(root));

        let plugin = WarpPlugin::new();
        let mut generated_items: Vec<String> = Vec::new();

        let mut item_queue = VecDeque::from(syntax_file.items(&self.db).elements(&self.db));

        while let Some(item) = item_queue.pop_front() {
            let res = plugin.generate_code(&self.db, item.clone());

            if let Some(generated) = res.code {
                let new_file = self.db.intern_file(FileLongId::Virtual(VirtualFile {
                    parent: Some(file_id),
                    name: generated.name,
                    content: Arc::new(generated.content.clone()),
                }));

                item_queue.extend(
                    self.db
                        .file_syntax(new_file)
                        .unwrap()
                        .items(&self.db)
                        .elements(&self.db),
                );
            }

            if !res.remove_original_item {
                generated_items.push(format_string(
                    &self.db,
                    item.as_syntax_node().get_text(&self.db),
                ));
            }
        }

        OrderedHashMap::from([
            ("generated_cairo_code".into(), generated_items.join("\n")),
            (
                "expected_diagnostics".into(),
                get_diagnostics_as_string(&mut self.db),
            ),
        ])
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    custom_implicits,
    "src/plugin_test_data/implicits",
    {
        module: "module",
        nested_module: "nested_module",
        calls: "calls",
        warp_memory_basic: "warp_memory_basic",
        warp_memory_accessor: "warp_memory_accessor"
    },
    ExpandContractTestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
starknet_contracts,
    "src/plugin_test_data/misc",
    {
        storage: "storage"
    },
    ExpandContractTestRunner
);
