use swc_ecma_ast::{ClassDecl, ImportDecl, TsInterfaceDecl, TsTypeAliasDecl, TsTypeAnn};
use swc_ecma_visit::Visit;

pub struct TsTypeVisitor {
    pub maybe_types: Vec<String>,
}

impl TsTypeVisitor {
    pub fn new() -> TsTypeVisitor {
        TsTypeVisitor {
            maybe_types: Vec::new(),
        }
    }
}

impl Visit for TsTypeVisitor {
    fn visit_class_decl(&mut self, node: &ClassDecl) {
        let type_name = node.ident.sym.to_string();

        self.maybe_types.push(type_name)
    }

    fn visit_import_decl(&mut self, node: &ImportDecl) {
        node.specifiers
            .iter()
            .for_each(|specifier| {
                if specifier.is_default() {
                    let specifier = specifier.as_default().unwrap();

                    self.maybe_types.push(specifier.local.sym.to_string());
                } else if specifier.is_named() {
                    let specifier = specifier.as_named().unwrap();

                    self.maybe_types.push(specifier.local.sym.to_string());
                } else {
                    panic!("Unsupported import: {:?}", specifier);
                }
            })
    }

    fn visit_ts_interface_decl(&mut self, node: &TsInterfaceDecl) {
        let type_name = node.id.sym.to_string();

        self.maybe_types.push(type_name)
    }

    fn visit_ts_type_alias_decl(&mut self, node: &TsTypeAliasDecl) {
        let type_name = node.id.sym.to_string();

        self.maybe_types.push(type_name);
    }
}
