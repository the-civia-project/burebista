use crate::ir::File;
use crate::ts_type_visitor::TsTypeVisitor;

use swc_common::BytePos;
use swc_common::input::StringInput as TsStringInput;
use swc_ecma_ast::Module as TsModule;
use swc_ecma_parser::Syntax::Typescript;
use swc_ecma_parser::{Parser as TsParser, TsSyntax as TsConfig};
use swc_ecma_visit::VisitWith;

fn get_ts_ast_for_string(
    code: &str,
    ts_config: Option<TsConfig>,
) -> Result<TsModule, swc_ecma_lexer::error::Error> {
    let ts_config = ts_config.unwrap_or_else(|| TsConfig {
        tsx: true,
        decorators: true,
        ..Default::default()
    });

    let input = TsStringInput::new(code, BytePos(0), BytePos(code.len() as u32));

    // Set up the parser with TypeScript syntax
    let mut parser = TsParser::new(Typescript(ts_config), input, None);

    let module = parser.parse_module();

    module
}

pub fn transform(
    code: &str,
    ts_config: Option<TsConfig>,
) -> Result<String, swc_ecma_lexer::error::Error> {
    let ts_ast = get_ts_ast_for_string(code, ts_config)?;

    let mut ts_type_visitor = TsTypeVisitor::new();

    ts_ast.visit_with(&mut ts_type_visitor);

    println!("{:#?}", ts_type_visitor.maybe_types);

    println!("TS Module {:#?}", ts_ast);

    let rs_ast = File::from_ts_module(&ts_ast);

    println!("Rust Module {:#?}", rs_ast);

    let rs_code = rs_ast.to_string();

    Ok(rs_code)
}
