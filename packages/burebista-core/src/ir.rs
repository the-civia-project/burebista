use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use swc_ecma_ast::{
    BinExpr as TsBinExpr, BinaryOp as TsBinaryOp, BindingIdent as TsBindingIdent,
    BlockStmt as TsBlockStmt, Bool as TsBoolean, Expr as TsExpr, Expr, FnDecl as TsFnDecl, Ident,
    Ident as TsIdent, Lit as TsLit, Module as TsModule, ModuleDecl as TsModuleDecl, ModuleDecl,
    ModuleItem as TsModuleItem, Number as TsNumber, Param as TsParam, ReturnStmt as TsReturnStmt,
    Stmt as TsStmt, Str as TsString, TsKeywordTypeKind, TsTypeAnn, TsTypeRef, VarDecl as TsVarDecl,
    VarDeclKind as TsVarDeclKind, VarDeclKind, VarDeclarator as TsVarDeclarator,
};

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn from_ts_ident(ident: &TsIdent) -> Self {
        Identifier {
            name: ident.sym.to_string(),
        }
    }

    pub fn from_ts_biding_ident(ts_biding_ident: TsBindingIdent) -> Self {
        Identifier {
            name: ts_biding_ident.sym.to_string(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

fn transform_ts_module_item_to_rs_root_level_statements(
    ts_items: Vec<TsModuleItem>,
) -> Vec<RootStatements> {
    let mut items = vec![];

    for ts_item in ts_items {
        if ts_item.is_stmt() {
            let ts_stmt = ts_item.as_stmt().unwrap();

            if ts_stmt.is_decl() {
                let ts_decl = ts_stmt.as_decl().unwrap();

                if ts_decl.is_var() {
                    let ts_var_declarations = ts_decl.as_var().unwrap();

                    let mutable = match ts_var_declarations.kind {
                        VarDeclKind::Const => false,
                        _ => true,
                    };

                    if mutable {
                        panic!("Mutable variables outside of functions are unsupported");
                    }

                    ts_var_declarations.decls.iter().for_each(|ts_var| {
                        items.push(RootStatements::from_ts_var_decl(ts_var, false));
                    })
                } else if ts_decl.is_fn_decl() {
                    items.push(RootStatements::from_ts_fn_decl(
                        ts_decl.as_fn_decl().unwrap(),
                        false,
                    ));
                } else if ts_decl.is_class() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_enum() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_interface() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_type_alias() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else {
                    panic!("Unsupported typescript declaration type {:#?}", ts_decl)
                }
            } else {
                panic!(
                    "Other types of statements are not supported outside function blocks {:#?}",
                    ts_stmt
                )
            }
        } else if ts_item.is_module_decl() {
            let ts_module_decl = ts_item.as_module_decl().unwrap();

            if ts_module_decl.is_export_decl() {
                let ts_export_decl = ts_module_decl.as_export_decl().unwrap();
                let ts_decl = ts_export_decl.clone().decl;

                if ts_decl.is_var() {
                    let ts_var_declarations = ts_decl.as_var().unwrap();

                    let mutable = match ts_var_declarations.kind {
                        VarDeclKind::Const => false,
                        _ => true,
                    };

                    if mutable {
                        panic!("Mutable variables outside of functions are unsupported");
                    }

                    ts_var_declarations.decls.iter().for_each(|ts_var| {
                        items.push(RootStatements::from_ts_var_decl(ts_var, true));
                    })
                } else if ts_decl.is_fn_decl() {
                    items.push(RootStatements::from_ts_fn_decl(
                        ts_decl.as_fn_decl().unwrap(),
                        true,
                    ));
                } else if ts_decl.is_class() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_enum() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_interface() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else if ts_decl.is_ts_type_alias() {
                    todo!("Not yet implemented {:#?}", ts_decl);
                } else {
                    panic!("Unsupported typescript declaration type {:#?}", ts_decl)
                }
            } else {
                panic!(
                    "Unsupported export module declaration type {:#?}",
                    ts_module_decl
                )
            }
        } else {
            panic!("Unsupported typescript module item {:#?}", ts_item)
        }
    }

    items
}

#[derive(Debug, Clone)]
pub struct File {
    pub imports: Vec<Import>,
    pub items: Vec<RootStatements>,
}

impl File {
    pub fn from_ts_module(ts_module: &TsModule) -> Self {
        let mut ts_import_items: Vec<ModuleDecl> = vec![];
        let mut ts_stmt_items: Vec<TsModuleItem> = vec![];

        ts_module.body.iter().for_each(|ts_item| {
            if ts_item.is_module_decl() {
                let ts_module_decl = ts_item.as_module_decl().unwrap();

                if ts_module_decl.is_export_decl() {
                    ts_stmt_items.push(ts_item.clone());
                } else {
                    ts_import_items.push(ts_module_decl.clone());
                }
            } else {
                ts_stmt_items.push(ts_item.clone());
            }
        });

        let imports = vec![];
        let items = transform_ts_module_item_to_rs_root_level_statements(ts_stmt_items);

        File { imports, items }
    }

    fn rs_ast_imports_to_string(&self) -> String {
        panic!("Not yet implemented")
    }

    fn rs_ast_items_to_string(&self) -> String {
        self.items.iter().fold(String::new(), |acc, item| {
            let mut acc = acc.clone();

            let output = match item {
                RootStatements::StaticVariableDeclaration(item) => format!("{};", item),
                RootStatements::FunctionDeclaration(item) => format!("{}", item),
            };

            acc.push_str(output.as_str());
            acc.push_str("\n");

            acc
        })
    }
}

impl Display for File {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output_imports = match !self.imports.is_empty() {
            true => Some(self.rs_ast_imports_to_string()),
            false => None,
        };

        let output_items = match !self.items.is_empty() {
            true => Some(self.rs_ast_items_to_string()),
            false => None,
        };

        let mut output = String::new();

        let has_both = output_imports.is_some() && output_items.is_some();

        if output_imports.is_some() {
            output.push_str(output_imports.unwrap().as_str());
        };

        if has_both {
            output.push('\n');
        }

        if output_items.is_some() {
            output.push_str(output_items.unwrap().as_str());
        }

        write!(f, "{}", output)
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: String,
    pub items: Vec<ImportItem>,
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub exported_name: String,
    pub as_name: Option<String>,
}

#[derive(Debug, Clone)]
pub enum RootStatements {
    StaticVariableDeclaration(StaticVariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

impl RootStatements {
    pub fn from_ts_var_decl(ts_var_decl: &TsVarDeclarator, exported: bool) -> Self {
        RootStatements::StaticVariableDeclaration(StaticVariableDeclaration::from_ts_var_decl(
            ts_var_decl,
            exported,
        ))
    }

    pub fn from_ts_fn_decl(ts_fn_decl: &TsFnDecl, exported: bool) -> Self {
        RootStatements::FunctionDeclaration(FunctionDeclaration::from_ts_fn_decl(
            ts_fn_decl, exported,
        ))
    }
}

#[derive(Debug, Clone)]
pub struct StaticVariableDeclaration {
    pub variable_name: Identifier,
    pub type_annotation: Type,
    pub initialization: Initialization,
    pub exported: bool,
}

impl StaticVariableDeclaration {
    pub fn from_ts_var_decl(ts_var_decl: &TsVarDeclarator, exported: bool) -> Self {
        let variable_name_ident = ts_var_decl.clone().name.ident().unwrap();

        let init = ts_var_decl.init.clone().unwrap();

        let variable_type = variable_name_ident.clone().type_ann;

        if variable_type.is_none() {
            panic!(
                "Cannot define static variables without a type {:#?}",
                ts_var_decl
            );
        }

        let variable_type = variable_type.unwrap();

        StaticVariableDeclaration {
            variable_name: Identifier::from_ts_biding_ident(variable_name_ident),
            type_annotation: Type::from_ts_type_ann(variable_type),
            initialization: Initialization::from_ts_expr(init),
            exported,
        }
    }
}

impl Display for StaticVariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.exported {
            write!(f, "pub ")?
        }

        write!(
            f,
            "static {}: {} = {}",
            self.variable_name, self.type_annotation, self.initialization
        )
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub is_public: bool,
    pub body: Vec<Statement>,
    pub exported: bool,
}

impl FunctionDeclaration {
    pub fn from_ts_fn_decl(ts_fn_decl: &TsFnDecl, exported: bool) -> Self {
        let return_type = match ts_fn_decl.clone().function.return_type {
            Some(ts_type_ann) => Some(Type::from_ts_type_ann(ts_type_ann)),
            None => None,
        };

        let disallow_return_statement = match return_type {
            Some(_) => false,
            None => true,
        };

        FunctionDeclaration {
            name: Identifier::from_ts_ident(&ts_fn_decl.clone().ident),
            parameters: ts_fn_decl
                .function
                .params
                .iter()
                .map(|ts_param| FunctionParameter::from_ts_param(ts_param))
                .collect(),
            return_type,
            is_public: false,
            body: FunctionDeclaration::body_from_ts_block_statement(
                ts_fn_decl.function.body.clone().unwrap(),
                disallow_return_statement,
            ),
            exported,
        }
    }

    fn body_from_ts_block_statement(
        ts_block_stmt: TsBlockStmt,
        disallow_return_statement: bool,
    ) -> Vec<Statement> {
        let mut statements = vec![];

        for ts_stmt in ts_block_stmt.stmts.iter() {
            if ts_stmt.is_decl() {
                let ts_decl = ts_stmt.as_decl().unwrap();

                if ts_decl.is_var() {
                    let ts_var_decl = ts_decl.as_var().unwrap();

                    ts_var_decl.decls.iter().for_each(|var_decl| {
                        statements.push(Statement::VariableDeclaration(
                            VariableDeclaration::from_ts_var_declarator(var_decl),
                        ))
                    });
                } else {
                    panic!("Unsupported declaration {:#?}", ts_decl);
                }
            } else if ts_stmt.is_return_stmt() {
                if disallow_return_statement {
                    panic!("Cannot have a return statement on a function with no return type");
                }

                let ts_return_stmt = ts_stmt.as_return_stmt().unwrap();

                statements.push(Statement::Return(Return::from_ts_return_stmt(
                    ts_return_stmt,
                )))
            } else {
                panic!("Unsupported statement {:#?}", ts_stmt);
            }
        }

        statements
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.exported {
            write!(f, "pub ")?;
        }

        write!(f, "fn {}(", self.name)?;

        let params = self.parameters.clone();

        let mut first = true;

        for param in params {
            if first {
                first = false;
                write!(f, "{}", param)?;
            } else {
                write!(f, ", {}", param)?;
            }
        }

        write!(f, ")")?;

        if self.return_type.is_some() {
            write!(f, " -> {}", self.return_type.as_ref().unwrap())?;
        }

        write!(f, " {{\n")?;

        let body = self.body.clone();

        for statement in body {
            write!(f, "    {}\n", statement)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub type_annotation: Type,
}

impl FunctionParameter {
    pub fn from_ts_param(ts_param: &TsParam) -> Self {
        let ts_pat = ts_param.clone().pat;

        if ts_pat.is_rest() {
            panic!("We do not support variadic parameters {:#?}", ts_pat);
        }

        let ts_ident = ts_pat.ident().unwrap();

        FunctionParameter {
            name: Identifier::from_ts_biding_ident(ts_ident.clone()),
            type_annotation: Type::from_ts_type_ann(ts_ident.type_ann.unwrap()),
        }
    }
}

impl Display for FunctionParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_annotation)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Return(Return),
    // Expression,
    // FunctionDeclaration,
    // If,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::VariableDeclaration(decl) => write!(f, "{}", decl),
            Statement::Return(ret) => write!(f, "{}", ret),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub variable_name: Identifier,
    pub type_annotation: Type,
    pub initialization: Initialization,
}

impl VariableDeclaration {
    pub fn from_ts_var_declarator(ts_var_decl: &TsVarDeclarator) -> Self {
        let variable_name_ident = ts_var_decl.clone().name.ident().unwrap();

        let init = ts_var_decl.init.clone().unwrap();

        let variable_type = variable_name_ident.clone().type_ann;

        if variable_type.is_none() {
            panic!("Cannot define variables without a type {:#?}", ts_var_decl);
        }

        let variable_type = variable_type.unwrap();

        VariableDeclaration {
            variable_name: Identifier::from_ts_biding_ident(variable_name_ident),
            type_annotation: Type::from_ts_type_ann(variable_type),
            initialization: Initialization::from_ts_expr(init),
        }
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {}: {} = {};",
            self.variable_name, self.type_annotation, self.initialization
        )
    }
}

#[derive(Debug, Clone)]
pub enum Return {
    Expression(Expression),
    Identifier(Identifier),
    Literal(Literal),
}

impl Return {
    pub fn from_ts_return_stmt(ts_return_stmt: &TsReturnStmt) -> Self {
        let ts_expr = ts_return_stmt.arg.clone().unwrap();

        if ts_expr.is_bin() {
            Return::Expression(Expression::from_ts_bin_expr(ts_expr.as_bin().unwrap()))
        } else if ts_expr.is_ident() {
            Return::Identifier(Identifier::from_ts_ident(ts_expr.as_ident().unwrap()))
        } else if ts_expr.is_lit() {
            Return::Literal(Literal::from_ts_lit(ts_expr.as_lit().unwrap()))
        } else {
            panic!("Unhandled return type {:#?}", ts_expr);
        }
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "return {};",
            match self {
                Return::Expression(expr) => expr.to_string(),
                Return::Identifier(ident) => ident.to_string(),
                Return::Literal(lit) => lit.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: String,
}

impl Type {
    pub fn from_ts_type_ann(variable_type: Box<TsTypeAnn>) -> Self {
        let name = if variable_type.type_ann.is_ts_keyword_type() {
            let ts_keyword_type = variable_type.type_ann.as_ts_keyword_type();

            if ts_keyword_type.is_none() {
                panic!(
                    "Cannot create variables without a type {:#?}",
                    variable_type.type_ann
                );
            }

            let ts_keyword_type = ts_keyword_type.unwrap();

            match ts_keyword_type.kind {
                TsKeywordTypeKind::TsStringKeyword => "String".to_string(),
                TsKeywordTypeKind::TsNumberKeyword => "f64".to_string(),
                TsKeywordTypeKind::TsBooleanKeyword => "bool".to_string(),
                _ => panic!("Unsupported keyword {:#?}", ts_keyword_type),
            }
        } else {
            panic!("Unsupported type {:?}", variable_type.type_ann);
        };

        Type { name }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Initialization {
    Number(NumberValue),
    String(StringValue),
    Boolean(BooleanValue),
    Expression(Expression),
    // Template(TemplateValue),
    // Array(ArrayValue),
    // Object(ObjectValue),
}

impl Initialization {
    pub fn from_ts_expr(expr: Box<TsExpr>) -> Self {
        if expr.is_lit() {
            Initialization::from_ts_lit(expr.as_lit().unwrap())
        } else if expr.is_bin() {
            Initialization::from_ts_bin_expr(expr.as_bin().unwrap())
        } else {
            panic!("Not handled expression: {:#?}", expr);
        }
    }

    fn from_ts_lit(lit: &TsLit) -> Initialization {
        if lit.is_num() {
            Initialization::Number(NumberValue {
                value: lit.as_num().unwrap().value,
            })
        } else if lit.is_str() {
            Initialization::String(StringValue {
                value: lit.as_str().unwrap().value.to_string(),
            })
        } else if lit.is_bool() {
            Initialization::Boolean(BooleanValue {
                value: lit.as_bool().unwrap().value,
            })
        } else {
            panic!("Not handled literal: {:#?}", lit);
        }
    }

    fn from_ts_bin_expr(ts_bin_expr: &TsBinExpr) -> Self {
        Initialization::Expression(Expression::from_ts_bin_expr(ts_bin_expr))
    }
}

impl Display for Initialization {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Initialization::Number(value) => write!(f, "{}", value),
            Initialization::String(value) => write!(f, "{}", value),
            Initialization::Boolean(value) => write!(f, "{}", value),
            Initialization::Expression(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub op: Operation,
    pub left: ExpressionPart,
    pub right: ExpressionPart,
}

impl Expression {
    pub fn from_ts_bin_expr(expr: &TsBinExpr) -> Self {
        let op = Operation::from_ts_binary_op(expr.clone().op);

        let left = ExpressionPart::from_ts_expr(expr.clone().left);
        let right = ExpressionPart::from_ts_expr(expr.clone().right);

        Expression { op, left, right }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionPart {
    Literal(Literal),
    Identifier(Identifier),
}

impl ExpressionPart {
    pub fn from_ts_expr(ts_expr: Box<TsExpr>) -> Self {
        if ts_expr.is_lit() {
            ExpressionPart::Literal(Literal::from_ts_lit(ts_expr.as_lit().unwrap()))
        } else if ts_expr.is_ident() {
            ExpressionPart::Identifier(Identifier::from_ts_ident(ts_expr.as_ident().unwrap()))
        } else {
            panic!("Not handled expression: {:#?}", ts_expr);
        }
    }
}

impl Display for ExpressionPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionPart::Literal(value) => write!(f, "{}", value),
            ExpressionPart::Identifier(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NumberValue {
    pub value: f64,
}

impl Display for NumberValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StringValue {
    pub value: String,
}

impl Display for StringValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "String::from(\"{}\")", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct BooleanValue {
    pub value: bool,
}

impl Display for BooleanValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
    pub value: Vec<Initialization>,
}

#[derive(Debug, Clone)]
pub struct ObjectValue {
    pub value: HashMap<String, Initialization>,
}

#[derive(Debug, Clone)]
pub struct TemplateValue {
    pub args: Vec<Initialization>,
}

#[derive(Debug, Clone)]
pub enum Operation {
    ArithmeticOperation(ArithmeticOperation),
    // LogicalOperation(LogicalOperation),
    // BitwiseOperation(BitwiseOperation),
    // UnaryOperation(UnaryOperation),
}

impl Operation {
    pub fn from_ts_binary_op(op: TsBinaryOp) -> Self {
        match op {
            TsBinaryOp::Add => Operation::ArithmeticOperation(ArithmeticOperation::Add),
            TsBinaryOp::Sub => Operation::ArithmeticOperation(ArithmeticOperation::Subtract),
            TsBinaryOp::Mul => Operation::ArithmeticOperation(ArithmeticOperation::Multiply),
            TsBinaryOp::Div => Operation::ArithmeticOperation(ArithmeticOperation::Divide),
            TsBinaryOp::Mod => Operation::ArithmeticOperation(ArithmeticOperation::Modulus),
            _ => panic!("Not handled operation: {:#?}", op),
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::ArithmeticOperation(op) => write!(f, "{}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ArithmeticOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
}

impl Display for ArithmeticOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithmeticOperation::Add => write!(f, "+"),
            ArithmeticOperation::Subtract => write!(f, "-"),
            ArithmeticOperation::Multiply => write!(f, "*"),
            ArithmeticOperation::Divide => write!(f, "/"),
            ArithmeticOperation::Modulus => write!(f, "%"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LogicalOperation {
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum BitwiseOperation {
    And,
    Or,
    XOr,
    Not,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Negate,
    TypeAssertion,
}

#[derive(Debug, Clone)]
pub enum Literal {
    NumberLiteral(NumberValue),
    StringLiteral(StringValue),
    BooleanLiteral(BooleanValue),
}

impl Literal {
    pub fn from_ts_lit(ts_lit: &TsLit) -> Self {
        if ts_lit.is_num() {
            Literal::NumberLiteral(NumberValue {
                value: ts_lit.as_num().unwrap().value,
            })
        } else if ts_lit.is_str() {
            Literal::StringLiteral(StringValue {
                value: ts_lit.as_str().unwrap().value.to_string(),
            })
        } else if ts_lit.is_bool() {
            Literal::BooleanLiteral(BooleanValue {
                value: ts_lit.as_bool().unwrap().value,
            })
        } else {
            panic!("Not handled literal: {:#?}", ts_lit);
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::NumberLiteral(value) => write!(f, "{}", value),
            Literal::StringLiteral(value) => write!(f, "{}", value),
            Literal::BooleanLiteral(value) => write!(f, "{}", value),
        }
    }
}
