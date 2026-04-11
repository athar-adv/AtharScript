#![allow(unused)]

use std::collections::HashMap;

use crate::ty::{Type, FunctionType, TypeArena, TypeId, ClassType};

use crate::vm::{
    Opcode, RuntimeValue, ConstantValue, FunctionProto,
    UpvalueDescriptor, UpvalueSource, NativeFunctionProto,
    pack_abx, pack_abc,
};

use crate::parser::{
    AstNode, BindingNode, TypeNode, ClassMember,
};

use crate::operator::Operator;

fn type_error(msg: &str) -> ! {
    panic!("TypeError: {}", msg)
}

#[derive(Debug)]
pub struct Local {
    pub reg:     usize,
    pub ty:      Type,
    pub backing: Option<ConstantValue>,
    pub mutable: bool,
    pub moved:   bool,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub children: HashMap<String, Namespace>,
    pub locals:   HashMap<String, (usize, bool)>,
    pub types:    HashMap<String, Type>,
}

impl Namespace {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            locals:   HashMap::new(),
            types:    HashMap::new(),
        }
    }
}

pub struct NamespaceBuilder<'a> {
    compiler:  &'a mut LucyCompiler,
    namespace: Namespace,
}

impl<'a> NamespaceBuilder<'a> {
    pub fn new(compiler: &'a mut LucyCompiler) -> Self {
        Self { compiler, namespace: Namespace::new() }
    }

    pub fn function(
        mut self,
        name:  &str,
        arity: u8,
        func:  fn(Vec<RuntimeValue>) -> RuntimeValue,
    ) -> Self {
        let idx = self.compiler.lulib_register_native_fn(name, arity, func);
        self.namespace.locals.insert(name.to_string(), (idx, true));
        self
    }

    pub fn build(self) -> Namespace { self.namespace }
}

#[derive(Debug)]
pub struct Scope {
    pub locals:     HashMap<String, Local>,
    pub exports:    HashMap<String, usize>,
    pub types:      HashMap<String, Type>,
    pub namespaces: HashMap<String, Namespace>,
    pub proto_depth: usize,
    reg_base:        usize,
}

pub struct RegisterAllocator {
    pub current_top: usize,
}

impl RegisterAllocator {
    fn alloc(&mut self) -> usize {
        let r = self.current_top;
        self.current_top += 1;
        r
    }
    fn free_to(&mut self, top: usize) {
        self.current_top = top;
    }
}

#[derive(Debug)]
pub enum LocalResolution {
    Local    { reg: usize, ty: Type, backing: Option<ConstantValue>, mutable: bool, moved: bool },
    OuterProto { reg: usize, ty: Type, backing: Option<ConstantValue>, mutable: bool, moved: bool },
}

#[derive(Debug)]
pub struct ScopeStack {
    pub scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self { Self { scopes: vec![] } }

    fn push(&mut self, reg_base: usize, proto_depth: usize) {
        self.scopes.push(Scope {
            locals:     HashMap::new(),
            exports:    HashMap::new(),
            types:      HashMap::new(),
            namespaces: HashMap::new(),
            proto_depth,
            reg_base,
        });
    }

    fn pop(&mut self) -> usize {
        self.scopes.pop().expect("popped empty scope stack").reg_base
    }

    pub fn mark_moved(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(local) = scope.locals.get_mut(name) {
                local.moved = true;
                return;
            }
        }
    }

    pub fn define_local(
        &mut self,
        name: String, reg: usize, ty: Type,
        backing: Option<ConstantValue>, mutable: bool,
    ) {
        self.get_current_scope_mut().locals.insert(
            name, Local { reg, ty, backing, mutable, moved: false },
        );
    }

    pub fn define_export(
        &mut self,
        name: String, reg: usize, ty: Type,
        backing: Option<ConstantValue>, mutable: bool,
    ) {
        let scope = self.get_current_scope_mut();
        scope.exports.insert(name.clone(), reg);
        scope.locals.insert(name, Local { reg, ty, backing, mutable, moved: false });
    }

    pub fn define_type(&mut self, name: String, ty: Type) {
        self.get_current_scope_mut().types.insert(name, ty);
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) { return Some(t); }
        }
        None
    }

    pub fn resolve_local(&self, name: &str, current_proto_depth: usize) -> Option<LocalResolution> {
        for scope in self.scopes.iter().rev() {
            if let Some(local) = scope.locals.get(name) {
                let res = if scope.proto_depth == current_proto_depth {
                    LocalResolution::Local {
                        reg: local.reg, ty: local.ty.clone(),
                        backing: local.backing.clone(),
                        mutable: local.mutable,
                        moved: local.moved,
                    }
                } else {
                    LocalResolution::OuterProto {
                        reg: local.reg,
                        ty: local.ty.clone(),
                        backing: local.backing.clone(),
                        mutable: local.mutable,
                        moved: local.moved
                    }
                };
                return Some(res);
            }
        }
        None
    }

    pub fn define_namespace(&mut self, name: String, ns: Namespace)
    {
        let scope = self.get_current_scope_mut();
        scope.namespaces.insert(name, ns);
    }

    pub fn get_current_scope(&self) -> &Scope {
        self.scopes.last().expect("no active scope")
    }
    pub fn get_current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("no active scope")
    }
}

#[derive(Clone)]
struct CompilingCtx {
    pub is_public:     bool,
    /// Expected return type of the current function (Unknown = infer first return).
    pub return_type:   Type,
    /// Name of the class currently being compiled (for `self` resolution).
    pub current_class: Option<String>,
}

impl CompilingCtx {
    fn new() -> Self {
        Self { is_public: false, return_type: Type::Unknown, current_class: None }
    }
}

pub struct LucyCompiler {
    pub reg_alloc: RegisterAllocator,
    pub scopes: ScopeStack,
    pub proto_stack: Vec<FunctionProto>,
    pub native_protos: Vec<NativeFunctionProto>,
    pub native_namespaces: HashMap<String, Namespace>,
    pub proto_depth: usize,

    pub type_arena: TypeArena,
}

impl LucyCompiler {
    pub fn lulib_openlib(
        &mut self,
        path:  &str,
        build: impl FnOnce(NamespaceBuilder) -> NamespaceBuilder,
    ) {
        let ns = build(NamespaceBuilder::new(self)).build();
        self.native_namespaces.insert(path.to_string(), ns);
    }

    pub fn lulib_register_native_fn(
        &mut self, name: &str, arity: u8, func: fn(Vec<RuntimeValue>) -> RuntimeValue,
    ) -> usize {
        let idx = self.native_protos.len();
        self.native_protos.push(NativeFunctionProto {
            name: name.to_string(), arity, func,
        });
        idx
    }

    pub fn lulib_register_namespace(
        &mut self,
        name:  &str,
        build: impl FnOnce(NamespaceBuilder) -> NamespaceBuilder,
    ) {
        let ns = build(NamespaceBuilder::new(self)).build();
        self.scopes.get_current_scope_mut().namespaces.insert(name.to_string(), ns);
    }
}

impl LucyCompiler {
    pub fn new() -> Self {
        let mut s = Self {
            reg_alloc:         RegisterAllocator { current_top: 0 },
            scopes:            ScopeStack::new(),
            proto_stack:       vec![],
            native_protos:     vec![],
            native_namespaces: HashMap::new(),
            proto_depth:       0,
            type_arena:        TypeArena::new(),
        };
        s.enter_proto("__main__".to_string(), 0);
        s
    }

    pub fn compile(&mut self, program: &AstNode) {
        let ctx = CompilingCtx::new();
        self.compile_stmt(program, &ctx);
        self.emit(pack_abc(Opcode::RET as u32, 0, 0, 0));
    }

    pub fn enter_scope(&mut self) {
        let base = self.reg_alloc.current_top;
        self.scopes.push(base, self.proto_depth);
    }

    pub fn exit_scope(&mut self) {
        let base = self.scopes.pop();
        self.reg_alloc.free_to(base);
    }

    fn current_proto(&mut self) -> &mut FunctionProto {
        self.proto_stack.last_mut().expect("no active proto")
    }

    fn emit(&mut self, op: u32) -> usize {
        let proto = self.current_proto();
        proto.code.push(op);
        proto.code.len() - 1
    }

    fn add_constant(&mut self, c: ConstantValue) -> usize {
        if let Some(i) = self.current_proto().constants.iter().position(|x| *x == c) {
            return i;
        }
        let proto = self.current_proto();
        proto.constants.push(c);
        proto.constants.len() - 1
    }

    fn enter_proto(&mut self, name: String, arity: u8) {
        self.proto_depth += 1;
        let saved_top = self.reg_alloc.current_top;
        self.reg_alloc.current_top = 0;
        self.proto_stack.push(FunctionProto {
            name, arity, max_regs: 0,
            code: vec![], constants: vec![], protos: vec![],
            upvalues: vec![], saved_reg_top: saved_top,
        });
    }

    fn exit_proto(&mut self) -> FunctionProto {
        self.proto_depth -= 1;
        let proto = self.proto_stack.pop().expect("no active proto");
        self.reg_alloc.current_top = proto.saved_reg_top;
        proto
    }

    fn capture_upvalue(&mut self, name: &str, source: UpvalueSource, ty: Type) -> usize {
        if let Some(idx) = self.current_proto()
            .upvalues.iter()
            .position(|u| u.name == name)
        {
            return idx;
        }
        let uv  = UpvalueDescriptor { name: name.to_string(), source, ty };
        let idx = self.current_proto().upvalues.len();
        self.current_proto().upvalues.push(uv);
        idx
    }
}

impl LucyCompiler {
    fn get_class(&self, ty: &Type) -> Option<&ClassType> {
        match ty {
            Type::Class(id) => Some(self.type_arena.get_class(*id)),
            _ => None,
        }
    }

    fn get_class_mut(&mut self, ty: &Type) -> Option<&mut ClassType> {
        match ty {
            Type::Class(id) => Some(self.type_arena.get_class_mut(*id)),
            _ => None,
        }
    }
}

impl LucyCompiler {
    fn compile_type(&self, node: &TypeNode) -> Type {
        match node {
            TypeNode::Inferred => Type::Unknown,

            TypeNode::ArrayType { elem_type } =>
                Type::Array(Box::new(self.compile_type(elem_type))),

            TypeNode::Qualified { inner, mutable, borrowed, moved } =>
                Type::Qualified {
                    inner:    Box::new(self.compile_type(inner)),
                    mutable:  *mutable,
                    borrowed: *borrowed,
                    moved:    *moved,
                },

            TypeNode::NominalType { name, generics } => {
                let args: Vec<Type> = generics.iter()
                    .map(|g| self.compile_type(g))
                    .collect();

                if let Some(ty) = Self::resolve_builtin(name) { return ty; }

                // Check scope type registry
                if let Some(class_ty) = self.scopes.lookup_type(name) {
                    return class_ty.clone();
                }

                if args.is_empty() {
                    Type::TypeVar(name.clone())
                } else {
                    Type::Generic { name: name.clone(), args }
                }
            }
        }
    }

    fn resolve_builtin(name: &str) -> Option<Type> {
        match name {
            "u8"     => Some(Type::U8),
            "i8"     => Some(Type::I8),
            "u16"    => Some(Type::U16),
            "i16"    => Some(Type::I16),
            "u32"    => Some(Type::U32),
            "i32"    => Some(Type::I32),
            "u64"    => Some(Type::U64),
            "i64"    => Some(Type::I64),
            "usize"  => Some(Type::USize),
            "bool"   => Some(Type::Bool),
            "string" => Some(Type::String),
            "empty"  => Some(Type::Empty),
            _        => None,
        }
    }

    fn infer_expr_type(&self, expr: &AstNode, ctx: &CompilingCtx) -> Type {
        match expr {
            AstNode::IntLiteral(_)    => Type::I32,
            AstNode::FloatLiteral(_)  => Type::F64,  // default float type
            AstNode::StringLiteral(_) => Type::String,
            AstNode::SelfExpr => {
                ctx.current_class.as_ref()
                    .and_then(|n| self.scopes.lookup_type(n))
                    .cloned()
                    .unwrap_or(Type::Unknown)
            }
            AstNode::Identifier(name) => {
                match self.scopes.resolve_local(name, self.proto_depth) {
                    Some(LocalResolution::Local { ty, .. })
                    | Some(LocalResolution::OuterProto { ty, .. }) => ty,
                    None => Type::Unknown,
                }
            }
            AstNode::Borrowed(inner) => {
                let inner_ty = self.infer_expr_type(inner, ctx);
                Type::Qualified {
                    inner:    Box::new(inner_ty),
                    mutable:  false,
                    borrowed: true,
                    moved:    false,
                }
            }
            AstNode::Moved(inner) => {
                let inner_ty = self.infer_expr_type(inner, ctx);
                Type::Qualified {
                    inner:    Box::new(inner_ty),
                    mutable:  false,
                    borrowed: false,
                    moved:    true,
                }
            }
            AstNode::BinaryOperation { op, left, right } => {
                let lt = self.infer_expr_type(left, ctx);
                let rt = self.infer_expr_type(right, ctx);
                // Float promotion
                if matches!(lt, Type::F64) || matches!(rt, Type::F64) { return Type::F64; }
                if matches!(lt, Type::F32) || matches!(rt, Type::F32) { return Type::F32; }
                if lt != Type::Unknown { lt } else { rt }
            }
            AstNode::NamespaceIndex { indexee, index } => {
                let ns_name = match indexee.as_ref() {
                    AstNode::Identifier(s) => s,
                    _ => return Type::Unknown,
                };

                let member_name = match index.as_ref() {
                    AstNode::Identifier(s) => s,
                    _ => return Type::Unknown,
                };

                // Look up namespace
                if let Some(ns) = Self::find_namespace_in_scopes(&self.scopes, ns_name) {
                    // Resolve actual function symbol: "Point::new"
                    let full_name = format!("{}::{}", ns_name, member_name);

                    if let Some(Type::Class(id)) = self.scopes.lookup_type(ns_name) {
                        let class = self.type_arena.get_class(*id);

                        if let Some((_, _, fn_ty, _)) = class.methods.get(member_name) {
                            return Type::Function(Box::new(fn_ty.clone()));
                        }
                    }
                }

                Type::Unknown
            }
            AstNode::FunctionCall { callee, .. } => {
                let callee_ty = self.infer_expr_type(callee, ctx);
                match callee_ty {
                    Type::Function(ft) => *ft.return_type,
                    _ => Type::Unknown,
                }
            }
            AstNode::DotIndex { indexee, index } => {
                let obj_ty = self.infer_expr_type(indexee, ctx);
                let name = match index.as_ref() {
                    AstNode::Identifier(s) => s.as_str(),
                    _ => return Type::Unknown,
                };

                match &obj_ty {
                    Type::Class(id) => {
                        let class = self.type_arena.get_class(*id);

                        if let Some((_, ty, _)) = class.fields.iter().find(|(n, _, _)| n == name) {
                            return ty.clone();
                        }

                        if let Some((_, _, fn_ty, _)) = class.methods.get(name) {
                            return Type::Function(Box::new(fn_ty.clone()));
                        }

                        Type::Unknown
                    }
                    _ => Type::Unknown,
                }
            }
            AstNode::ClassLiteral { ty, .. } => {
                let class_name = match ty.as_ref() {
                    AstNode::Identifier(s) => s.clone(),
                    AstNode::SelfExpr => {
                        ctx.current_class.clone().unwrap_or_else(|| {
                            type_error("Self used outside of class")
                        })
                    }
                    other => return Type::Unknown,
                };
                self.scopes.lookup_type(&class_name).cloned().unwrap_or(Type::Unknown)
            }
            _ => Type::Unknown,
        }
    }

    fn resolve_type<'a>(&self, ty: &'a Type, context: &str) -> &'a Type {
        match ty {
            Type::Qualified { inner, borrowed, moved, mutable } => {
                if *borrowed && *moved {
                    type_error(&format!(
                        "{}: type cannot be both borrowed and moved", context
                    ));
                }
                
                self.resolve_type(inner, context)
            }
            other => other,
        }
    }

    fn assert_assignable(&self, lhs: &Type, rhs: &Type, context: &str) {
        if matches!(lhs, Type::Unknown) || matches!(rhs, Type::Unknown) { return; }
        let l = lhs.inner();
        let r = rhs.inner();
        if l != r {
            type_error(&format!(
                "{}: expected '{}', got '{}'",
                context, lhs, rhs
            ));
        }
    }

    fn assert_numeric(&self, ty: &Type, context: &str) {
        if matches!(ty, Type::Unknown) { return; }
        if !ty.inner().is_numeric() {
            type_error(&format!("{}: '{}' is not a numeric type", context, ty));
        }
    }
}

impl Type {
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::U8  | Type::I8  |
            Type::U16 | Type::I16 |
            Type::U32 | Type::I32 |
            Type::U64 | Type::I64 |
            Type::USize
        )
    }
    fn is_numeric_or_float(&self) -> bool {
        self.is_numeric() || matches!(self, Type::F32 | Type::F64)
    }
    fn inner(&self) -> &Type {
        match self {
            Type::Qualified { inner, .. } => inner.as_ref(),
            other => other,
        }
    }
    fn is_mutable(&self) -> bool {
        match self {
            Type::Qualified { mutable, .. } => *mutable,
            _ => false,
        }
    }
}

impl LucyCompiler {
    fn compile_body(&mut self, stmts: &[AstNode], ctx: &CompilingCtx) {
        self.enter_scope();
        for stmt in stmts { self.compile_stmt(stmt, ctx); }
        self.exit_scope();
    }

    fn compile_stmt(&mut self, stmt: &AstNode, ctx: &CompilingCtx) {
        match stmt {
            AstNode::Program(stmts) => {
                self.enter_scope();
                for node in stmts { self.compile_stmt(node, ctx); }

                // Auto-call `main` if defined
                if let Some(LocalResolution::Local { reg, .. }) =
                    self.scopes.resolve_local("main", self.proto_depth)
                {
                    self.emit(pack_abc(Opcode::CALL as u32, reg as u32, 0, 1));
                }
                self.exit_scope();
            }

            AstNode::StaticImportStmt { namespace_alias, path } => {
                let namespace = if let Some(ns) = self.native_namespaces.get(path) {
                    ns.clone()
                } else {
                    use std::fs;
                    use crate::lexer;
                    use crate::parser::LucyParser;
                    let source = fs::read_to_string(path)
                        .unwrap_or_else(|e| panic!("Cannot read '{}': {}", path, e));
                    let tokens = lexer::tokenize(source);
                    let ast    = LucyParser::new(tokens).parse_file_source();
                    self.compile_import_file(&ast, ctx)
                };
                self.scopes.get_current_scope_mut()
                    .namespaces.insert(namespace_alias.clone(), namespace);
            }

            AstNode::UseStmt { base_path, used } => {
                let resolved: Vec<(String, usize, Type)> = {
                    let namespace = Self::resolve_namespace_path(&self.scopes, base_path)
                        .unwrap_or_else(|| panic!("Cannot resolve use path: {:?}", base_path));
                    used.iter().map(|(actual, alias)| {
                        let (native_idx, _) = *namespace.locals.get(actual)
                            .unwrap_or_else(|| panic!("'{}' not found in namespace", actual));
                        (alias.clone(), native_idx, Type::Unknown)
                    }).collect()
                };
                for (alias, native_idx, ty) in resolved {
                    let cv        = ConstantValue::NativeFunctionProto(native_idx);
                    let const_idx = self.add_constant(cv.clone());
                    let dst       = self.reg_alloc.alloc();
                    self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, const_idx as u32));
                    self.scopes.define_local(alias, dst, ty, Some(cv), false);
                }
            }

            AstNode::Public(inner) => {
                let mut pub_ctx = ctx.clone();
                pub_ctx.is_public = true;
                self.compile_stmt(inner, &pub_ctx);
            }

            AstNode::VarDeclaration { binding, init_value } => {
                match binding {
                    BindingNode::IdentifierBinding { name, ty } => {
                        let dst          = self.reg_alloc.alloc();
                        let declared_ty  = self.compile_type(ty);
                        let is_mutable   = declared_ty.is_mutable();

                        // Infer type from initialiser if declared type is Unknown
                        let resolved_ty = if matches!(declared_ty, Type::Unknown) {
                            if let Some(expr) = init_value {
                                self.infer_expr_type(expr, ctx)
                            } else {
                                Type::Unknown
                            }
                        } else {
                            declared_ty.clone()
                        };

                        if ctx.is_public {
                            self.scopes.define_export(
                                name.clone(), dst, resolved_ty.clone(), None, is_mutable,
                            );
                        } else {
                            self.scopes.define_local(
                                name.clone(), dst, resolved_ty.clone(), None, is_mutable,
                            );
                        }

                        if let Some(expr) = init_value {
                            // Type-check the initialiser
                            let init_ty = self.infer_expr_type(expr, ctx);
                            if !matches!(declared_ty, Type::Unknown) {
                                self.assert_assignable(&declared_ty, &init_ty,
                                    &format!("assignment to '{}'", name));
                            }
                            self.compile_expr(expr, dst, ctx);
                        }
                    }
                    other => panic!("Unhandled binding in VarDeclaration: {:?}", other),
                }
            }

            AstNode::FunctionDeclaration { name, params, type_params, return_type, body } => {
                self.compile_function_decl(name, params, return_type, body, false, ctx);
            }

            AstNode::ClassDefinition { name, members } => {
                self.compile_class_definition(name, members, ctx);
            }

            AstNode::ReturnStmt { value } => {
                match value {
                    Some(expr) => {
                        let src = self.reg_alloc.alloc();
                        let ret_ty = self.infer_expr_type(expr, ctx);

                        // Type-check return value against declared return type
                        if !matches!(ctx.return_type, Type::Unknown) {
                            self.assert_assignable(&ctx.return_type, &ret_ty,
                                "return statement");
                        }

                        self.compile_expr(expr, src, ctx);
                        self.emit(pack_abc(Opcode::RET as u32, src as u32, 1, 0));
                        self.reg_alloc.free_to(src);
                    }
                    None => {
                        if !matches!(ctx.return_type, Type::Unknown | Type::Empty) {
                            type_error(&format!(
                                "bare return in function expecting '{}'", ctx.return_type
                            ));
                        }
                        self.emit(pack_abc(Opcode::RET as u32, 0, 0, 0));
                    }
                }
            }

            AstNode::FunctionCall { .. } | AstNode::BinaryOperation { .. } => {
                let scratch = self.reg_alloc.alloc();
                self.compile_expr(stmt, scratch, ctx);
                self.reg_alloc.free_to(scratch);
            }

            AstNode::Assignment { left, right } => {
                match left.as_ref() {
                    AstNode::Identifier(name) => {
                        match self.scopes.resolve_local(name, self.proto_depth) {
                            Some(LocalResolution::Local { reg, ty, mutable, .. }) => {
                                if !mutable {
                                    type_error(&format!(
                                        "cannot assign to immutable variable '{}'", name
                                    ));
                                }
                                let rhs_ty = self.infer_expr_type(right, ctx);
                                self.assert_assignable(&ty, &rhs_ty,
                                    &format!("assignment to '{}'", name));
                                self.compile_expr(right, reg, ctx);
                            }
                            Some(LocalResolution::OuterProto { .. }) =>
                                type_error("cannot assign to variable captured from outer scope"),
                            None =>
                                type_error(&format!("undefined variable '{}'", name)),
                        }
                    }
                    AstNode::DotIndex { indexee, index } => {
                        // obj.field = value
                        let obj_reg = self.reg_alloc.alloc();
                        self.compile_expr(indexee, obj_reg, ctx);

                        let field_name = match index.as_ref() {
                            AstNode::Identifier(s) => s.clone(),
                            other => panic!("DotIndex assignment: expected ident, got {:?}", other),
                        };

                        let obj_ty = self.infer_expr_type(indexee, ctx);
                        if let Type::Class(id) = &obj_ty {
                            let class = self.type_arena.get_class(*id);

                            if let Some((_, field_ty, _)) = class.fields.iter().find(|(n, _, _)| n == &field_name) {
                                let rhs_ty = self.infer_expr_type(right, ctx);
                                self.assert_assignable(field_ty, &rhs_ty,
                                    &format!("assignment to field '{}'", field_name));
                            }
                        }

                        let val_reg   = self.reg_alloc.alloc();
                        self.compile_expr(right, val_reg, ctx);

                        let field_index = match &obj_ty {
                            Type::Class(id) => {
                                let class = self.type_arena.get_class(*id);
                                *class.field_index_map.get(&field_name).unwrap()
                            }
                            _ => unreachable!(),
                        };

                        self.emit(pack_abc(
                            Opcode::SETFIELD as u32,
                            obj_reg as u32,
                            val_reg as u32,
                            field_index as u32,
                        ));
                        self.reg_alloc.free_to(obj_reg);
                    }
                    other => panic!("Unsupported assignment target: {:?}", other),
                }
            }

            other => panic!("Unhandled stmt node: {:?}", other),
        }
    }
}

impl LucyCompiler {
    fn compile_class_definition(
        &mut self,
        class_name: &str,
        members:    &[ClassMember],
        ctx:        &CompilingCtx,
    ) {
        let mut field_types = Vec::new();
        let mut field_index_map = HashMap::new();

        for m in members {
            if let ClassMember::Field { name, ty, is_public } = m {
                let t = self.compile_type(ty);
                field_index_map.insert(name.clone(), field_types.len());
                field_types.push((name.clone(), t, *is_public));
            }
        }

        let class_id = self.type_arena.alloc_class(ClassType {
            name: class_name.to_string(),
            fields: field_types,
            field_index_map,
            methods: HashMap::new(),
            operators: HashMap::new(),
            class_proto_constant: None,
        });

        self.scopes.define_type(class_name.to_string(), Type::Class(class_id));

        let mut class_ctx = ctx.clone();
        class_ctx.current_class = Some(class_name.to_string());

        let mut ns = Namespace::new();

        let mut method_proto_indices: HashMap<String, usize> = HashMap::new();
        let mut op_proto_indices: HashMap<String, usize> = HashMap::new();
        // First pass: pre-push empty protos to claim indices and register methods
        for m in members {
            if let ClassMember::Method { name: method_name, params, has_self, is_public, .. } = m {
                let arity = params.len() as u8 + if *has_self { 1 } else { 0 };
                
                let placeholder = FunctionProto {
                    name: format!("{}::{}", class_name, method_name),
                    arity,
                    max_regs: 0,
                    code: vec![],
                    constants: vec![],
                    protos: vec![],
                    upvalues: vec![],
                    saved_reg_top: self.reg_alloc.current_top,
                };
                let local_idx = {
                    let parent = self.current_proto();
                    let idx = parent.protos.len();
                    parent.protos.push(placeholder);
                    idx
                };
                method_proto_indices.insert(method_name.clone(), local_idx);

                // Insert method entry with correct proto_idx from the start
                let class = self.type_arena.get_class_mut(class_id);
                let method_idx = class.methods.len();
                class.methods.insert(
                    method_name.clone(),
                    (method_idx, local_idx, FunctionType { params: vec![], return_type: Box::new(Type::Unknown) }, *is_public)
                );

                ns.locals.insert(method_name.clone(), (local_idx, *is_public));
            }
            else if let ClassMember::OperatorOverload { op, .. } = m {
                let arity = 2; // binary ops only for now (extend later)
                let op_name = format!("{}::operator@{:?}", class_name, op);
                
                let placeholder = FunctionProto {
                    name: op_name.clone(),
                    arity,
                    max_regs: 0,
                    code: vec![],
                    constants: vec![],
                    protos: vec![],
                    upvalues: vec![],
                    saved_reg_top: self.reg_alloc.current_top,
                };

                let local_idx = {
                    let parent = self.current_proto();
                    let idx = parent.protos.len();
                    parent.protos.push(placeholder);
                    idx
                };

                op_proto_indices.insert(op_name.clone(), local_idx);

                let class = self.type_arena.get_class_mut(class_id);
                class.operators.insert(op.clone(), (local_idx, FunctionType { params: vec![], return_type: Box::new(Type::Unknown) }));
            }
        }

        // Now build the ClassProto with correct indices
        {
            let class = self.type_arena.get_class(class_id);
            let field_vis: Vec<bool> = class.fields.iter().map(|(_, _, p)| *p).collect();
            let mut ordered = vec![(404usize, false); class.methods.len()];

            for (_, (method_idx, proto_idx, _, is_public)) in &class.methods {
                ordered[*method_idx] = (*proto_idx, *is_public);
            }
            let mut operators = HashMap::new();
            for (op, (proto_idx, _)) in &class.operators {
                operators.insert(op.clone(), *proto_idx);
            }
            
            self.type_arena.get_class_mut(class_id).class_proto_constant = Some(
                ConstantValue::ClassProto {
                    name: class_name.to_string(),
                    fields: field_vis,
                    methods: ordered,
                    operators,
                }
            );
        }

        self.scopes.define_namespace(class_name.to_string(), ns);

        // Second pass: compile bodies and replace placeholder protos
        for m in members {
            if let ClassMember::Method {
                name: method_name, has_self, params, return_type, body, is_public, ..
            } = m {
                let mut all_params = Vec::new();
                if *has_self {
                    all_params.push(BindingNode::IdentifierBinding {
                        name: "self".to_string(),
                        ty: TypeNode::NominalType {
                            name: class_name.to_string(),
                            generics: vec![],
                        },
                    });
                }
                all_params.extend_from_slice(params);

                let full_name = format!("{}::{}", class_name, method_name);
                let local_idx = method_proto_indices[method_name];

                // Compile into a real proto
                self.enter_proto(full_name.clone(), all_params.len() as u8);
                self.enter_scope();
                for (i, param) in all_params.iter().enumerate() {
                    if let BindingNode::IdentifierBinding { name: pname, ty } = param {
                        let compiled_ty = self.compile_type(ty);
                        self.scopes.define_local(pname.clone(), i, compiled_ty, None, false);
                        self.reg_alloc.alloc();
                    }
                }
                let mut fn_ctx = class_ctx.clone();
                fn_ctx.return_type = self.compile_type(return_type);
                fn_ctx.is_public = false;

                for stmt in body {
                    self.compile_stmt(stmt, &fn_ctx);
                }
                //self.emit(pack_abc(Opcode::RET as u32, 0, 0, 0));
                self.exit_scope();
                let real_proto = self.exit_proto();

                // Replace the placeholder at local_idx
                self.current_proto().protos[local_idx] = real_proto;

                let fn_ty = FunctionType {
                    params: all_params.iter().map(|p| match p {
                        BindingNode::IdentifierBinding { ty, .. } => self.compile_type(ty),
                        _ => Type::Unknown,
                    }).collect(),
                    return_type: Box::new(fn_ctx.return_type.clone()),
                };

                let cv = ConstantValue::FunctionProto(local_idx);
                self.scopes.define_local(
                    full_name.clone(), 0,
                    Type::Function(Box::new(fn_ty.clone())),
                    Some(cv), false,
                );

                let class = self.type_arena.get_class_mut(class_id);
                let entry = class.methods.get_mut(method_name).unwrap();
                entry.2 = fn_ty;
            }
            else if let ClassMember::OperatorOverload { op, params, return_type, body } = m
            {
                let mut all_params = params.clone();

                let full_name = format!("{}::operator@{:?}", class_name, op);
                let local_idx = op_proto_indices[&full_name];

                // Compile into a real proto
                self.enter_proto(full_name.clone(), all_params.len() as u8);
                self.enter_scope();
                for (i, param) in all_params.iter().enumerate() {
                    if let BindingNode::IdentifierBinding { name: pname, ty } = param {
                        let compiled_ty = self.compile_type(ty);
                        self.scopes.define_local(pname.clone(), i, compiled_ty, None, false);
                        self.reg_alloc.alloc();
                    }
                }
                let mut fn_ctx = class_ctx.clone();
                fn_ctx.return_type = self.compile_type(return_type);
                
                for stmt in body {
                    self.compile_stmt(stmt, &fn_ctx);
                }
                //self.emit(pack_abc(Opcode::RET as u32, 0, 0, 0));
                self.exit_scope();
                let real_proto = self.exit_proto();

                // Replace the placeholder at local_idx
                self.current_proto().protos[local_idx] = real_proto;

                let fn_ty = FunctionType {
                    params: all_params.iter().map(|p| match p {
                        BindingNode::IdentifierBinding { ty, .. } => self.compile_type(ty),
                        _ => Type::Unknown,
                    }).collect(),
                    return_type: Box::new(fn_ctx.return_type.clone()),
                };

                let cv = ConstantValue::FunctionProto(local_idx);
                self.scopes.define_local(
                    full_name.clone(), 0,
                    Type::Function(Box::new(fn_ty.clone())),
                    Some(cv), false,
                );

                let class = self.type_arena.get_class_mut(class_id);
                let entry = class.operators.get_mut(op).unwrap();
                entry.1 = fn_ty;
            }
        }
    }

    fn compile_function_decl(
        &mut self,
        name:        &str,
        params:      &[BindingNode],
        return_type: &TypeNode,
        body:        &[AstNode],
        is_method:  bool,
        ctx:         &CompilingCtx,
    ) -> (usize, usize, FunctionType) {
        let arity = params.len() as u8;

        self.enter_proto(name.to_string(), arity);
        self.enter_scope();

        for (i, param) in params.iter().enumerate() {
            match param {
                BindingNode::IdentifierBinding { name: pname, ty } => {
                    let compiled_ty = self.compile_type(ty);
                    if matches!(compiled_ty, Type::Unknown) && pname != "self" {
                        // Params must have explicit types (cannot be inferred reliably)
                        // We allow Unknown only for `self` which is typed by the compiler
                        type_error(&format!(
                            "parameter '{}' of '{}' must have an explicit type", pname, name
                        ));
                    }
                    self.scopes.define_local(pname.clone(), i, compiled_ty, None, false);
                    self.reg_alloc.alloc();
                }
                other => panic!("Unhandled param binding: {:?}", other),
            }
        }

        let declared_ret = match self.compile_type(return_type) {
            Type::TypeVar(ref n) if n == "Self" => {
                if let Some(class) = &ctx.current_class {
                    self.scopes.lookup_type(class)
                        .cloned()
                        .expect("Self type not found")
                } else {
                    Type::Unknown
                }
            }
            other => other,
        };
        
        let mut fn_ctx = ctx.clone();
        fn_ctx.return_type = declared_ret.clone();
        fn_ctx.is_public   = false;

        let mut inferred_ret = Type::Unknown;
        for stmt in body {
            if let AstNode::ReturnStmt { value: Some(expr) } = stmt {
                let t = self.infer_expr_type(expr, &fn_ctx);
                if !matches!(t, Type::Unknown) && matches!(inferred_ret, Type::Unknown) {
                    inferred_ret = t.clone();
                    if matches!(declared_ret, Type::Unknown) {
                        fn_ctx.return_type = t;
                    }
                }
            }
            self.compile_stmt(stmt, &fn_ctx);
        }

        self.emit(pack_abc(Opcode::RET as u32, 0, 0, 0));

        self.exit_scope();
        let proto = self.exit_proto();

        let proto_local_idx = {
            let parent = self.current_proto();
            let idx    = parent.protos.len();
            parent.protos.push(proto);
            idx
        };

        let final_ret = if matches!(declared_ret, Type::Unknown) {
            inferred_ret.clone()
        } else {
            declared_ret.clone()
        };

        let fn_type = FunctionType {
            params: params.iter().map(|p| match p {
                BindingNode::IdentifierBinding { ty, .. } => self.compile_type(ty),
                _ => Type::Unknown,
            }).collect(),
            return_type: Box::new(final_ret),
        };

        if is_method {
            return (0, proto_local_idx, fn_type);
        }

        let cv        = ConstantValue::FunctionProto(proto_local_idx);
        let const_idx = self.add_constant(cv.clone());
        let dst       = self.reg_alloc.alloc();
        self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, const_idx as u32));

        if ctx.is_public {
            self.scopes.define_export(
                name.to_string(), dst, Type::Function(Box::new(fn_type.clone())), Some(cv), false,
            );
        } else {
            self.scopes.define_local(
                name.to_string(), dst, Type::Function(Box::new(fn_type.clone())), Some(cv), false,
            );
        }

        (dst, proto_local_idx, fn_type)
    }
}

impl LucyCompiler {
    fn compile_expr(&mut self, expr: &AstNode, dst: usize, ctx: &CompilingCtx) {
        match expr {
            AstNode::IntLiteral(n) => {
                let k = self.add_constant(ConstantValue::I32(*n));
                self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, k as u32));
            }
            AstNode::FloatLiteral(f) => {
                let k = self.add_constant(ConstantValue::F64(*f));
                self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, k as u32));
            }
            AstNode::StringLiteral(s) => {
                let k = self.add_constant(ConstantValue::String(s.clone()));
                self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, k as u32));
            }

            AstNode::SelfExpr => {
                match self.scopes.resolve_local("self", self.proto_depth) {
                    Some(LocalResolution::Local { reg, .. }) => {
                        if reg != dst {
                            self.emit(pack_abc(Opcode::MOVE as u32, dst as u32, reg as u32, 0));
                        }
                    }
                    _ => type_error("'self' used outside of a method"),
                }
            }

            AstNode::Borrowed(inner) => {
                self.compile_expr(inner, dst, ctx);
            }

            AstNode::Moved(inner) => {
                match inner.as_ref() {
                    AstNode::Identifier(name) => {
                        match self.scopes.resolve_local(name, self.proto_depth) {
                            Some(LocalResolution::Local { moved: true, .. }) => {
                                type_error(&format!("use of already-moved variable '{}'", name));
                            }
                            Some(LocalResolution::OuterProto { .. }) => {
                                type_error(&format!("cannot move '{}' captured from outer scope", name));
                            }
                            None => type_error(&format!("undefined variable '{}'", name)),
                            Some(LocalResolution::Local { reg, .. }) => {
                                if reg != dst {
                                    self.emit(pack_abc(Opcode::MOVE as u32, dst as u32, reg as u32, 0));
                                }
                                self.scopes.mark_moved(name);
                            }
                        }
                    }
                    other => {
                        self.compile_expr(other, dst, ctx);
                    }
                }
            }

            AstNode::Identifier(name) => {
                match self.scopes.resolve_local(name, self.proto_depth) {
                    Some(LocalResolution::Local { reg, moved, .. }) => {
                        if moved {
                            type_error(&format!("use of moved variable '{}'", name));
                        }
                        if reg != dst {
                            self.emit(pack_abc(Opcode::MOVE as u32, dst as u32, reg as u32, 0));
                        }
                    }
                    Some(LocalResolution::OuterProto { reg, ty, backing, mutable, moved }) => {
                        if let Some(cv) = backing {
                            let k = self.add_constant(cv);
                            self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, k as u32));
                        } else {
                            let uv_idx = self.capture_upvalue(
                                name,
                                UpvalueSource::ParentRegister(reg),
                                ty,
                            );
                            self.emit(pack_abc(
                                Opcode::GETUPVAL as u32, dst as u32, uv_idx as u32, 0,
                            ));
                        }
                    }
                    None => type_error(&format!("undefined variable '{}'", name)),
                }
            }

            AstNode::ClassLiteral { ty, fields } => {
                let class_name = match ty.as_ref() {
                    AstNode::Identifier(s) => s.clone(),
                    AstNode::SelfExpr => {
                        ctx.current_class.clone().unwrap_or_else(|| {
                            type_error("Self used outside of class")
                        })
                    }
                    AstNode::Borrowed(b) | AstNode::Moved(b) if **b == AstNode::SelfExpr => {
                        ctx.current_class.clone().unwrap_or_else(|| {
                            type_error("Self used outside of class")
                        })
                    }
                    AstNode::Borrowed(b) | AstNode::Moved(b) => {
                        let unwrapped = *b.clone();

                        if let AstNode::Identifier(s) = unwrapped
                        {
                            s.clone()
                        }
                        else if unwrapped == AstNode::SelfExpr
                        {
                            ctx.current_class.clone().unwrap_or_else(|| {
                                type_error("Self used outside of class")
                            })
                        }
                        else {
                            panic!("Unknown class type")
                        }
                    }
                    other => panic!("Unknown class {:?}", other),
                };
                
                let class_ty_opt = self.scopes.lookup_type(&class_name).cloned();
                let field_vis: Vec<bool> = match &class_ty_opt {
                    Some(Type::Class(id)) => {
                        let class = self.type_arena.get_class(*id);
                        println!("USING CLASS METHODS: {:?}", class.methods);
                        println!("USING CLASS OPERATORS: {:?}", class.operators);

                        class.fields.iter().map(|(n, _, is_pub)| *is_pub).collect()
                    }
                    _ => vec![],
                };

                let proto_k = {
                    let class_id = match &class_ty_opt {
                        Some(Type::Class(id)) => *id,
                        _ => panic!("ClassLiteral: '{}' is not a class type", class_name),
                    };
                    let cv = self.type_arena.get_class(class_id)
                        .class_proto_constant
                        .clone()
                        .unwrap_or_else(|| panic!("ClassProto not built for '{}' — class used before fully defined", class_name));
                    self.add_constant(cv)
                };
                self.emit(pack_abx(Opcode::NEWCLASS as u32, dst as u32, proto_k as u32));

                for (fname, fexpr) in fields {
                    // Type-check each field against the registered class type
                    if let Some(Type::Class(id)) = &class_ty_opt {
                        let class = self.type_arena.get_class(*id);

                        if let Some((_, expected_ty, _)) = class.fields.iter().find(|(n, _, _)| n == fname) {
                            let actual_ty = self.infer_expr_type(fexpr, ctx);
                            self.assert_assignable(expected_ty, &actual_ty,
                                &format!("field '{}' of '{}'", fname, class_name));
                        } else {
                            type_error(&format!(
                                "class '{}' has no field '{}'", class_name, fname
                            ));
                        }
                    }

                    let val_reg = self.reg_alloc.alloc();
                    self.compile_expr(fexpr, val_reg, ctx);

                    let field_index = match &class_ty_opt {
                        Some(Type::Class(id)) => {
                            let class = self.type_arena.get_class(*id);
                            class.field_index_map.get(fname)
                                .unwrap_or_else(|| panic!("Unknown field '{}'", fname))
                        }
                        _ => panic!("Expected class type"),
                    };

                    self.emit(pack_abc(
                        Opcode::SETFIELD as u32,
                        dst as u32,
                        val_reg as u32,
                        *field_index as u32,
                    ));
                    self.reg_alloc.free_to(val_reg);
                }
            }
            
            AstNode::FunctionCall { callee, args } => {
                let is_method_call = matches!(callee.as_ref(), AstNode::DotIndex { .. });
                
                self.compile_expr(callee, dst, ctx);

                let nargs = args.len();
                for arg in args.iter() {
                    let arg_reg = self.reg_alloc.alloc();
                    self.compile_expr(arg, arg_reg, ctx);
                }

                // If this is a method call, GETMETHOD already placed `self` at dst+1,
                // so we must include it in the argument count.
                let effective_nargs = if is_method_call { nargs + 1 } else { nargs };
                
                let callee_ty = self.infer_expr_type(callee, ctx);
                if let Type::Function(ft) = &callee_ty {
                    let param_offset = if is_method_call { 1 } else { 0 }; // skip `self`
                    for (i, (arg, param_ty)) in args.iter()
                        .zip(ft.params.iter().skip(param_offset))
                        .enumerate()
                    {
                        match param_ty {
                            Type::Qualified { moved: true, .. } => {
                                match arg {
                                    AstNode::Identifier(n) => {
                                        type_error(&format!(
                                            "variable '{}' must be explicitly moved (use '->{}')", n, n
                                        ));
                                    }

                                    AstNode::Moved(_) => {}
                                    _ => {}
                                }
                            }
                            Type::Qualified { borrowed: true, .. } => {
                                match arg {
                                    AstNode::Identifier(n) => {
                                        let arg_ty = self.infer_expr_type(arg, ctx);
                                        if !matches!(arg_ty, Type::Qualified { borrowed: true, .. }) {
                                            type_error(&format!(
                                                "variable '{}' must be explicitly borrowed (use '&{}')", n, n
                                            ));
                                        }
                                    }
                                    AstNode::Borrowed(_) => {}
                                    _ => {} // temporary — pass freely
                                }
                            }
                            _ => {}
                        }
                    }
                }

                self.emit(pack_abc(Opcode::CALL as u32, dst as u32, effective_nargs as u32, 1));
                self.reg_alloc.free_to(dst + 1);
            }

            AstNode::DotIndex { indexee, index } => {
                let obj_reg = if self.reg_alloc.current_top <= dst + 1 {
                    self.reg_alloc.current_top = dst + 2;
                    dst + 1
                } else {
                    self.reg_alloc.alloc()
                };
                
                self.compile_expr(indexee, obj_reg, ctx);

                let field_name = match index.as_ref() {
                    AstNode::Identifier(s) => s.clone(),
                    _ => panic!("DotIndex: expected identifier"),
                };

                let obj_ty = self.infer_expr_type(indexee, ctx);
                
                if let Type::Class(id) = self.resolve_type(&obj_ty, "DotIndex")
                {
                    let class = self.type_arena.get_class(*id);

                    let fields = &class.fields;
                    let field_index_map = &class.field_index_map;
                    let methods = &class.methods;
                    let name = &class.name;

                    let is_inside_class = ctx.current_class.as_ref() == Some(&name);

                    if let Some((_, _, is_public)) = fields.iter().find(|(n, _, _)| n == &field_name) {
                        if !*is_public && !is_inside_class {
                            type_error(&format!("field '{}.{}' is private", name, field_name));
                        }

                        let field_index = *field_index_map.get(&field_name).unwrap();

                        self.emit(pack_abc(
                            Opcode::GETFIELD as u32,
                            dst as u32,
                            obj_reg as u32,
                            field_index as u32,
                        ));

                        self.reg_alloc.free_to(obj_reg);
                        return;
                    }
                    if let Some((method_idx, _, fn_ty, is_public)) = methods.get(&field_name) {
                        if !*is_public && !is_inside_class {
                            type_error(&format!("method '{}.{}' is private", name, field_name));
                        }

                        // Check if this is a static function (no `self` param)
                        let has_self = fn_ty.params.first()
                            .map(|p| matches!(p, Type::Class(_)))
                            .unwrap_or(false);

                        if !has_self {
                            type_error(&format!(
                                "'{}::{}' is a static function and cannot be called on an instance; use '{}::{}(...)' instead",
                                name, field_name, name, field_name
                            ));
                        }

                        self.emit(pack_abc(
                            Opcode::GETMETHOD as u32,
                            dst as u32,
                            obj_reg as u32,
                            *method_idx as u32,
                        ));

                        return;
                    }

                    panic!("'{}' has no field or method '{}' !!! {:?} ||| {:?}", name, field_name, methods, fields);
                }
            }

            AstNode::NamespaceIndex { indexee, index } => {
                let method_name = match index.as_ref() {
                    AstNode::Identifier(s) => s.clone(),
                    other => panic!("NamespaceIndex: expected ident, got {:?}", other),
                };
                let ns_name = match indexee.as_ref() {
                    AstNode::Identifier(s) => s.clone(),
                    other => panic!("NamespaceIndex: expected ident, got {:?}", other),
                };
                
                let (reg, is_public) = {
                    let ns = Self::find_namespace_in_scopes(&self.scopes, &ns_name)
                        .unwrap_or_else(|| panic!("No namespace '{}'", ns_name));

                    ns.locals.get(&method_name)
                        .copied()
                        .unwrap_or_else(|| panic!("No '{}' in namespace '{}'", method_name, ns_name))
                };

                let is_inside_class = ctx.current_class.as_ref() == Some(&ns_name);

                if !is_public && !is_inside_class {
                    type_error(&format!(
                        "method '{}::{}' is private",
                        ns_name, method_name
                    ));
                }

                match self.scopes.resolve_local(&format!("{}::{}", ns_name, method_name), self.proto_depth) {
                    Some(LocalResolution::Local { reg: real_reg, .. }) => {
                        if real_reg != dst {
                            self.emit(pack_abc(Opcode::MOVE as u32, dst as u32, real_reg as u32, 0));
                        }
                    }
                    Some(LocalResolution::OuterProto { backing: Some(cv), .. }) => {
                        let k = self.add_constant(cv);
                        self.emit(pack_abx(Opcode::LOADK as u32, dst as u32, k as u32));
                    }
                    other => panic!("Failed to resolve method '{}::{}', got {:?}", ns_name, method_name, other),
                }
            }

            AstNode::BinaryOperation { op, left, right } => {
                let lt = self.infer_expr_type(left, ctx);
                let rt = self.infer_expr_type(right, ctx);

                let l_ty = lt.inner().clone();
                let r_ty = rt.inner().clone();

                let l_reg = self.reg_alloc.alloc();
                let r_reg = self.reg_alloc.alloc();

                self.compile_expr(left, l_reg, ctx);
                self.compile_expr(right, r_reg, ctx);
                
                let overload = match (&l_ty, &r_ty) {
                    (Type::Class(id), _) => {
                        let class = self.type_arena.get_class(*id);
                        class.operators.get(op)
                    }
                    (_, Type::Class(id)) => {
                        let class = self.type_arena.get_class(*id);
                        class.operators.get(op)
                    }
                    _ => None,
                };

                let is_overloaded = overload.is_some();

                let vm_op = match (op, is_overloaded) {
                    (Operator::Add, true) => Opcode::ADDOV,
                    (Operator::Sub, true) => Opcode::SUBOV,
                    (Operator::Mul, true) => Opcode::MULOV,
                    (Operator::Div, true) => Opcode::DIVOV,

                    (Operator::Add, false) => Opcode::ADD,
                    (Operator::Sub, false) => Opcode::SUB,
                    (Operator::Mul, false) => Opcode::MUL,
                    (Operator::Div, false) => Opcode::DIV,
                    (Operator::Mod, false) => Opcode::MOD,
                    (Operator::Pow, false) => Opcode::POW,
                    (Operator::BLShift, false) => Opcode::BLSHIFT,
                    (Operator::BRShift, false) => Opcode::BRSHIFT,
                    (Operator::BAnd, false) => Opcode::BAND,
                    (Operator::BOr, false) => Opcode::BOR,

                    _ => type_error(&format!("Unhandled operator {:?}", op)),
                };

                if let Some((_proto_idx, fn_ty)) = overload {
                    let params = &fn_ty.params;

                    if params.len() != 2 {
                        type_error(&format!(
                            "operator '{:?}' must take exactly 2 parameters",
                            op
                        ));
                    }

                    self.assert_assignable(&params[0], &l_ty,
                        &format!("lhs of operator '{:?}'", op));

                    self.assert_assignable(&params[1], &r_ty,
                        &format!("rhs of operator '{:?}'", op));
                }

                self.emit(pack_abc(
                    vm_op as u32,
                    dst as u32,
                    l_reg as u32,
                    r_reg as u32,
                ));

                self.reg_alloc.free_to(l_reg);
            }

            AstNode::TypeCast { left, right } => {
                let target_ty = self.compile_type(right);
                let src_reg = self.reg_alloc.alloc();
                self.compile_expr(left, src_reg, ctx);
                
                let ty_const = self.add_constant(ConstantValue::Type(target_ty));
                self.emit(pack_abc(
                    Opcode::TYCAST as u32,
                    dst as u32,
                    src_reg as u32,
                    ty_const as u32,
                ));
                self.reg_alloc.free_to(src_reg);
            }

            other => panic!("Unhandled expr node: {:?}", other),
        }
    }

    fn assert_numeric_or_float(&self, ty: &Type, context: &str) {
        if matches!(ty, Type::Unknown) { return; }
        // Float types are not in ty.rs as separate cases so guard via is_numeric + Unknown
        // (F32/F64 would expand here once added to ty.rs)
        if !ty.inner().is_numeric() && !matches!(ty.inner(), Type::Unknown) {
            type_error(&format!("{}: '{}' is not a numeric type", context, ty));
        }
    }
}

impl LucyCompiler {
    fn compile_import_file(&mut self, program: &AstNode, ctx: &CompilingCtx) -> Namespace {
        self.enter_proto("__import__".to_string(), 0);
        self.enter_scope();

        let stmts = match program {
            AstNode::Program(s) => s,
            other => panic!("Expected Program, got {:?}", other),
        };

        for stmt in stmts { self.compile_stmt(stmt, ctx); }

        let scope = self.scopes.get_current_scope();
        let mut ns = Namespace::new();
        for (name, &reg) in &scope.exports {
            ns.locals.insert(name.clone(), (reg, true));
        }
        for (name, _) in &scope.namespaces {
            ns.children.insert(name.clone(), Namespace::new());
        }

        self.exit_scope();
        self.exit_proto();
        ns
    }

    fn resolve_namespace_path<'b>(
        scopes: &'b ScopeStack,
        path:   &AstNode,
    ) -> Option<&'b Namespace> {
        match path {
            AstNode::Identifier(name) => {
                for scope in scopes.scopes.iter().rev() {
                    if let Some(ns) = scope.namespaces.get(name.as_str()) {
                        return Some(ns);
                    }
                }
                None
            }
            AstNode::NamespaceIndex { indexee, index } => {
                let parent = Self::resolve_namespace_path(scopes, indexee)?;
                let child  = match index.as_ref() {
                    AstNode::Identifier(s) => s.as_str(),
                    other => panic!("Expected ident in namespace path, got {:?}", other),
                };
                parent.children.get(child)
            }
            other => panic!("Unexpected node in use path: {:?}", other),
        }
    }

    pub fn find_namespace_in_scopes<'a>(
        scopes: &'a ScopeStack,
        name:   &str,
    ) -> Option<&'a Namespace> {
        for scope in scopes.scopes.iter().rev() {
            if let Some(ns) = scope.namespaces.get(name) {
                return Some(ns);
            }
        }
        None
    }
}

use std::fmt;
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::U8    => write!(f, "u8"),
            Type::I8    => write!(f, "i8"),
            Type::U16   => write!(f, "u16"),
            Type::I16   => write!(f, "i16"),
            Type::U32   => write!(f, "u32"),
            Type::I32   => write!(f, "i32"),
            Type::U64   => write!(f, "u64"),
            Type::I64   => write!(f, "i64"),
            Type::F32   => write!(f, "f32"),
            Type::F64   => write!(f, "f64"),
            Type::USize => write!(f, "usize"),
            Type::Bool  => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Empty => write!(f, "empty"),
            Type::Unknown => write!(f, "<inferred>"),
            Type::Array(inner) => write!(f, "[{}]", inner),
            Type::TypeVar(n)   => write!(f, "{}", n),
            Type::Class(id) => write!(f, "{:?}", id),
            Type::Function(ft) => {
                write!(f, "fn(")?;
                for (i, p) in ft.params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ft.return_type)
            }
            Type::Qualified { inner, mutable, borrowed, moved } => {
                if *mutable  { write!(f, "mut ")?; }
                if *borrowed { write!(f, "&")?; }
                if *moved    { write!(f, "move ")?; }
                write!(f, "{}", inner)
            }
            Type::Generic { name, args } => {
                write!(f, "{}<", name)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", a)?;
                }
                write!(f, ">")
            }
        }
    }
}