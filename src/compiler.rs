// compiler.rs

use std::collections::HashMap;
use crate::v_type::VType;
use crate::vm::{NativeFunction, RuntimeValue};
use crate::ast::{AstNode, Parameter, Parser, FmtPart};
use crate::lexer::tokenize;

#[derive(Debug, Clone)]
pub struct RegAlloc {
    next: u32,
}

impl RegAlloc {
    pub fn new() -> Self { Self { next: 0 } }
    pub fn alloc(&mut self) -> u32 {
        let r = self.next;
        self.next += 1;
        r
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub reg: u32,
    pub ty: VType
}

#[derive(Debug, Clone)]
pub struct PrototypeFunction {
    pub name: String,
    pub params: Vec<Parameter>,
    pub code: Vec<Instruction>,
    pub ctx: CompileCtx,
    pub return_type: VType,
    pub generics: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    pub symbols: HashMap<String, Symbol>,
    pub const_symbols: HashMap<String, u32>,
    pub fn_map: HashMap<String, usize>,
    pub native_map: HashMap<String, NativeEntry>,
}

#[derive(Debug, Clone)]
pub struct NativeEntry {
    pub global_idx: usize,
    pub params: Vec<VType>,
    pub return_type: VType,
    pub generic_names: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct StructPrototype {
    pub name: String,
    pub fields: Vec<(String, VType)>, // actual data
    pub methods: HashMap<String, usize>, // method index
    pub struct_type: VType,
    pub generics: Vec<String>
}

macro_rules! find_const_arm {
    ($consts:expr, $variant:path, $val:expr) => {{
        let v = $consts.iter().position(|x| matches!(x, $variant(e) if *e == $val));
        v.map(|n| n as u32)
    }};
}

fn substitute_type(ty: &VType, map: &HashMap<String, VType>) -> VType {
    match ty {
        VType::Generic(name) => {
            map.get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone())
        }
        VType::Unresolved(name) => {
            map.get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone())
        }
        VType::Struct(name, gens) => {
            VType::Struct(
                name.clone(),
                gens.iter().map(|g| substitute_type(g, map)).collect()
            )
        }
        VType::Array(inner) => {
            VType::Array(Box::new(substitute_type(inner, map)))
        }
        other => other.clone()
    }
}

fn instantiate_struct(
    ctx: &mut CompileCtx,
    base_name: &str,
    generic_args: Vec<VType>
) -> usize {
    let key = format!(
        "{}<{}>",
        base_name,
        generic_args.iter()
            .map(|g| format!("{:?}", g))
            .collect::<Vec<_>>()
            .join(",")
    );

    if let Some(idx) = ctx.struct_instances.get(&key) {
        return *idx;
    }
    let base = ctx.structs.iter()
        .find(|s| s.name == base_name)
        .unwrap_or_else(|| panic!("Unknown struct '{}'", base_name))
        .clone();
    if base.generics.is_empty() {
        return ctx.structs.iter()
            .position(|s| s.name == base_name)
            .unwrap();
    }
    if base.generics.len() != generic_args.len() {
        panic!("Generic arity mismatch for '{}'", base_name);
    }
    if base.generics.len() > 0 && generic_args.is_empty() {
        panic!("Generic struct '{}' requires type arguments", base_name);
    }

    let subst: HashMap<_, _> = base.generics.iter()
        .cloned()
        .zip(generic_args.iter().cloned())
        .collect();

    let fields = base.fields.iter()
        .map(|(name, ty)| (name.clone(), substitute_type(ty, &subst)))
        .collect::<Vec<_>>();

    let mut methods = HashMap::new();
    for (name, fn_idx) in &base.methods {
        let base_fn = ctx.protos[*fn_idx].clone();

        let subst: HashMap<_, _> = base.generics.iter()
            .cloned()
            .zip(generic_args.iter().cloned())
            .collect();

        let new_params = base_fn.params.iter().map(|p| {
            Parameter {
                ident: p.ident.clone(),
                v_type: substitute_type(&p.v_type, &subst),
            }
        }).collect::<Vec<_>>();

        let new_return = substitute_type(&base_fn.return_type, &subst);
        
        let new_fn = PrototypeFunction {
            name: format!("{}::{}", key, base_fn.name),
            params: new_params,
            code: base_fn.code.clone(),
            ctx: base_fn.ctx.clone(),
            return_type: new_return,
            generics: vec![],
        };

        let new_idx = ctx.protos.len() - 1;
        ctx.protos.push(new_fn);

        methods.insert(name.clone(), new_idx);
    }
    let new_proto = StructPrototype {
        name: key.clone(),
        fields,
        struct_type: VType::Struct(key.clone(), vec![]),
        generics: vec![],
        methods,
    };
    
    let new_idx = ctx.structs.len() - 1;
    ctx.structs.push(new_proto);
    ctx.struct_instances.insert(key, new_idx);

    new_idx
}

pub type NativeModuleRegistry = HashMap<String, NativeNamespace>;

pub struct NativeNamespace {
    pub name: String,
    pub functions: Vec<NativeFunction>,
}

impl NativeNamespace {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), functions: vec![] }
    }
    
    pub fn register(mut self, name: &str, generics: Vec<&str>, params: Vec<VType>, return_type: VType, func: fn(RuntimeValue, &[RuntimeValue], &[VType]) -> RuntimeValue) -> Self {
        self.functions.push(NativeFunction {
            name: name.to_string(),
            func,
            params,
            return_type,
            generic_names: generics.into_iter().map(|s| s.to_string()).collect(),
        });
        self
    }
}

#[derive(Debug, Clone)]
pub struct CompileCtx {
    pub consts: Vec<ConstValue>,
    pub symbols: Vec<Symbol>,
    pub protos: Vec<PrototypeFunction>,
    pub structs: Vec<StructPrototype>,
    pub regs: RegAlloc,
    pub native_map: HashMap<String, usize>,
    pub struct_instances: HashMap<String, usize>,
    pub init_code: Vec<Instruction>,
    pub const_symbols: HashMap<String, u32>, // name -> const pool index
    pub namespaces: HashMap<String, Namespace>,
}

impl CompileCtx {
    pub fn new() -> Self {
        Self {
            consts: vec![],
            symbols: vec![],
            protos: vec![],
            structs: vec![],
            regs: RegAlloc::new(),
            native_map: HashMap::new(),
            struct_instances: HashMap::new(),
            init_code: vec![],
            const_symbols: HashMap::new(),
            namespaces: HashMap::new(),
        }
    }

    pub fn with_parent(parent: &CompileCtx) -> Self {
        Self {
            consts: (*parent.consts).to_vec(),
            symbols: (*parent.symbols).to_vec(),
            protos: (*parent.protos).to_vec(),
            structs: (*parent.structs).to_vec(),
            regs: RegAlloc::new(),
            native_map: parent.native_map.clone(),
            struct_instances: HashMap::new(),
            init_code: vec![],
            const_symbols: parent.const_symbols.clone(),
            namespaces: parent.namespaces.clone(),
        }
    }

    pub fn register_native_namespaces(
        &mut self,
        all_natives: &mut Vec<NativeFunction>,
        namespaces: Vec<NativeNamespace>,
    ) {
        for ns in namespaces {
            let mut ns_map = Namespace {
                symbols: HashMap::new(),
                const_symbols: HashMap::new(),
                fn_map: HashMap::new(),
                native_map: HashMap::new(),
            };

            for nf in ns.functions {
                let clone = nf.clone();
                let idx = all_natives.len(); // position in the VM's vec = the index
                ns_map.native_map.insert(nf.name.clone(), NativeEntry {
                    global_idx: idx,
                    params: nf.params.clone(),
                    return_type: nf.return_type.clone(),
                    generic_names: nf.generic_names.clone(),
                });
                all_natives.push(clone);
            }

            self.namespaces.insert(ns.name, ns_map);
        }
    }

    #[inline]
    pub fn add_const(&mut self, val: ConstValue) -> u32 {
        self.consts.push(val);
        (self.consts.len() - 1) as u32
    }

    pub fn find_const(&mut self, val: ConstValue) -> Option<u32> {
        match val {
           ConstValue::Array(ref elem) => {
                let v = self.consts.iter()
                    .position(|x| matches!(x, ConstValue::Array(e) if e == elem));
                v.map(|n| n as u32)
            }
            ConstValue::Function(idx) => find_const_arm!(self.consts, ConstValue::Function, idx),
            ConstValue::NativeFunction(idx) => find_const_arm!(self.consts, ConstValue::NativeFunction, idx),

            ConstValue::U8(i) => find_const_arm!(self.consts, ConstValue::U8, i),
            ConstValue::I8(i) => find_const_arm!(self.consts, ConstValue::I8, i),
            ConstValue::U16(i) => find_const_arm!(self.consts, ConstValue::U16, i),
            ConstValue::I16(i) => find_const_arm!(self.consts, ConstValue::I16, i),
            ConstValue::U32(i) => find_const_arm!(self.consts, ConstValue::U32, i),
            ConstValue::I32(i) => find_const_arm!(self.consts, ConstValue::I32, i),
            ConstValue::U64(i) => find_const_arm!(self.consts, ConstValue::U64, i),
            ConstValue::I64(i) => find_const_arm!(self.consts, ConstValue::I64, i),
            ConstValue::F32(i) => find_const_arm!(self.consts, ConstValue::F32, i),
            ConstValue::F64(i) => find_const_arm!(self.consts, ConstValue::F64, i),
            
            ConstValue::USize(i) => find_const_arm!(self.consts, ConstValue::USize, i),
            ConstValue::Str(ref i) => find_const_arm!(self.consts, ConstValue::Str, *i),
            ConstValue::Struct(i) => find_const_arm!(self.consts, ConstValue::Struct, i),
            ConstValue::StructInst(i, ty) => {
                 let v = self.consts.iter().position(|x| matches!(x, ConstValue::StructInst(e, v) if *e == i && *v == ty));
                v.map(|n| n as u32)
            }
            ConstValue::Type(ref i) => find_const_arm!(self.consts, ConstValue::Type, *i),
            ConstValue::Bool(i) => find_const_arm!(self.consts, ConstValue::Bool, i),
            ConstValue::Empty => {
                self.consts.iter().position(|x| matches!(x, ConstValue::Empty)).map(|n| n as u32)
            }
            other => panic!("unhandled find_const val: '{:?}'", other),
        }
    }

    #[inline]
    pub fn find_or_add_const(&mut self, val: ConstValue) -> u32 {
        self.find_const(val.clone())
            .or_else(||{Some(self.add_const(val))})
            .expect("not found")
    }

    #[inline]
    pub fn add_symbol(&mut self, name: String, reg: u32, ty: VType) {
        self.symbols.push(Symbol { name, reg, ty });
    }

    #[inline]
    pub fn find_fn_addr_by_name(&mut self, name: &str) -> Option<usize> {
        self.protos
            .iter()
            .position(|p| p.name == name)
    }

    #[inline]
    pub fn find_symbol(&mut self, name: &str) -> Option<&Symbol> {
        self.symbols.iter()
            .find(|s| s.name == name)
    }

    #[inline]
    pub fn register_native_fns(&mut self, fns: &Vec<NativeFunction>) {
        for (idx, nf) in fns.iter().enumerate() {
            self.native_map.insert(nf.name.clone(), idx);
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    U8(u8),
    I8(i8),

    U16(u16),
    I16(i16),

    U32(u32),
    I32(i32),
    
    U64(u64),
    I64(i64),

    F32(f32),
    F64(f64),
    USize(usize),
    Str(String),
    Bool(bool),
    Empty,

    Function(usize),
    NativeFunction(usize),
    StructInst(usize, Vec<VType>), // (struct_idx, concrete_type)
    Struct(usize),
    Array(Box<VType>),
    Type(VType),
}

pub type Instruction = u32;

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    LoadConst,
    Move,

    Call,
    Ret,
    BeginBlock,
    EndBlock,
    Noop,

    NewStruct,
    StoreStructField,
    LoadStructField,

    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Cast,
    
    NewArray,
    GetArrayIdx,
    SetArrayIdx,
    ArrayLen,

    Jmp, // Will only jump to the ip if the value in register a is true
    Jn,

    Eq,
    Le,
    Lt,

    LNot,
    LAnd,
    LOr,

    BNot,
    BAnd,
    BOr,
    BXOr,
    BLShift,
    BRShift
}

impl TryFrom<u32> for Opcode {
    type Error = String;
    
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            x if x == Opcode::ArrayLen as u32 => Ok(Opcode::ArrayLen),
            x if x == Opcode::GetArrayIdx as u32 => Ok(Opcode::GetArrayIdx),
            x if x == Opcode::SetArrayIdx as u32 => Ok(Opcode::SetArrayIdx),
            x if x == Opcode::LoadConst as u32 => Ok(Opcode::LoadConst),
            x if x == Opcode::Move as u32 => Ok(Opcode::Move),
            x if x == Opcode::Call as u32 => Ok(Opcode::Call),
            x if x == Opcode::Ret as u32 => Ok(Opcode::Ret),
            x if x == Opcode::Add as u32 => Ok(Opcode::Add),
            x if x == Opcode::Sub as u32 => Ok(Opcode::Sub),
            x if x == Opcode::Mul as u32 => Ok(Opcode::Mul),
            x if x == Opcode::Div as u32 => Ok(Opcode::Div),
            x if x == Opcode::Pow as u32 => Ok(Opcode::Pow),
            x if x == Opcode::BLShift as u32 => Ok(Opcode::BLShift),
            x if x == Opcode::BRShift as u32 => Ok(Opcode::BRShift),
            x if x == Opcode::BXOr as u32 => Ok(Opcode::BXOr),
            x if x == Opcode::BOr as u32 => Ok(Opcode::BOr),
            x if x == Opcode::BAnd as u32 => Ok(Opcode::BAnd),
            x if x == Opcode::Eq as u32 => Ok(Opcode::Eq),
            x if x == Opcode::Lt as u32 => Ok(Opcode::Lt),
            x if x == Opcode::Le as u32 => Ok(Opcode::Le),
            x if x == Opcode::LNot as u32 => Ok(Opcode::LNot),
            x if x == Opcode::LAnd as u32 => Ok(Opcode::LAnd),
            x if x == Opcode::LOr as u32 => Ok(Opcode::LOr),
            x if x == Opcode::Jmp as u32 => Ok(Opcode::Jmp),
            x if x == Opcode::Jn as u32 => Ok(Opcode::Jn),
            x if x == Opcode::BeginBlock as u32 => Ok(Opcode::BeginBlock),
            x if x == Opcode::EndBlock as u32 => Ok(Opcode::EndBlock),
            x if x == Opcode::NewStruct as u32 => Ok(Opcode::NewStruct),
            x if x == Opcode::StoreStructField as u32 => Ok(Opcode::StoreStructField),
            x if x == Opcode::LoadStructField as u32 => Ok(Opcode::LoadStructField),
            x if x == Opcode::Noop as u32 => Ok(Opcode::Noop),
            _ => Err(format!("Unknown opcode: {}", value)),
        }
    }
}

pub const OPCODE_BITS: u32 = 6;

pub const A_BITS: u32 = 8;
pub const B_BITS: u32 = 9;
pub const C_BITS: u32 = 9;

pub const A_SHIFT: u32 = OPCODE_BITS;
pub const B_SHIFT: u32 = A_SHIFT + A_BITS;
pub const C_SHIFT: u32 = B_SHIFT + B_BITS;

#[inline]
pub fn pack_i_abx(op: Opcode, a: u32, bx: u32) -> Instruction {
    (op as u32)
    | (a << A_SHIFT)
    | (bx << B_SHIFT)
}

#[inline]
pub fn pack_i_abc(op: Opcode, a: u32, b: u32, c: u32) -> Instruction {
    (op as u32) 
    | (a << A_SHIFT) 
    | (b << B_SHIFT) 
    | (c << C_SHIFT)
}

pub fn coerce_type(from: &VType, to: &VType, ctx: &mut CompileCtx, reg: u32, code: &mut Vec<Instruction>) {
    match (from, to) {
        (a, VType::Auto) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        },

        (VType::Generic(..), b) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        },
        
        // Tostring, emit a Cast instruction
        (VType::U8  | VType::I8  |
         VType::U16 | VType::I16 |
         VType::U32 | VType::I32 |
         VType::F32 | VType::F64 |
         VType::USize,
         VType::String) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            // Cast A Bx = R[A] = R[A] cast-to const[Bx]
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        }

        // Numeric widening / narrowing — emit a Cast instruction
        (VType::U8  | VType::I8  |
         VType::U16 | VType::I16 |
         VType::U32 | VType::I32 |
         VType::F32 | VType::F64 |
         VType::USize,
         VType::U8  | VType::I8  |
         VType::U16 | VType::I16 |
         VType::U32 | VType::I32 |
         VType::U64 | VType::I64 |
         VType::F32 | VType::F64 |
         VType::USize) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            // Cast A Bx = R[A] = R[A] cast-to const[Bx]
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        }

        (VType::String, VType::String) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            // Cast A Bx = R[A] = R[A] cast-to const[Bx]
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        }

        (VType::Array(from_elem), VType::Array(to_elem)) => {
            if from_elem != to_elem {
                let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
                code.push(pack_i_abx(Opcode::Cast, reg, type_const));
            }
        }

        (VType::Struct(name, generics), VType::Struct(name2, generics2)) => {
            if name != name2 || generics != generics2 {
                let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
                code.push(pack_i_abx(Opcode::Cast, reg, type_const));
            }
        }

        // Incompatible types
        (f, t) => panic!(
            "Cannot coerce {:?} into {:?}: types are not compatible",
            f, t
        ),
    }
}

pub fn type_of(ctx: &mut CompileCtx, node: &AstNode) -> VType
{
    match node {
        AstNode::TypeCast { value, ty } => {
            ty.clone()
        }
        AstNode::Index { target, .. } => {
            match type_of(ctx, &*target) {
                VType::Array(elem_type) => *elem_type,
                other => panic!("Cannot index into non-array type: {:?}", other),
            }
        }
        AstNode::FmtString { .. } => VType::String,
        AstNode::String(..) => VType::String,
        AstNode::Integer(..) => VType::I32,
        AstNode::Double(..) => VType::F64,
        AstNode::Call {callee, args, generics} => {
            match callee.as_ref() {
                AstNode::Call { callee, .. } => {
                    let inner_ty = type_of(ctx, callee);

                    match inner_ty {
                        VType::Function(generics, params, return_type) => return VType::Function(generics, params, return_type),
                        
                        other => panic!("Trying to call non-function type: {:?}", other),
                    }
                }
                AstNode::Identifier(name) => {
                    // Check protos first
                    if let Some(proto) = ctx.protos.iter().find(|p| p.name == name.as_str()) {
                        if proto.generics.is_empty() || generics.is_empty() {
                            return proto.return_type.clone();
                        }
                        let subst: HashMap<String, VType> = proto.generics.iter()
                            .cloned()
                            .zip(generics.iter().cloned())
                            .collect();
                        return substitute_type(&proto.return_type, &subst);
                    }

                    for ns in ctx.namespaces.values() {
                        if let Some(entry) = ns.native_map.get(name.as_str()) {
                            if generics.is_empty() {
                                return entry.return_type.clone();
                            }
                            let subst: HashMap<String, VType> = entry.generic_names.iter()
                                .cloned()
                                .zip(generics.iter().cloned())
                                .collect();
                            return substitute_type(&entry.return_type, &subst);
                        }
                    }

                    // Fall back to Auto for natives registered via use (only idx known)
                    if ctx.native_map.contains_key(name.as_str()) {
                        return VType::Auto;
                    }

                    panic!("unknown symbol '{}'", name);
                }
                AstNode::BinaryOp {op, left, right} if op == "::" => {
                    fn flatten(node: &AstNode) -> (String, String) {
                        match node {
                            AstNode::BinaryOp { op, left, right } if op == "::" => {
                                let (ns, rest) = flatten(left);
                                let rhs = match right.as_ref() {
                                    AstNode::Identifier(s) => s.clone(),
                                    _ => panic!("Invalid namespace"),
                                };
                                (ns, if rest.is_empty() { rhs } else { format!("{}::{}", rest, rhs) })
                            }
                            AstNode::Identifier(ns) => (ns.clone(), "".to_string()),
                            _ => panic!("Invalid namespace"),
                        }
                    }

                    let (ns_name, item) = match (left.as_ref(), right.as_ref()) {
                        (AstNode::Identifier(ns), AstNode::Identifier(item)) => (ns.clone(), item.clone()),
                        (l, r) => {
                            let (ns, rest) = flatten(l);
                            let item = match r {
                                AstNode::Identifier(s) => s.clone(),
                                _ => panic!("Invalid namespace RHS"),
                            };
                            (ns, if rest.is_empty() { item } else { format!("{}::{}", rest, item) })
                        }
                    };

                    let ns = ctx.namespaces.get(&ns_name)
                        .unwrap_or_else(|| panic!("Unknown namespace '{}'", ns_name));

                    // NORMAL FUNCTIONS
                    if let Some(&idx) = ns.fn_map.get(&item) {
                        return ctx.protos[idx].return_type.clone();
                    }

                    // NATIVE FUNCTIONS
                    if let Some(entry) = ns.native_map.get(&item) {
                        return entry.return_type.clone();
                    }

                    panic!("'{}::{}' is not callable", ns_name, item);
                }
                AstNode::BinaryOp { op, left, right } if op == "." => {
                    let left_ty = type_of(ctx, left);

                    match left_ty {
                        VType::Struct(name, generics) => {
                            // resolve concrete struct if generic
                            let struct_name = if !generics.is_empty() {
                                let idx = instantiate_struct(ctx, &name, generics.clone());
                                ctx.structs[idx].name.clone()
                            } else {
                                name.clone()
                            };

                            let proto = ctx.structs.iter()
                                .find(|s| s.name == struct_name)
                                .unwrap_or_else(|| panic!("Unknown struct '{}'", struct_name));

                            let method_name = match right.as_ref() {
                                AstNode::Identifier(s) => s,
                                _ => panic!("Invalid method call RHS: {:?}", right),
                            };

                            if let Some(method_idx) = proto.methods.get(method_name) {
                                let proto_fn = &ctx.protos[*method_idx];
                                
                                if proto_fn.generics.is_empty() || generics.is_empty() {
                                    return proto_fn.return_type.clone();
                                }

                                let subst: HashMap<String, VType> = proto_fn.generics.iter()
                                    .cloned()
                                    .zip(generics.iter().cloned())
                                    .collect();

                                return substitute_type(&proto_fn.return_type, &subst);
                            }

                            if proto.fields.iter().any(|(f, _)| f == method_name) {
                                panic!("Field '{}' is not callable", method_name);
                            }

                            panic!("Method '{}' not found in struct '{}'", method_name, struct_name);
                        }

                        other => panic!("Cannot call method on non-struct type: {:?}", other),
                    }
                }
                other => panic!("Identifier empty?")
            }
        }
        AstNode::StructLiteral { name, generics, .. } => {
            if !generics.is_empty() {
                VType::Struct(name.clone(), generics.clone())
            } else {
                VType::Struct(name.clone(), vec![])
            }
        }
        AstNode::ArrayLiteral {exprs} => {
            let first_type = type_of(ctx, &*exprs[0].clone());
            let len = exprs.len();
            if len > 1
            {
                for i in 1..len
                {
                    let this_type = type_of(ctx, &*exprs[i].clone());
                    if this_type != first_type {
                        panic!("Array types were not all the same")
                    }
                }
            }
            VType::Array(Box::new(first_type))
        }
        AstNode::Identifier(name) => {
            if let Some(&cidx) = ctx.const_symbols.get(name.as_str()) {
                return match &ctx.consts[cidx as usize] {
                    ConstValue::U8(_)    => VType::U8,
                    ConstValue::I8(_)    => VType::I8,
                    ConstValue::U16(_)   => VType::U16,
                    ConstValue::I16(_)   => VType::I16,
                    ConstValue::U32(_)   => VType::U32,
                    ConstValue::I32(_)   => VType::I32,
                    ConstValue::F32(_)   => VType::F32,
                    ConstValue::F64(_)   => VType::F64,
                    ConstValue::USize(_) => VType::USize,
                    ConstValue::Str(_)   => VType::String,
                    ConstValue::Bool(_)  => VType::Bool,
                    other => panic!("type_of: unhandled const_symbol type {:?}", other),
                };
            }
            
            let sym = ctx.find_symbol(name.as_str())
                .unwrap_or_else(|| panic!("unknown symbol '{}'", name));
            sym.ty.clone()
        }
        AstNode::BinaryOp {op, left, right} => {
            match op.as_str() {
                "+" => type_of(ctx, left),
                "::" => {
                    let ns_name = match left.as_ref() {
                        AstNode::Identifier(n) => n,
                        _ => panic!("Invalid namespace LHS: {:?}", left),
                    };

                    let ns = ctx.namespaces.get(ns_name)
                        .unwrap_or_else(|| panic!("Unknown namespace '{}'", ns_name));

                    match right.as_ref() {
                        AstNode::Identifier(item) => {
                            if let Some(&cidx) = ns.const_symbols.get(item) {
                                return match &ctx.consts[cidx as usize] {
                                    ConstValue::U8(_)    => VType::U8,
                                    ConstValue::I8(_)    => VType::I8,
                                    ConstValue::U16(_)   => VType::U16,
                                    ConstValue::I16(_)   => VType::I16,
                                    ConstValue::U32(_)   => VType::U32,
                                    ConstValue::I32(_)   => VType::I32,
                                    ConstValue::F32(_)   => VType::F32,
                                    ConstValue::F64(_)   => VType::F64,
                                    ConstValue::USize(_) => VType::USize,
                                    ConstValue::Str(_)   => VType::String,
                                    ConstValue::Bool(_)  => VType::Bool,
                                    other => panic!("type_of(ns): unhandled const {:?}", other),
                                };
                            }

                            if let Some(sym) = ns.symbols.get(item) {
                                return sym.ty.clone();
                            }

                            if let Some(idx) = ns.fn_map.get(item) {
                                return ctx.protos[*idx].return_type.clone();
                            }
                            
                            if let Some(entry) = ns.native_map.get(item) {
                                return entry.return_type.clone();
                            }
                            panic!("'{}::{}' not found", ns_name, item);
                        }

                        _ => panic!("Invalid namespace RHS: {:?}", right),
                    }
                }
                "." => {
                    let left_ty = type_of(ctx, &*left);

                    match left_ty {
                        VType::Struct(name, generics) => {
                            // If it's a generic instance, resolve concrete struct
                            let struct_name = if !generics.is_empty() {
                                let idx = instantiate_struct(ctx, &name, generics.clone());
                                ctx.structs[idx].name.clone()
                            } else {
                                name.clone()
                            };

                            let proto = ctx.structs.iter()
                                .find(|s| s.name == struct_name)
                                .unwrap_or_else(|| panic!("Unknown struct '{}'", struct_name));

                            let field_name = match right.as_ref() {
                                AstNode::FieldKey(f) | AstNode::Identifier(f) => f, // borrow
                                _ => panic!("Invalid field access RHS: {:?}", right),
                            };

                            let (_, field_ty) = proto.fields.iter()
                                .find(|(fname, _)| fname == field_name)
                                .unwrap_or_else(|| {
                                    panic!("Field '{}' not found in struct '{}'", field_name, struct_name)
                                });

                            field_ty.clone()
                        }

                        other => panic!("Cannot access field on non-struct type: {:?}", other),
                    }
                }
                _ => panic!("Unhandled op: {}", op)
            }
        }
        other => panic!("Unhandled ast::type_of case: {:#?}", other)
    }
}

pub fn compile_expr(ast: AstNode, ctx: &mut CompileCtx, reg: u32) -> Vec<Instruction> {
    let mut code = vec![];
    
    match ast {
        AstNode::TypeCast { value, ty } => {
            // Borrow for type checking
            let from_ty = type_of(ctx, &*value);

            // Compile the inner expression into the target register
            code.extend(compile_expr(*value, ctx, reg));

            // Emit a cast if needed
            coerce_type(&from_ty, &ty, ctx, reg, &mut code);
        }
        AstNode::Integer(n) => {
            let cidx = ctx.find_or_add_const(ConstValue::I32(n));
            code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
        }
        AstNode::Double(n) => {
            let cidx = ctx.find_or_add_const(ConstValue::F64(n));
            code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
        }
        AstNode::String(s) => {
            let cidx = ctx.find_or_add_const(ConstValue::Str(s));
            code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
        }
        AstNode::Identifier(name) => {
            let st = name.as_str();
            if st == "true" {
                let cidx = ctx.find_or_add_const(ConstValue::Bool(true));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if st == "false" {
                let cidx = ctx.find_or_add_const(ConstValue::Bool(false));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if st == "empty" {
                let cidx = ctx.find_or_add_const(ConstValue::Empty);
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if let Some(idx) = ctx.find_fn_addr_by_name(name.as_str()) {
                let cidx = ctx.find_or_add_const(ConstValue::Function(idx));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if let Some(native_idx) = ctx.native_map.get(&name) {
                let cidx = ctx.find_or_add_const(ConstValue::NativeFunction(*native_idx));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else if let Some(&cidx) = ctx.const_symbols.get(&name) {
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
            }
            else
            {
                let sym = ctx.find_symbol(&name)
                    .unwrap_or_else(|| panic!("Unknown identifier `{}`", name));
                if sym.reg != reg {
                    code.push(pack_i_abx(Opcode::Move, reg, sym.reg));
                }
            }
        }
        AstNode::Index { target, index } => {
            let arr_reg = ctx.regs.alloc();
            let idx_reg = ctx.regs.alloc();

            // Compile the array expression into arr_reg
            code.extend(compile_expr(*target, ctx, arr_reg));
            // Compile the index expression into idx_reg
            code.extend(compile_expr(*index, ctx, idx_reg));

            // R[reg] = arr_reg[idx_reg]
            code.push(pack_i_abc(Opcode::GetArrayIdx, reg, arr_reg, idx_reg));
        }
        AstNode::BinaryOp { op, left, right } => {
            let (lreg, rreg) = (ctx.regs.alloc(), ctx.regs.alloc());

            let mut negate_flag = false;
            let mut swap_operands = false;
            let oper_code = match op.as_str() {
                "+" => Opcode::Add,
                "-" => Opcode::Sub,
                "*" => Opcode::Mul,
                "/" => Opcode::Div,
                "==" => Opcode::Eq,
                "!=" => {
                    negate_flag = true;
                    Opcode::Eq
                }
                "<" => Opcode::Lt,
                ">" => {
                    swap_operands = true;
                    Opcode::Lt
                }
                "<=" => Opcode::Le,
                ">=" => {
                    swap_operands = true;
                    Opcode::Le
                }
                "<<" => Opcode::BLShift,
                ">>" => Opcode::BRShift,
                "|" => Opcode::BOr,
                "&" => Opcode::BAnd,
                "||" => Opcode::LOr,
                "&&" => Opcode::LAnd,
                
                "::" => {
                    fn flatten(node: AstNode) -> (String, String) {
                        match node {
                            AstNode::BinaryOp { op, left, right } if op == "::" => {
                                let (ns, rest) = flatten(*left);
                                let rhs = match *right {
                                    AstNode::Identifier(s) => s,
                                    _ => panic!("Invalid namespace"),
                                };
                                (ns, format!("{}::{}", rest, rhs))
                            }
                            AstNode::Identifier(ns) => (ns, "".to_string()),
                            _ => panic!("Invalid namespace"),
                        }
                    }

                    let (ns_name, item) = match (*left, *right) {
                        (AstNode::Identifier(ns), AstNode::Identifier(item)) => (ns, item),
                        (l, r) => {
                            let (ns, rest) = flatten(l);
                            let item = match r {
                                AstNode::Identifier(s) => s,
                                _ => panic!("Invalid namespace RHS"),
                            };
                            (ns, if rest.is_empty() { item } else { format!("{}::{}", rest, item) })
                        }
                    };

                    let ns = ctx.namespaces.get(&ns_name)
                        .unwrap_or_else(|| panic!("Unknown namespace '{}'", ns_name));

                    // FUNCTIONS
                    if let Some(&idx) = ns.fn_map.get(&item) {
                        let cidx = ctx.find_or_add_const(ConstValue::Function(idx));
                        code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
                        return code;
                    }

                    // CONSTS
                    if let Some(&cidx) = ns.const_symbols.get(&item) {
                        code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
                        return code;
                    }

                    // TYPES / STRUCTS
                    if let Some(sym) = ns.symbols.get(&item) {
                        if sym.reg != reg {
                            code.push(pack_i_abx(Opcode::Move, reg, sym.reg));
                        }
                        return code;
                    }

                    // NATIVE FUNCTIONS
                    if let Some(entry) = ns.native_map.get(&item) {
                        let cidx = ctx.find_or_add_const(ConstValue::NativeFunction(entry.global_idx));
                        code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
                        return code;
                    }

                    panic!("'{}' not found in namespace '{}'", item, ns_name);
                }
                "." => {
                    let struct_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*left, ctx, struct_reg));
                    
                    let field_idx = resolve_field_access(&*right, ctx, &struct_reg);
                    let field_idx_reg = ctx.regs.alloc();
                    let field_idx_const = ctx.find_or_add_const(ConstValue::USize(field_idx));
                    code.push(pack_i_abx(Opcode::LoadConst, field_idx_reg, field_idx_const));
                    
                    code.push(pack_i_abc(Opcode::LoadStructField, reg, struct_reg, field_idx as u32));
                    return code;
                }

                other => panic!("unhandled operator: {}", other)
            };
            
            if swap_operands {
                code.extend(compile_expr(*right, ctx, lreg));
                code.extend(compile_expr(*left, ctx, rreg));
            } else {
                code.extend(compile_expr(*left, ctx, lreg));
                code.extend(compile_expr(*right, ctx, rreg));
            }
            code.push(pack_i_abc(oper_code, reg, lreg, rreg));
            if negate_flag {
                code.push(pack_i_abx(Opcode::LNot, reg, reg))
            }
        }
        AstNode::FmtString { parts } => {
            if parts.is_empty() {
                let cidx = ctx.find_or_add_const(ConstValue::Str(String::new()));
                code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));
                return code;
            }

            // Load the first part into reg, then Add each subsequent part into it
            let mut first = true;
            for part in parts {
                let part_reg = if first { reg } else { ctx.regs.alloc() };

                match part {
                    FmtPart::Literal(s) => {
                        let cidx = ctx.find_or_add_const(ConstValue::Str(s));
                        code.push(pack_i_abx(Opcode::LoadConst, part_reg, cidx));
                    }
                    FmtPart::Interpolated(expr) => {
                        let expr_ty = type_of(ctx, &expr);
                        code.extend(compile_expr(*expr, ctx, part_reg));
                        
                        coerce_type(&expr_ty, &VType::String, ctx, part_reg, &mut code);
                    }
                }

                if !first {
                    // reg = reg + part_reg  (string concat)
                    code.push(pack_i_abc(Opcode::Add, reg, reg, part_reg));
                }

                first = false;
            }
        }
        AstNode::Call { callee, args, generics } => {
            // Build param types with generics substituted
            let param_types: Vec<VType> = match callee.as_ref() {
                AstNode::Identifier(name) => {
                    ctx.protos.iter()
                        .find(|p| p.name == name.as_str())
                        .map(|p| {
                            let subst: HashMap<String, VType> = p.generics.iter()
                                .cloned()
                                .zip(generics.iter().cloned())
                                .collect();
                            p.params.iter()
                                .map(|param| substitute_type(&param.v_type, &subst))
                                .collect()
                        })
                        .unwrap_or_default()
                }
                AstNode::BinaryOp { op, left, right } if op == "::" => {
                    let ns_name = match left.as_ref() {
                        AstNode::Identifier(n) => n.as_str(),
                        _ => "",
                    };
                    let fn_name = match right.as_ref() {
                        AstNode::Identifier(n) => n.as_str(),
                        AstNode::Call { callee, .. } => match callee.as_ref() {
                            AstNode::Identifier(n) => n.as_str(),
                            _ => "",
                        },
                        _ => "",
                    };
                    // For native namespace calls, substitute Generic params with call-site generics
                    ctx.namespaces.get(ns_name)
                        .and_then(|ns| ns.native_map.get(fn_name))
                        .map(|entry| {
                            let subst: HashMap<String, VType> = entry.generic_names.iter()
                                .cloned()
                                .zip(generics.iter().cloned())
                                .collect();
                            entry.params.iter()
                                .map(|p| substitute_type(p, &subst))
                                .collect()
                        })
                        .unwrap_or_default()
                }
                _ => vec![],
            };

            // Get the return type with generics substituted, for post-call coercion
            let return_ty: Option<VType> = match callee.as_ref() {
                AstNode::Identifier(name) => {
                    ctx.protos.iter()
                        .find(|p| p.name == name.as_str())
                        .map(|p| {
                            let subst: HashMap<String, VType> = p.generics.iter()
                                .cloned()
                                .zip(generics.iter().cloned())
                                .collect();
                            substitute_type(&p.return_type, &subst)
                        })
                }
                AstNode::BinaryOp { op, left, right } if op == "::" => {
                    let ns_name = match left.as_ref() {
                        AstNode::Identifier(n) => n.as_str(),
                        _ => "",
                    };
                    let fn_name = match right.as_ref() {
                        AstNode::Identifier(n) => n.as_str(),
                        AstNode::Call { callee, .. } => match callee.as_ref() {
                            AstNode::Identifier(n) => n.as_str(),
                            _ => "",
                        },
                        _ => "",
                    };
                    ctx.namespaces.get(ns_name)
                        .and_then(|ns| ns.native_map.get(fn_name))
                        .map(|entry| {
                            let subst: HashMap<String, VType> = entry.generic_names.iter()
                                .cloned()
                                .zip(generics.iter().cloned())
                                .collect();
                            substitute_type(&entry.return_type, &subst)
                        })
                }
                _ => None,
            };
            
            match *callee {
                AstNode::BinaryOp { op, left, right } if op == "." => {
                    let self_reg = ctx.regs.alloc();
                    let self_ty = type_of(ctx, &left);

                    code.extend(compile_expr(*left, ctx, self_reg));

                    let (struct_name, generics) = match self_ty {
                        VType::Struct(n, g) => (n, g),
                        _ => panic!("Method call on non-struct"),
                    };

                    let struct_name = if !generics.is_empty() {
                        let idx = instantiate_struct(ctx, &struct_name, generics.clone());
                        ctx.structs[idx].name.clone()
                    } else {
                        struct_name
                    };

                    let proto = ctx.structs.iter()
                        .find(|s| s.name == struct_name)
                        .unwrap();

                    let method_name = match *right {
                        AstNode::Identifier(s) => s,
                        _ => panic!("Invalid method call"),
                    };

                    let method_idx = proto.methods.get(&method_name)
                        .unwrap_or_else(|| panic!("Method '{}' not found", method_name));

                    let cidx = ctx.find_or_add_const(ConstValue::Function(*method_idx));
                    code.push(pack_i_abx(Opcode::LoadConst, reg, cidx));

                    let arg_base = reg + 1;

                    code.push(pack_i_abx(Opcode::Move, arg_base, self_reg));
                    let args_len = args.len();
                    for (i, arg) in args.into_iter().enumerate() {
                        let arg_reg = arg_base + 1 + i as u32;
                        code.extend(compile_expr(*arg, ctx, arg_reg));
                    }

                    code.push(pack_i_abc(
                        Opcode::Call,
                        reg,
                        (args_len + 1) as u32,
                        generics.len() as u32
                    ));

                    return code;
                }

                _ => {
                    code.extend(compile_expr(*callee, ctx, reg));
                }
            }

            let arg_base = reg + 1;
            let mut arg_regs = vec![];
            let len = args.len();
            for i in 0..len {
                arg_regs.push(arg_base + i as u32);
                ctx.regs.next = ctx.regs.next.max(arg_base + i as u32 + 1);
            }

            for (i, arg) in args.into_iter().enumerate() {
                let arg_reg = arg_regs[i];
                let arg_ty = type_of(ctx, &arg);
                code.extend(compile_expr(*arg, ctx, arg_reg));
                if let Some(param_ty) = param_types.get(i) {
                    coerce_type(&arg_ty, param_ty, ctx, arg_reg, &mut code);
                }
            }

            let type_arg_base = arg_base + len as u32;
            for (i, ty) in generics.iter().enumerate() {
                let type_reg = type_arg_base + i as u32;
                ctx.regs.next = ctx.regs.next.max(type_reg + 1);
                let cidx = ctx.find_or_add_const(ConstValue::Type(ty.clone()));
                code.push(pack_i_abx(Opcode::LoadConst, type_reg, cidx));
            }

            code.push(pack_i_abc(Opcode::Call, reg, len as u32, generics.len() as u32));
            
            if let Some(ret_ty) = return_ty {
                if !matches!(ret_ty, VType::Auto | VType::Empty) {
                    let type_const = ctx.find_or_add_const(ConstValue::Type(ret_ty));
                    code.push(pack_i_abx(Opcode::Cast, reg, type_const));
                }
            }
        }
        AstNode::StructLiteral { name, fields, generics } => {
            let struct_idx = if !generics.is_empty() {
                instantiate_struct(ctx, &name, generics.clone())
            } else {
                ctx.structs.iter()
                    .position(|s| s.name == name)
                    .expect("Struct should exist")
            };
            let struct_const_idx = ctx.find_or_add_const(ConstValue::StructInst(struct_idx, generics));

            let struct_proto = &ctx.structs[struct_idx];
            
            code.push(pack_i_abx(Opcode::NewStruct, reg, struct_const_idx));
            
            let mut field_regs = vec![];
            for _ in 0..fields.len() {
                let a = ctx.regs.alloc();
                field_regs.push(a);
            }
            let mut field_idcs = vec![];

            for (i, (field_name, ..)) in fields.iter().enumerate() {
                let field_idx = struct_proto.fields.iter()
                    .position(|(fname, _)| fname == field_name)
                    .unwrap_or_else(|| panic!("Unknown field '{}' for struct '{}'", field_name, name));
                field_idcs.insert(i, field_idx);
            }

            for (i, (field_name, field_value)) in fields.iter().enumerate() {
                let field_idx = field_idcs[i];
                let field_reg = field_regs[i];
                
                code.extend(compile_expr(field_value.clone(), ctx, field_reg));
                
                code.push(pack_i_abc(Opcode::StoreStructField, reg, field_idx as u32, field_reg));
            }
        }
        AstNode::ArrayLiteral { exprs } => {
            if exprs.is_empty() {
                panic!("Array literal must have at least one element (cannot infer element type from empty literal)");
            }

            let elem_type = type_of(ctx, &exprs[0]);
            let len = exprs.len();

            let mut elem_regs: Vec<u32> = (0..len).map(|_| ctx.regs.alloc()).collect();

            let array_type_const =
                ctx.find_or_add_const(
                    ConstValue::Array(
                        Box::new(
                            elem_type.clone()
                        )
                    )
                );
            code.push(pack_i_abx(Opcode::NewArray, reg, array_type_const));

            for (i, expr) in exprs.into_iter().enumerate() {
                let elem_reg  = elem_regs[i];
                let expr_type = type_of(ctx, &expr);

                code.extend(compile_expr(*expr, ctx, elem_reg));

                let mut coerce_code = vec![];
                coerce_type(&expr_type, &elem_type, ctx, elem_reg, &mut coerce_code);
                code.extend(coerce_code);

                let idx_const = ctx.find_or_add_const(ConstValue::USize(i));
                let idx_reg   = ctx.regs.alloc();
                code.push(pack_i_abx(Opcode::LoadConst, idx_reg, idx_const));
                code.push(pack_i_abc(Opcode::SetArrayIdx, reg, idx_reg, elem_reg));
            }
        }
        other => panic!("unknown expr node: {:?}", other),
    }

    code
}

fn resolve_field_access(node: &AstNode, ctx: &CompileCtx, struct_reg: &u32) -> usize {
    match node {
        AstNode::Identifier(field_name) | AstNode::FieldKey(field_name) => {
            //TODO: Needs proper typechecking
            for struct_proto in &ctx.structs {
                if let Some(field_idx) = struct_proto.fields.iter()
                    .position(|(fname, _)| fname == field_name) {
                    return field_idx;
                }
            }
            panic!("Field '{}' not found in any struct", field_name);
        }
        AstNode::BinaryOp { op, left: _, right } if op == "." => {
            resolve_field_access(right, ctx, struct_reg)
        }
        _ => panic!("Invalid field access pattern: expected Identifier, got {:?}", node)
    }
}

pub fn compile_stmt(ast: AstNode, ctx: &mut CompileCtx, code: &mut Vec<Instruction>) {
    match ast {
        AstNode::Export { item } => {
            compile_stmt(*item, ctx, code);
        }
        AstNode::Import { alias, path } => {}
        AstNode::Use { module_alias, items } => {}

        AstNode::Assignment { assignee, value } => {
            match *assignee {
                AstNode::Identifier(name) => {
                    if let Some(symbol) = ctx.find_symbol(&name) {
                        let target_reg = symbol.reg;

                        let value_reg = ctx.regs.alloc();
                        code.extend(compile_expr(*value, ctx, value_reg));

                        if value_reg != target_reg {
                            code.push(pack_i_abx(Opcode::Move, target_reg, value_reg));
                        }
                    } else {
                        panic!("Undefined variable: {}", name);
                    }
                }
                AstNode::Index { target, index } => {
                    let value_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*value, ctx, value_reg));

                    let arr_reg = ctx.regs.alloc();
                    let idx_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*target, ctx, arr_reg));
                    code.extend(compile_expr(*index, ctx, idx_reg));

                    code.push(pack_i_abc(Opcode::SetArrayIdx, arr_reg, idx_reg, value_reg));
                }
                AstNode::BinaryOp { op, left, right } if op == "." => {
                    let value_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*value, ctx, value_reg));
                    let (struct_reg, field_indices) = compile_field_access_chain(*left, *right, ctx);
                    
                    if field_indices.len() == 1 {
                        // struct.field = value
                        code.push(pack_i_abc(Opcode::StoreStructField, struct_reg, field_indices[0] as u32, value_reg));
                    } else {
                        // struct.field1.field2 = value
                        let mut current_reg = struct_reg;
                        for &field_idx in &field_indices[..field_indices.len()-1] {
                            let next_reg = ctx.regs.alloc();
                            code.push(pack_i_abc(Opcode::LoadStructField, next_reg, current_reg, field_idx as u32));
                            current_reg = next_reg;
                        }
                        
                        // Store to the final field
                        let final_field_idx = field_indices[field_indices.len()-1];
                        code.push(pack_i_abc(Opcode::StoreStructField, current_reg, final_field_idx as u32, value_reg));
                    }
                }
                other =>  panic!("Invalid assignment target '{:?}' - only identifiers and index chains supported", other)
            }
        }
        AstNode::Declaration { name, value, v_type } => {
            let inferred_ty = type_of(ctx, &value);
            
            if let AstNode::Identifier(name_str) = *name {
                let reg = ctx.regs.alloc();

                let final_ty = match v_type {
                    VType::Inferred => inferred_ty.clone(),
                    _ => v_type.clone(),
                };

                ctx.add_symbol(name_str.clone(), reg, final_ty.clone());

                code.extend(compile_expr(*value, ctx, reg));

                coerce_type(&inferred_ty, &final_ty, ctx, reg, code);
            } else {
                panic!("Declaration: expected identifier as binding name, got {:?}", name);
            }
        }

        AstNode::StructDecl { name, fields, struct_type, generics, methods } => {
            let str_type = VType::Struct(name.clone(), vec![]);
            let reg = ctx.regs.alloc();

            let mut actual_fields = Vec::new();
            let mut method_map = HashMap::new();

            for p in fields {
                actual_fields.push((p.ident, p.v_type));
            }

            for m in methods {
                let (name, node) = m;
                if let AstNode::Function { name: fn_name, params, body, return_type, generics: fn_generics } = node {
                    let mut new_params = vec![
                        Parameter {
                            ident: "self".to_string(),
                            v_type: str_type.clone(),
                        }
                    ];

                    new_params.extend(params);

                    let mut sub_ctx = CompileCtx::with_parent(ctx);
                    for param in &new_params {
                        let r = sub_ctx.regs.alloc();
                        sub_ctx.add_symbol(param.ident.clone(), r, param.v_type.clone());
                    }

                    let mut code = vec![];
                    for stmt in body {
                        compile_stmt(*stmt, &mut sub_ctx, &mut code);
                    }

                    let proto = PrototypeFunction {
                        name: format!("{}::{}", name, fn_name),
                        params: new_params,
                        code,
                        ctx: sub_ctx,
                        return_type,
                        generics: fn_generics,
                    };

                    let fn_idx = ctx.protos.len();
                    ctx.protos.push(proto);

                    method_map.insert(fn_name, fn_idx);
                } else {
                    panic!("Struct method is not a function");
                }
            }

            let struct_idx = ctx.structs.iter()
                .position(|s| s.name == name)
                .expect("Struct should already be pre-registered");

            let struct_proto = &mut ctx.structs[struct_idx];

            struct_proto.fields = actual_fields;
            struct_proto.methods = method_map;

            ctx.add_symbol(name.clone(), reg, str_type.clone());
            ctx.add_const(ConstValue::Struct(struct_idx));
        }
        
        AstNode::Function { name, params, body, return_type, generics } => {
            let mut func_ctx = CompileCtx::with_parent(ctx);
            
            for param in &params {
                let reg = func_ctx.regs.alloc();
                func_ctx.add_symbol(param.ident.clone(), reg, param.v_type.clone());
            }

            let proto_idx = ctx.protos.len();
            let reg = ctx.regs.alloc();

            let mut param_types = vec![];
            let mut generic_types = vec![];

            for param in &params {
                param_types.push(param.v_type.clone());
            }
            for name in &generics {
                generic_types.push(VType::Generic(name.clone()));
            }
            
            ctx.add_symbol(name.clone(), reg, VType::Function(generic_types, param_types, Box::new(return_type.clone())));
            ctx.add_const(ConstValue::Function(proto_idx));

            let mut func_code = vec![];
            for stmt in body {
                compile_stmt(*stmt, &mut func_ctx, &mut func_code);
            }

            ctx.protos.push(PrototypeFunction {
                name: name.clone(),
                params,
                code: func_code,
                ctx: func_ctx,
                return_type,
                generics,
            });
        }
        
        AstNode::Call { callee, args, generics } => {
            let temp_reg = ctx.regs.alloc();
            code.extend(compile_expr(AstNode::Call { callee, args, generics }, ctx, temp_reg));
        }
        
        AstNode::Return { args } => {
            let a = if args.is_empty() {
                0
            } else {
                let ret_reg = ctx.regs.alloc();
                code.extend(compile_expr(args[0].clone(), ctx, ret_reg));
                ret_reg + 1
            };
            
            //code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
            code.push(pack_i_abx(Opcode::Ret, a, 0));
        }
        
        AstNode::Program(nodes) => {
            let native_entries: Vec<(String, usize)> =
                ctx.native_map.iter().map(|(n, &idx)| (n.clone(), idx)).collect();
            
            for (name, native_idx) in native_entries {
                let reg = ctx.regs.alloc();
                for ns in ctx.namespaces.values() {
                    if let Some(entry) = ns.native_map.get(name.as_str()) {
                        let mut param_types = entry.params.clone();
                        let return_type = Box::new(entry.return_type.clone());
                        let mut generic_types = vec![];
                        for v in &entry.generic_names {
                            generic_types.push(VType::Generic(v.clone()));
                        }
                        ctx.add_symbol(name.clone(), reg, VType::Function(generic_types, param_types, return_type));
                        ctx.find_or_add_const(ConstValue::NativeFunction(native_idx));
                        break;
                    }
                }
            }

            for node in &nodes {
                if let AstNode::StructDecl { name, generics, .. } = node {
                    let struct_type = VType::Struct(name.clone(), vec![]);
                    
                    ctx.structs.push(StructPrototype {
                        name: name.clone(),
                        fields: vec![],
                        methods: HashMap::new(),
                        struct_type: struct_type.clone(),
                        generics: generics.clone(),
                    });
                }
            }

            for node in &nodes {
                if let AstNode::StructDecl { name, fields, .. } = node {
                    let struct_proto = ctx.structs.iter_mut()
                        .find(|s| s.name == *name)
                        .unwrap();

                    struct_proto.fields = fields.iter()
                        .map(|p| (p.ident.clone(), p.v_type.clone()))
                        .collect();
                }
            }

            for node in nodes {
                compile_stmt(node.clone(), ctx, code);
            }
        }

        AstNode::DoBlock(block) => {
            code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
            
            for stmt in block {
                compile_stmt(*stmt, ctx, code);
            }
            
            code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
        }
        AstNode::WhileLoop { condition, body } => {
            let loop_start = code.len();
            let cond_reg = ctx.regs.alloc();
            code.extend(compile_expr(*condition.clone(), ctx, cond_reg));
            
            let jmp_false_pos = code.len();
            code.push(pack_i_abx(Opcode::Noop, 0, 0));
            code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
            for stmt in body {
                compile_stmt(*stmt, ctx, code);
            }
            code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
            code.push(pack_i_abx(Opcode::Jmp, cond_reg, loop_start as u32)); // Use cond_reg instead of 0

            let exit_idx = code.len() as u32;
            code[jmp_false_pos] = pack_i_abx(Opcode::Jn, cond_reg, exit_idx);
        }
        AstNode::ForLoop { iteratee, params, body } => {
            let binding = &params[0];
            let binding_name = binding.ident.clone();
            let binding_type = binding.v_type.clone();

            match *iteratee {
                // Range iteration: for i: i32 in 0..10
                AstNode::BinaryOp { op, left, right } if op == ".." => {
                    let counter_reg = ctx.regs.alloc();
                    let end_reg = ctx.regs.alloc();
                    let cond_reg = ctx.regs.alloc();

                    // Evaluate start and end once before the loop
                    code.extend(compile_expr(*left, ctx, counter_reg));
                    code.extend(compile_expr(*right, ctx, end_reg));

                    // Register the binding as a symbol pointing at counter_reg
                    ctx.add_symbol(binding_name.clone(), counter_reg, binding_type);

                    // Loop start: condition check
                    let loop_start = code.len();
                    code.push(pack_i_abc(Opcode::Lt, cond_reg, counter_reg, end_reg));

                    let jmp_false_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0));

                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }

                    // counter++
                    let one_const = ctx.find_or_add_const(ConstValue::I32(1));
                    let one_reg = ctx.regs.alloc();
                    code.push(pack_i_abx(Opcode::LoadConst, one_reg, one_const));
                    code.push(pack_i_abc(Opcode::Add, counter_reg, counter_reg, one_reg));

                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));

                    // Unconditional jump back to condition
                    let true_const = ctx.find_or_add_const(ConstValue::Bool(true));
                    let always_reg = ctx.regs.alloc();
                    code.push(pack_i_abx(Opcode::LoadConst, always_reg, true_const));
                    code.push(pack_i_abx(Opcode::Jmp, always_reg, loop_start as u32));

                    let exit_idx = code.len() as u32;
                    code[jmp_false_pos] = pack_i_abx(Opcode::Jn, cond_reg, exit_idx);
                }

                // Array iteration: for item: u8 in my_array
                iteratee_node => {
                    let arr_reg = ctx.regs.alloc();
                    let counter_reg = ctx.regs.alloc();
                    let len_reg = ctx.regs.alloc();
                    let cond_reg = ctx.regs.alloc();
                    let binding_reg = ctx.regs.alloc();

                    // Evaluate the array expression once
                    code.extend(compile_expr(iteratee_node, ctx, arr_reg));

                    // Load length from the array pointer
                    code.push(pack_i_abx(Opcode::ArrayLen, arr_reg, len_reg));

                    // i = 0
                    let zero_const = ctx.find_or_add_const(ConstValue::USize(0));
                    code.push(pack_i_abx(Opcode::LoadConst, counter_reg, zero_const));

                    // Register the binding symbol
                    ctx.add_symbol(binding_name.clone(), binding_reg, binding_type);

                    // Loop start: i < len
                    let loop_start = code.len();
                    code.push(pack_i_abc(Opcode::Lt, cond_reg, counter_reg, len_reg));

                    let jmp_false_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0));

                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));

                    // binding = array[i]
                    code.push(pack_i_abc(Opcode::GetArrayIdx, binding_reg, arr_reg, counter_reg));

                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }

                    // i++
                    let one_const = ctx.find_or_add_const(ConstValue::USize(1));
                    let one_reg = ctx.regs.alloc();
                    code.push(pack_i_abx(Opcode::LoadConst, one_reg, one_const));
                    code.push(pack_i_abc(Opcode::Add, counter_reg, counter_reg, one_reg));

                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));

                    // Unconditional jump back to condition
                    let true_const = ctx.find_or_add_const(ConstValue::Bool(true));
                    let always_reg = ctx.regs.alloc();
                    code.push(pack_i_abx(Opcode::LoadConst, always_reg, true_const));
                    code.push(pack_i_abx(Opcode::Jmp, always_reg, loop_start as u32));

                    let exit_idx = code.len() as u32;
                    code[jmp_false_pos] = pack_i_abx(Opcode::Jn, cond_reg, exit_idx);
                }
            }
        }
        AstNode::When { subject, arms } => {
            let subject_reg = ctx.regs.alloc();
            let subject_ty = type_of(ctx, &subject);
            let cond_reg = ctx.regs.alloc();
            code.extend(compile_expr(*subject, ctx, subject_reg));
            
            let mut end_jumps: Vec<usize> = vec![]; // positions to patch with jump-to-end

            for arm in arms {
                if arm.is_else {
                    if let Some(binding_name) = arm.binding {
                        ctx.add_symbol(binding_name, subject_reg, subject_ty.clone());
                    }
                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in arm.body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
                    // no jump needed — else arm is last
                } else {
                    // compile each pattern as: cond_reg = (subject == pattern)
                    // if multiple patterns, OR them together
                    let mut pattern_jump_positions: Vec<usize> = vec![];

                    for (i, pattern) in arm.patterns.iter().enumerate() {
                        let pat_reg = ctx.regs.alloc();
                        code.extend(compile_expr(pattern.clone(), ctx, pat_reg));

                        // coerce pattern -> subject type BEFORE comparison
                        let pat_ty = type_of(ctx, pattern);
                        coerce_type(&pat_ty, &subject_ty, ctx, pat_reg, code);

                        code.push(pack_i_abc(Opcode::Eq, cond_reg, subject_reg, pat_reg));

                        if i < arm.patterns.len() - 1 {
                            pattern_jump_positions.push(code.len());
                            code.push(pack_i_abx(Opcode::Noop, 0, 0));
                        }
                    }

                    // after last pattern, if no match jump to next arm
                    let jump_to_next_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // patch with Jn to next arm

                    // patch the short-circuit jumps to land here (start of body)
                    let body_start = code.len() as u32;
                    for pos in pattern_jump_positions {
                        code[pos] = pack_i_abx(Opcode::Jmp, cond_reg, body_start);
                    }

                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in arm.body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));

                    // jump to end of entire when after successful arm
                    end_jumps.push(code.len());
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // patch with Jmp to end

                    // patch the "no match" jump to here (next arm)
                    let next_arm = code.len() as u32;
                    code[jump_to_next_pos] = pack_i_abx(Opcode::Jn, cond_reg, next_arm);
                }
            }

            // patch all end-of-arm jumps to here
            let end = code.len() as u32;
            let true_const = ctx.find_or_add_const(ConstValue::Bool(true));
            let always_reg = ctx.regs.alloc();
            code.push(pack_i_abx(Opcode::LoadConst, always_reg, true_const));
            for pos in end_jumps {
                code[pos] = pack_i_abx(Opcode::Jmp, always_reg, end);
            }
        }
        AstNode::ConditionalBranch { condition, body, next } => {
            match condition {
                Some(condition) => {
                    // Conditional branch (if/elif)
                    let cond_reg = ctx.regs.alloc();
                    code.extend(compile_expr(*condition, ctx, cond_reg));
                    
                    // Jump to next branch if condition is false
                    let jmp_to_next_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // Placeholder for Jn instruction
                    
                    // Compile this branch's body
                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
                    
                    // Jump to end of entire conditional chain (skip remaining branches)
                    let jmp_to_end_pos = code.len();
                    code.push(pack_i_abx(Opcode::Noop, 0, 0)); // Placeholder for Jmp instruction
                    
                    // Mark where the next branch starts
                    let next_branch_start = code.len() as u32;
                    
                    // Patch the "jump to next branch" instruction
                    code[jmp_to_next_pos] = pack_i_abx(Opcode::Jn, cond_reg, next_branch_start);
                    
                    // Recursively compile the next branch in the chain
                    if let Some(next_branch) = next {
                        compile_stmt(*next_branch, ctx, code);
                    }
                    
                    // Mark the end of the entire conditional chain
                    let end_of_chain = code.len() as u32;
                    
                    // Patch the "jump to end" instruction
                    code[jmp_to_end_pos] = pack_i_abx(Opcode::Jmp, 0, end_of_chain);
                }
                None => {
                    // Unconditional branch (else)
                    code.push(pack_i_abx(Opcode::BeginBlock, 0, 0));
                    for stmt in body {
                        compile_stmt(*stmt, ctx, code);
                    }
                    code.push(pack_i_abx(Opcode::EndBlock, 0, 0));
                    
                    // No need to compile next branch - this is the final else clause
                    // Any remaining next branches would be unreachable dead code
                }
            }
        }
        other => panic!("unhandled node '{:?}'", other),
    }
}

fn compile_field_access_chain(left: AstNode, right: AstNode, ctx: &mut CompileCtx) -> (u32, Vec<usize>) {
    let mut field_indices = Vec::new();
    fn collect_field_indices(node: &AstNode, indices: &mut Vec<usize>, ctx: &CompileCtx) {
        match node {
            AstNode::Identifier(field_name) => {
                for struct_proto in &ctx.structs {
                    if let Some(field_idx) = struct_proto.fields.iter()
                        .position(|(fname, _)| fname == field_name) 
                    {
                        indices.push(field_idx);
                        return;
                    }
                }
                panic!("Field '{}' not found in any struct", field_name);
            }
            AstNode::BinaryOp { op, left: _, right } if op == "." => {
                collect_field_indices(right, indices, ctx);
            }
            _ => panic!("Invalid field access pattern in chain: {:?}", node)
        }
    }
    
    // Get the base struct register
    let struct_reg = match left {
        AstNode::Identifier(name) => {
            let symbol = ctx.find_symbol(&name)
                .unwrap_or_else(|| panic!("Unknown identifier `{}`", name));
            symbol.reg
        }
        AstNode::BinaryOp { op, left, right } if op == "." => {
            // Handle nested struct access
            let (base_reg, mut base_indices) = compile_field_access_chain(*left, *right, ctx);
            field_indices.append(&mut base_indices);
            base_reg
        }
        _ => panic!("Invalid base for field access: {:?}", left)
    };
    
    // Collect field indices from the right side
    collect_field_indices(&right, &mut field_indices, ctx);
    
    (struct_reg, field_indices)
}

pub fn resolve_import_exports<F>(
    program: &mut AstNode,
    ctx: &mut CompileCtx,
    all_natives: &mut Vec<NativeFunction>,
    mut native_modules: NativeModuleRegistry,
    read_file: F,
) -> Result<(), String>
where
    F: Fn(&str) -> Result<String, String>,
{
    let nodes = match program {
        AstNode::Program(nodes) => nodes,
        _ => return Err("resolve_import_exports: expected AstNode::Program".into()),
    };

    let mut imports: Vec<(String, String)> = vec![];
    let mut uses: Vec<(String, Vec<(String, String)>)> = vec![];

    for node in nodes.iter() {
        match node {
            AstNode::Import { alias, path } => imports.push((alias.clone(), path.clone())),
            AstNode::Use { module_alias, items } => uses.push((module_alias.clone(), items.clone())),
            _ => {}
        }
    }

    let mut module_exports: HashMap<String, Vec<(String, AstNode)>> = HashMap::new();

    for (alias, path) in &imports {
        if let Some(mut ns) = native_modules.remove(path) {
            ns.name = alias.clone(); // user's alias wins
            ctx.register_native_namespaces(all_natives, vec![ns]);
            continue;
        }

        let src = read_file(path)
            .map_err(|e| format!("import '{}': could not read file: {}", path, e))?;

        let tokens = tokenize(src);
        let module_ast = Parser::new(tokens)
            .parse()
            .map_err(|e| format!("import '{}': parse error: {}", path, e))?;

        let exported = collect_module_exports(module_ast)?;

        let mut ns = Namespace {
            symbols: HashMap::new(),
            const_symbols: HashMap::new(),
            fn_map: HashMap::new(),
            native_map: HashMap::new(),
        };
        
        let clone = exported.clone();
        for (name, node) in exported {
            inject_into_namespace(ctx, &mut ns, node, &name)?;
        }
        
        ctx.namespaces.insert(alias.clone(), ns);
        module_exports.insert(alias.clone(), clone);
    }

    for (module_alias, requested_items) in &uses {
        // Native namespace, pull requested items into top-level scope
        if ctx.namespaces.contains_key(module_alias) {
            let entries: Vec<((String, String), NativeEntry)> = {
                let ns = ctx.namespaces.get(module_alias).unwrap();
                requested_items.iter()
                    .filter_map(|item| {
                        let (name, alias) = item;
                        ns.native_map.get(name).map(|entry| (item.clone(), entry.clone()))
                    })
                    .collect()
            };

            for (item, entry) in entries {
                let (name, alias) = item;
                ctx.native_map.insert(alias.clone(), entry.global_idx);
                let reg = ctx.regs.alloc();

                let mut param_types = entry.params.clone();
                let return_type = Box::new(entry.return_type.clone());
                let mut generic_types = vec![];
                for v in &entry.generic_names {
                    generic_types.push(VType::Generic(v.clone()));
                }

                ctx.add_symbol(alias.clone(), reg, VType::Function(
                    generic_types, param_types, return_type
                ));
                ctx.find_or_add_const(ConstValue::NativeFunction(entry.global_idx));
            }
            continue;
        }
        
        let exports = module_exports.get(module_alias).ok_or_else(|| {
            format!("`use {}`: no `import` with that alias found", module_alias)
        })?;
        
        for (export_name, node) in exports {
            if let Some((_, alias)) = requested_items.iter()
                .find(|(req, _)| req == export_name)
            {
                inject_export_into_ctx(ctx, node.clone(), alias)?;
            }
        }
    }

    resolve_all_unresolved_types(program, ctx)?;

    Ok(())
}

fn resolve_all_unresolved_types(node: &mut AstNode, ctx: &CompileCtx) -> Result<(), String> {
    match node {
        AstNode::Program(nodes) => {
            for n in nodes.iter_mut() { resolve_all_unresolved_types(n, ctx)?; }
        }
        AstNode::Function { params, return_type, body, .. } => {
            for param in params { param.v_type = resolve_type_in_ctx(&param.v_type, ctx)?; }
            *return_type = resolve_type_in_ctx(return_type, ctx)?;
            for stmt in body { resolve_all_unresolved_types(stmt, ctx)?; }
        }
        AstNode::Declaration { v_type, .. } => {
            *v_type = resolve_type_in_ctx(v_type, ctx)?;
        }
        AstNode::StructDecl { fields, .. } => {
            for param in fields.iter_mut() {
                param.v_type = resolve_type_in_ctx(&param.v_type, ctx)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn resolve_type_in_ctx(ty: &VType, ctx: &CompileCtx) -> Result<VType, String> {
    match ty {
        VType::Unresolved(full_name) => {
            let parts: Vec<_> = full_name.split("::").collect();
            let name = if parts.len() == 2 { parts[1] } else { full_name.as_str() };

            for s in &ctx.structs {
                if s.name == *full_name {
                    return Ok(VType::Struct(full_name.to_string(), vec![]));
                }
            }

            Err(format!("resolve_type_in_ctx: cannot resolve '{}'", full_name))
        }
        VType::Struct(name, generics) => {
            let resolved_generics: Result<Vec<_>, _> =
                generics.iter().map(|g| resolve_type_in_ctx(g, ctx)).collect();
            Ok(VType::Struct(name.clone(), resolved_generics?))
        }
        _ => Ok(ty.clone()),
    }
}

fn collect_module_exports(program: AstNode) -> Result<Vec<(String, AstNode)>, String> {
    let nodes = match program {
        AstNode::Program(nodes) => nodes,
        _ => return Err("collect_module_exports: expected Program node".into()),
    };

    let mut out = Vec::new();
    for node in nodes {
        if let AstNode::Export { item } = node {
            let name = export_name(&item)
                .ok_or_else(|| format!("exported item has no retrievable name: {:?}", item))?;
            out.push((name, *item));
        }
    }

    Ok(out)
}

fn export_name(node: &AstNode) -> Option<String> {
    match node {
        AstNode::Function { name, .. } => Some(name.clone()),
        AstNode::StructDecl { name, .. } => Some(name.clone()),
        AstNode::Declaration { name, .. } => {
            if let AstNode::Identifier(n) = name.as_ref() { Some(n.clone()) } else { None }
        }
        _ => None,
    }
}

pub fn inject_into_namespace(
    ctx: &mut CompileCtx,
    ns: &mut Namespace,
    node: AstNode,
    name: &str
) -> Result<(), String> {
    match node {
        AstNode::Function { params, body, return_type, generics, .. } => {
            let mut func_ctx = CompileCtx::with_parent(ctx);

            for param in &params {
                let reg = func_ctx.regs.alloc();
                func_ctx.add_symbol(param.ident.clone(), reg, param.v_type.clone());
            }

            let mut func_code = vec![];
            for stmt in body {
                compile_stmt(*stmt, &mut func_ctx, &mut func_code);
            }

            let proto_idx = ctx.protos.len();
            ctx.protos.push(PrototypeFunction {
                name: name.to_string(),
                params,
                code: func_code,
                ctx: func_ctx,
                generics,
                return_type,
            });

            ns.fn_map.insert(name.to_string(), proto_idx);
        }

        AstNode::StructDecl { name, fields, struct_type, generics, methods } => {
            let struct_idx = ctx.structs.len() - 1;

            let mut actual_fields = Vec::new();
            let mut method_map = HashMap::new();

            for p in fields {
                actual_fields.push((p.ident, p.v_type));
            }

            // methods (ONLY register indices here, assume already compiled elsewhere)
            for m in methods {
                let (name, node) = m;
                if let AstNode::Function { name: fn_name, .. } = node {
                    let fn_idx = ctx.protos.iter()
                        .position(|p| p.name == format!("{}::{}", name, fn_name))
                        .unwrap_or_else(|| panic!("Method '{}' not compiled", fn_name));

                    method_map.insert(fn_name, fn_idx);
                } else {
                    panic!("Struct method is not a function");
                }
            }

            ctx.structs.push(StructPrototype {
                name: name.to_string(),
                fields: actual_fields,
                methods: method_map,
                struct_type: struct_type.clone(),
                generics,
            });

            ns.symbols.insert(name.to_string(), Symbol {
                name: name.to_string(),
                reg: 0,
                ty: struct_type,
            });
        }

        AstNode::Declaration { value, v_type, .. } => {
            let reg = ctx.regs.alloc();
            let mut code = vec![];
            code.extend(compile_expr(*value, ctx, reg));

            let idx = reg; // or const extraction

            ns.const_symbols.insert(name.to_string(), idx);
        }

        _ => return Err("Unsupported export".into()),
    }

    Ok(())
}

fn inject_export_into_ctx(ctx: &mut CompileCtx, node: AstNode, name: &str) -> Result<(), String> {
    match node {
        AstNode::Function { params, body, return_type, generics, .. } => {
            if ctx.find_fn_addr_by_name(name).is_some() { return Ok(()); }

            let mut func_ctx = CompileCtx::with_parent(ctx);
            for param in &params {
                let reg = func_ctx.regs.alloc();
                func_ctx.add_symbol(param.ident.clone(), reg, param.v_type.clone());
            }

            let mut func_code = vec![];
            for stmt in body { compile_stmt(*stmt, &mut func_ctx, &mut func_code); }

            let proto_idx = ctx.protos.len();
            let reg = ctx.regs.alloc();

            let mut param_types = vec![];
            let mut generic_types = vec![];

            for param in &params {
                param_types.push(param.v_type.clone());
            }
            for name in &generics {
                generic_types.push(VType::Generic(name.clone()));
            }

            ctx.add_symbol(name.to_string(), reg, VType::Function(generic_types, param_types, Box::new(return_type.clone())));
            ctx.find_or_add_const(ConstValue::Function(proto_idx));
            ctx.protos.push(PrototypeFunction {
                name: name.to_string(),
                params,
                code: func_code,
                ctx: func_ctx,
                generics,
                return_type,
            });
        }

        AstNode::StructDecl { name, fields, struct_type, generics, methods, .. } => {
            if ctx.structs.iter().any(|s| s.name == name) { return Ok(()); }

            let struct_idx = ctx.structs.len() - 1;
            let reg = ctx.regs.alloc();

            ctx.add_symbol(name.to_string(), reg, VType::Struct(name.to_string(), vec![]));
            ctx.add_const(ConstValue::Struct(struct_idx));

            let mut actual_fields = Vec::new();
            let mut method_map = HashMap::new();
            
            for p in fields {
                actual_fields.push((p.ident, p.v_type));
            }

            for m in methods {
                let (name, node) = m;
                if let AstNode::Function { name: fn_name, .. } = node {
                    let fn_idx = ctx.protos.iter()
                        .position(|p| p.name == format!("{}::{}", name, fn_name))
                        .unwrap_or_else(|| panic!("Method '{}' not compiled", fn_name));

                    method_map.insert(fn_name, fn_idx);
                } else {
                    panic!("Struct method is not a function");
                }
            }

            ctx.structs.push(StructPrototype {
                name: name.to_string(),
                fields: actual_fields,
                methods: method_map,
                struct_type,
                generics,
            });
        }

        AstNode::Declaration { value, v_type, .. } => {
            if ctx.const_symbols.contains_key(name) { return Ok(()); }

            let inferred_ty = type_of(ctx, &value);
            let temp_reg = ctx.regs.alloc();
            let mut temp_code = vec![];
            temp_code.extend(compile_expr(*value, ctx, temp_reg));

            let resolved_ty = if v_type == VType::Auto { inferred_ty.clone() } else { v_type.clone() };
            coerce_type(&inferred_ty, &resolved_ty, ctx, temp_reg, &mut temp_code);

            let final_idx = match resolved_ty {
                VType::U8   => ctx.find_or_add_const(ConstValue::U8(extract_i32(&ctx, temp_code[0]) as u8)),
                VType::I8   => ctx.find_or_add_const(ConstValue::I8(extract_i32(&ctx, temp_code[0]) as i8)),
                VType::U16  => ctx.find_or_add_const(ConstValue::U16(extract_i32(&ctx, temp_code[0]) as u16)),
                VType::I16  => ctx.find_or_add_const(ConstValue::I16(extract_i32(&ctx, temp_code[0]) as i16)),
                VType::U32  => ctx.find_or_add_const(ConstValue::U32(extract_i32(&ctx, temp_code[0]) as u32)),
                VType::I32  => ctx.find_or_add_const(ConstValue::I32(extract_i32(&ctx, temp_code[0]))),
                VType::F32  => ctx.find_or_add_const(ConstValue::F32(extract_i32(&ctx, temp_code[0]) as f32)),
                VType::F64  => ctx.find_or_add_const(ConstValue::F64(extract_i32(&ctx, temp_code[0]) as f64)),
                VType::USize => ctx.find_or_add_const(ConstValue::USize(extract_i32(&ctx, temp_code[0]) as usize)),
                _ => { let bx = temp_code[0] >> B_SHIFT; bx }
            };

            ctx.const_symbols.insert(name.to_string(), final_idx);
        }

        other => return Err(format!("inject_export_into_ctx: unsupported export kind: {:?}", other)),
    }

    Ok(())
}

fn extract_i32(ctx: &CompileCtx, load_const_instr: Instruction) -> i32 {
    let bx = load_const_instr >> B_SHIFT;
    match &ctx.consts[bx as usize] {
        ConstValue::I32(n) => *n,
        ConstValue::U8(n)  => *n as i32,
        ConstValue::I8(n)  => *n as i32,
        ConstValue::U16(n) => *n as i32,
        ConstValue::I16(n) => *n as i32,
        ConstValue::U32(n) => *n as i32,
        ConstValue::F32(n) => *n as i32,
        ConstValue::F64(n) => *n as i32,
        ConstValue::USize(n) => *n as i32,
        other => panic!("extract_i32: unexpected const {:?}", other),
    }
}