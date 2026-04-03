use std::collections::HashMap;
use crate::ast::{AstNode, Parameter};
use crate::v_type::VType;
use crate::vm::NativeFunction;

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
}

#[derive(Debug, Clone)]
pub struct StructPrototype {
    pub name: String,
    pub fields: Vec<(String, VType)>,
    pub struct_type: VType,
    pub generics: Vec<String>
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
                .unwrap_or_else(|| panic!("Unbound generic '{}'", name))
                .clone()
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
    // Build unique key
    let key = format!(
        "{}<{}>",
        base_name,
        generic_args.iter()
            .map(|g| format!("{:?}", g))
            .collect::<Vec<_>>()
            .join(",")
    );

    // Cache hit
    if let Some(idx) = ctx.struct_instances.get(&key) {
        return *idx;
    }

    // Find base prototype
    let base = ctx.structs.iter()
        .find(|s| s.name == base_name)
        .unwrap_or_else(|| panic!("Unknown struct '{}'", base_name))
        .clone();

    if base.generics.len() != generic_args.len() {
        panic!("Generic arity mismatch for '{}'", base_name);
    }
    if base.generics.len() > 0 && generic_args.is_empty() {
        panic!("Generic struct '{}' requires type arguments", base_name);
    }

    // Build substitution map
    let subst: HashMap<_, _> = base.generics.iter()
        .cloned()
        .zip(generic_args.iter().cloned())
        .collect();

    // Substitute fields
    let fields = base.fields.iter()
        .map(|(name, ty)| (name.clone(), substitute_type(ty, &subst)))
        .collect::<Vec<_>>();

    // Create new prototype
    let new_proto = StructPrototype {
        name: key.clone(), // unique name for instantiated generic
        fields,
        struct_type: VType::Struct(key.clone(), vec![]),
        generics: vec![], // concrete now
    };

    // Push and get the correct index
    let new_idx = ctx.structs.len();
    ctx.structs.push(new_proto);
    ctx.struct_instances.insert(key, new_idx);

    new_idx
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
            ConstValue::I32(i) => find_const_arm!(self.consts, ConstValue::I32, i),
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
        // Identity — nothing to do
        (a, b) if a == b => {}
        (a, VType::Auto) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        },

        (VType::Generic(..), b) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        },

        // Numeric widening / narrowing — emit a Cast instruction
        (VType::U8  | VType::I8  |
         VType::U16 | VType::I16 |
         VType::U32 | VType::I32 |
         VType::F32 | VType::F64 |
         VType::USize,
         VType::U8  | VType::I8  |
         VType::U16 | VType::I16 |
         VType::U32 | VType::I32 |
         VType::F32 | VType::F64 |
         VType::USize) => {
            let type_const = ctx.find_or_add_const(ConstValue::Type(to.clone()));
            // Cast A Bx  →  R[A] = R[A] cast-to const[Bx]
            code.push(pack_i_abx(Opcode::Cast, reg, type_const));
        }

        (VType::Array(from_elem), VType::Array(to_elem)) => {
            if from_elem != to_elem {
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

fn type_of(ctx: &mut CompileCtx, node: &AstNode) -> VType
{
    match node {
        AstNode::Index { target, .. } => {
            match type_of(ctx, &*target) {
                VType::Array(elem_type) => *elem_type,
                other => panic!("Cannot index into non-array type: {:?}", other),
            }
        }
        AstNode::String(..) => VType::String,
        AstNode::Integer(..) => VType::I32,
        AstNode::Call {callee, args, generics} => {
            if let AstNode::Identifier(name) = callee.as_ref() {
                let proto_idx = ctx.find_fn_addr_by_name(name.as_str())
                    .unwrap_or_else(|| {
                        panic!("unknown symbol")
                    });
                let proto = &ctx.protos[proto_idx];
                return proto.return_type.clone();
            }
            panic!("Identifier empty?");
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
            let sym = ctx.find_symbol(name.as_str())
                .unwrap_or_else(|| {
                    panic!("unknown symbol")
                });
            sym.ty.clone()
        },
        AstNode::BinaryOp {op, left, right} => {
            match op.as_str() {
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
            else {
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
        AstNode::Call { callee, args, generics } => {
            code.extend(compile_expr(*callee, ctx, reg));

            let mut arg_regs = vec![];
            for _ in 0..args.len() {
                let a = ctx.regs.alloc();
                arg_regs.push(a);
            }

            for (i, arg) in args.into_iter().enumerate() {
                let arg_reg = arg_regs[i];
                code.extend(compile_expr(*arg, ctx, arg_reg));
            }
            
            let num_args = arg_regs.len() as u32;
            // Convention: CALL A Bx -> function in R[A], args in R[A+1 .. A+Bx], returns into R[A]
            code.push(pack_i_abx(Opcode::Call, reg, num_args));
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
            println!("{}, {:?}", name, struct_proto.fields);
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

            // Infer element type from the first element; all others must match or be
            // coercible to it.
            let elem_type = type_of(ctx, &exprs[0]);
            let len = exprs.len();

            // Allocate registers for all elements up-front (mirrors the Call convention).
            let mut elem_regs: Vec<u32> = (0..len).map(|_| ctx.regs.alloc()).collect();

            // Emit a NewArray instruction: A = destination reg, Bx = const-pool index
            // for an Array(elem_type, len) descriptor.
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

                // Compile the element expression into its dedicated register.
                code.extend(compile_expr(*expr, ctx, elem_reg));

                // Coerce the element to the array's canonical element type if needed.
                if expr_type != elem_type {
                    let mut coerce_code = vec![];
                    coerce_type(&expr_type, &elem_type, ctx, elem_reg, &mut coerce_code);
                    code.extend(coerce_code);
                }

                // Emit the index constant and the SetArrayIdx instruction.
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
        AstNode::Assignment { assignee, value } => {
            match *assignee {
                AstNode::Identifier(name) => {
                    if let Some(symbol) = ctx.find_symbol(&name) {
                        let target_reg = symbol.reg;
                        code.extend(compile_expr(*value, ctx, target_reg));
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
                    
                    // For nested field access, need to traverse to the final struct
                    // and then store to the final field
                    if field_indices.len() == 1 {
                        // Simple case: struct.field = value
                        code.push(pack_i_abc(Opcode::StoreStructField, struct_reg, field_indices[0] as u32, value_reg));
                    } else {
                        // Complex case: struct.field1.field2 = value
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
            let t = type_of(ctx, &value);
            
            if let AstNode::Identifier(name_str) = *name {
                let reg = ctx.regs.alloc();
                ctx.add_symbol(name_str.clone(), reg, v_type.clone());
                code.extend(compile_expr(*value, ctx, reg));
                
                if t != v_type {
                    // Attempt coercion — coerce_type will panic with a descriptive
                    // message if the types are genuinely incompatible.
                    coerce_type(&t, &v_type, ctx, reg, code);
                }
            } else {
                panic!("Declaration: expected identifier as binding name, got {:?}", name);
            }
        }

        AstNode::StructDecl { name, fields, struct_type , generics} => {
            let str_type = VType::Struct(name.clone(), vec![]);
            let struct_idx = ctx.structs.len();
            let reg = ctx.regs.alloc();
            ctx.add_symbol(name.clone(), reg, str_type.clone());
            ctx.add_const(ConstValue::Struct(struct_idx));

            let mut actual_fields = Vec::new();
            for p in fields {
                actual_fields.push(
                    (p.ident, p.v_type)
                )
            }

            let proto = StructPrototype {
                name: name.clone(),
                fields: actual_fields,
                struct_type: str_type,
                generics
            };
            ctx.structs.push(proto)
        }
        
        AstNode::Function { name, params, body, return_type } => {
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
            let reg = ctx.regs.alloc();
            ctx.add_symbol(name.clone(), reg, VType::Function);
            ctx.add_const(ConstValue::Function(proto_idx));
            
            let proto = PrototypeFunction {
                name: name.clone(),
                params: params,
                code: func_code,
                ctx: func_ctx,
                return_type,
            };
            ctx.protos.push(proto);
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
                ctx.add_symbol(name.clone(), reg, VType::Function);
                ctx.add_const(ConstValue::NativeFunction(native_idx));
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
                // Find the field index - in a real implementation you'd want proper type tracking
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