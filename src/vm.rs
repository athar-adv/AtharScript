use crate::compiler::{coerce_type, ConstValue, Instruction, Opcode, PrototypeFunction, StructPrototype, A_SHIFT, B_SHIFT, C_SHIFT, OPCODE_BITS};
use crate::compiler::ConstValue as CV;
use crate::VType;

#[derive(Debug, Clone)]
pub enum StackSlot {
    Struct(StructInstance),
    Array(Vec<RuntimeValue>),
    Empty
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    U8(u8),
    I8(i8),

    U16(u16),
    I16(i16),

    U32(u32),
    I32(i32),

    F32(f32),
    F64(f64),

    Str(String),
    USize(usize),
    Bool(bool),

    Ptr(VType, usize),

    Function(usize),
    Struct(usize),
    Array(Vec<RuntimeValue>),
    NativeFunction(usize),

    Empty,
}

#[derive(Debug, Clone)]
pub struct StructInstance {
    struct_idx: usize,
    concrete_type: Vec<VType>, // 🔥 THIS
    fields: Vec<RuntimeValue>,
}

#[derive(Debug)]
pub struct NativeFunction {
    pub name: String,
    //pub param_count: usize,
    pub func: fn(&[RuntimeValue]) -> RuntimeValue,
}

#[derive(Debug)]
pub struct CallFrame {
    proto_index: usize,     // which prototype we're running
    pc: usize,              // instruction pointer in that proto
    regs: Vec<RuntimeValue>, // registers for this frame (register 0..N)
    stack_base: usize,
    local_var_bases: Vec<usize>,
}

fn resolve_type(
    ty: &VType,
    concrete: &[VType],
    generic_names: &[String]
) -> VType {
    match ty {
        VType::Generic(name) => {
            let idx = generic_names.iter()
                .position(|g| g == name)
                .expect("Generic not found");

            concrete.get(idx)
                .cloned()
                .expect("Missing concrete generic type")
        }
        VType::Array(inner) => {
            VType::Array(Box::new(
                resolve_type(inner, concrete, generic_names)
            ))
        }
        VType::Struct(name, generics) => {
            VType::Struct(
                name.clone(),
                generics.iter()
                    .map(|g| resolve_type(g, concrete, generic_names))
                    .collect()
            )
        }
        VType::Function => VType::Function,

        // everything else
        other => other.clone(),
    }
}

macro_rules! apply_binop {
    ($left:expr, $right:expr, $op:tt) => {
        match ($left, $right) {
            (RuntimeValue::I32(l),   RuntimeValue::I32(r))   => RuntimeValue::I32(l $op r),
            (RuntimeValue::U32(l),   RuntimeValue::U32(r))   => RuntimeValue::U32(l $op r),
            (RuntimeValue::I8(l),    RuntimeValue::I8(r))    => RuntimeValue::I8(l $op r),
            (RuntimeValue::U8(l),    RuntimeValue::U8(r))    => RuntimeValue::U8(l $op r),
            (RuntimeValue::I16(l),   RuntimeValue::I16(r))   => RuntimeValue::I16(l $op r),
            (RuntimeValue::U16(l),   RuntimeValue::U16(r))   => RuntimeValue::U16(l $op r),
            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => RuntimeValue::USize(l $op r),
            (RuntimeValue::F32(l),   RuntimeValue::F32(r))   => RuntimeValue::F32(l $op r),
            (RuntimeValue::F64(l),   RuntimeValue::F64(r))   => RuntimeValue::F64(l $op r),
            (l, r) => panic!("Binary operation applied to incompatible operands: {:?} {:?}", l, r),
        }
    };
}

fn can_cast_type(from: &VType, to: &VType) -> bool {
    if from == to {
        return true;
    }

    match (from, to) {
        (VType::U8, VType::U16 | VType::U32 | VType::I32 | VType::I16 | VType::USize | VType::F32 | VType::F64) => true,
        (VType::U16, VType::U32 | VType::I32 | VType::USize | VType::F32 | VType::F64 | VType::U8 | VType::I16) => true,
        (VType::U32, VType::USize | VType::F32 | VType::F64 | VType::I32 | VType::U8 | VType::U16 | VType::I16) => true,

        (VType::I8, VType::I16 | VType::I32 | VType::F32 | VType::F64 | VType::U8 | VType::U16 | VType::U32) => true,
        (VType::I16, VType::I32 | VType::F32 | VType::F64 | VType::U8 | VType::U16 | VType::U32 | VType::I8) => true,
        (VType::I32, VType::I8 | VType::I16 | VType::U8 | VType::U16 | VType::U32 | VType::F32 | VType::F64 | VType::USize) => true,

        (VType::F32, VType::F64 | VType::I32 | VType::U32 | VType::I8 | VType::U8 | VType::USize) => true,
        (VType::F64, VType::F32 | VType::I32 | VType::U32 | VType::I8 | VType::U8 | VType::USize) => true,

        (VType::USize, VType::I32 | VType::U32 | VType::U8 | VType::U16 | VType::F32 | VType::F64) => true,
        (VType::Array(a), VType::Array(b)) => can_cast_type(a, b),
        (VType::Struct(n1, g1), VType::Struct(n2, g2)) => {
            n1 == n2 && g1 == g2
        }

        _ => false,
    }
}

fn cast_runtime_value(val: RuntimeValue, to: &VType) -> RuntimeValue {
    match (val, to) {
        (v, VType::Auto) => v,

        (RuntimeValue::Ptr(VType::Array(from), idx), VType::Array(to)) => {
            if can_cast_type(&from, to) {
                RuntimeValue::Ptr(VType::Array(to.clone()), idx)
            } else {
                panic!("Array element type mismatch: {:?} -> {:?}", from, to)
            }
        }
        (RuntimeValue::Ptr(VType::Struct(name1, g1), idx), VType::Struct(name2, g2)) => {
            if name1 == *name2 && g1 == *g2 {
                RuntimeValue::Ptr(VType::Struct(name2.clone(), g2.clone()), idx)
            } else {
                panic!("Struct type mismatch: {:?} -> {:?}", name1, name2)
            }
        }
        
        // ── identity ──────────────────────────────────────────────────────────
        (RuntimeValue::I32(n),  VType::I32)   => RuntimeValue::I32(n),
        (RuntimeValue::U8(n),   VType::U8)    => RuntimeValue::U8(n),
        (RuntimeValue::I8(n),   VType::I8)    => RuntimeValue::I8(n),
        (RuntimeValue::U16(n),  VType::U16)   => RuntimeValue::U16(n),
        (RuntimeValue::I16(n),  VType::I16)   => RuntimeValue::I16(n),
        (RuntimeValue::U32(n),  VType::U32)   => RuntimeValue::U32(n),
        (RuntimeValue::F32(n),  VType::F32)   => RuntimeValue::F32(n),
        (RuntimeValue::F64(n),  VType::F64)   => RuntimeValue::F64(n),
        (RuntimeValue::USize(n),VType::USize) => RuntimeValue::USize(n),

        // ── unsigned widening / narrowing ─────────────────────────────────────
        (RuntimeValue::U8(n),  VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::U8(n),  VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::U8(n),  VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::U8(n),  VType::I16)  => RuntimeValue::I16(n as i16),
        (RuntimeValue::U8(n),  VType::USize)=> RuntimeValue::USize(n as usize),
        (RuntimeValue::U8(n),  VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::U8(n),  VType::F64)  => RuntimeValue::F64(n as f64),

        (RuntimeValue::U16(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::U16(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::U16(n), VType::USize)=> RuntimeValue::USize(n as usize),
        (RuntimeValue::U16(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::U16(n), VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::U16(n), VType::U8)   => RuntimeValue::U8(n as u8),   // narrowing
        (RuntimeValue::U16(n), VType::I16)  => RuntimeValue::I16(n as i16),

        (RuntimeValue::U32(n), VType::USize)=> RuntimeValue::USize(n as usize),
        (RuntimeValue::U32(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::U32(n), VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::U32(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::U32(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::U32(n), VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::U32(n), VType::I16)  => RuntimeValue::I16(n as i16),

        // ── signed widening / narrowing ───────────────────────────────────────
        (RuntimeValue::I8(n),  VType::I16)  => RuntimeValue::I16(n as i16),
        (RuntimeValue::I8(n),  VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::I8(n),  VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::I8(n),  VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::I8(n),  VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::I8(n),  VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::I8(n),  VType::U32)  => RuntimeValue::U32(n as u32),

        (RuntimeValue::I16(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::I16(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::I16(n), VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::I16(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::I16(n), VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::I16(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::I16(n), VType::I8)   => RuntimeValue::I8(n as i8),

        (RuntimeValue::I32(n), VType::I8)   => RuntimeValue::I8(n as i8),
        (RuntimeValue::I32(n), VType::I16)  => RuntimeValue::I16(n as i16),
        (RuntimeValue::I32(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::I32(n), VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::I32(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::I32(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::I32(n), VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::I32(n), VType::USize)=> RuntimeValue::USize(n as usize),

        // ── float conversions ─────────────────────────────────────────────────
        (RuntimeValue::F32(n), VType::F64)  => RuntimeValue::F64(n as f64),
        (RuntimeValue::F32(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::F32(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::F32(n), VType::I8)   => RuntimeValue::I8(n as i8),
        (RuntimeValue::F32(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::F32(n), VType::USize)=> RuntimeValue::USize(n as usize),

        (RuntimeValue::F64(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::F64(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::F64(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::F64(n), VType::I8)   => RuntimeValue::I8(n as i8),
        (RuntimeValue::F64(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::F64(n), VType::USize)=> RuntimeValue::USize(n as usize),

        // ── usize ─────────────────────────────────────────────────────────────
        (RuntimeValue::USize(n), VType::I32)  => RuntimeValue::I32(n as i32),
        (RuntimeValue::USize(n), VType::U32)  => RuntimeValue::U32(n as u32),
        (RuntimeValue::USize(n), VType::U8)   => RuntimeValue::U8(n as u8),
        (RuntimeValue::USize(n), VType::U16)  => RuntimeValue::U16(n as u16),
        (RuntimeValue::USize(n), VType::F32)  => RuntimeValue::F32(n as f32),
        (RuntimeValue::USize(n), VType::F64)  => RuntimeValue::F64(n as f64),

        (RuntimeValue::Str(v), VType::String) => RuntimeValue::Str(v),
        
        (src, target) => panic!(
            "cast_runtime_value: no cast defined from {:?} to {:?}",
            src, target
        ),
    }
}

fn cast_value(val: RuntimeValue, to: &VType, stack: &mut [StackSlot]) -> RuntimeValue {
    match val {
        RuntimeValue::Ptr(VType::Array(_), stack_idx) => {
            match to {
                VType::Array(to_ty) => {
                    let old_elements = match std::mem::replace(&mut stack[stack_idx], StackSlot::Array(vec![])) {
                        StackSlot::Array(vec) => vec,
                        _ => panic!("Ptr does not point to an array at index {}", stack_idx),
                    };

                    let new_elements: Vec<RuntimeValue> = old_elements
                        .into_iter()
                        .map(|elem| cast_value(elem, to_ty, stack))
                        .collect();

                    stack[stack_idx] = StackSlot::Array(new_elements);
                    RuntimeValue::Ptr(VType::Array(Box::new(*to_ty.clone())), stack_idx)
                }
                VType::Auto => {
                    RuntimeValue::Ptr(VType::Array(Box::new(VType::Auto)), stack_idx)
                }
                other => panic!("Cannot cast array pointer to non-array type: {:?}", other),
            }
        }

        RuntimeValue::Ptr(VType::Struct(name1, g1), idx) => {
            match to {
                VType::Auto => {
                    RuntimeValue::Ptr(VType::Struct(name1, g1), idx)
                }
                VType::Struct(name2, g2) => {
                    if &name1 == name2 && &g1 == g2 {
                        RuntimeValue::Ptr(VType::Struct(name2.clone(), g2.clone()), idx)
                    } else {
                        panic!("Struct type mismatch: {:?} -> {:?}", name1, name2);
                    }
                }
                other => panic!("Cannot cast struct pointer to {:?}", other),
            }
        }

        other => cast_runtime_value(other, to),
    }
}

#[derive(Debug)]
pub struct VM {
    pub native_functions: Vec<NativeFunction>,
    pub protos: Vec<PrototypeFunction>, // prototypes compiled earlier
    pub struct_protos: Vec<StructPrototype>,
    pub consts: Vec<ConstValue>,        // top-level constant pool (if any)
    pub frames: Vec<CallFrame>,         // call stack
    pub frame_regs: usize,
    pub stack: Vec<StackSlot>,
    pub block_stack_bases: Vec<usize>,
}

impl VM {
    pub fn new(native_functions: Vec<NativeFunction>, protos: Vec<PrototypeFunction>, struct_protos: Vec<StructPrototype>, consts: Vec<ConstValue>) -> Self {
        Self {
            native_functions,
            protos,
            struct_protos,
            consts,
            frames: Vec::new(),
            frame_regs: 256,
            stack: Vec::new(),
            block_stack_bases: Vec::new()
        }
    }
    
    pub fn register_native(&mut self, name: &str, /*param_count: usize, */func: fn(&[RuntimeValue]) -> RuntimeValue) -> usize {
        let idx = self.native_functions.len();
        self.native_functions.push(NativeFunction {
            name: name.to_string(),
            //param_count,
            func,
        });
        idx
    }
    
   fn get_struct_mut(&mut self, stack_idx: usize) -> &mut StructInstance {
        match self.stack.get_mut(stack_idx) {
            Some(StackSlot::Struct(s)) => s,
            _ => panic!("Invalid struct stack index: {}", stack_idx),
        }
    }

    fn get_struct(&self, stack_idx: usize) -> &StructInstance {
        match self.stack.get(stack_idx) {
            Some(StackSlot::Struct(s)) => s,
            _ => panic!("Invalid struct stack index: {}", stack_idx),
        }
    }

    fn cleanup_stack(&mut self, stack_base: usize) {
        if stack_base <= self.stack.len() {
            self.stack.truncate(stack_base);
        }
    }

    fn is_valid_struct_ptr(&self, stack_idx: usize) -> bool {
        stack_idx < self.stack.len()
    }

    pub fn run_from_proto(&mut self, main_proto_index: usize) {
        self.push_frame(main_proto_index);
        while !self.frames.is_empty() {
            let top_idx = self.frames.len() - 1;
            {
                let frame = &mut self.frames[top_idx];
                let proto = &self.protos[frame.proto_index];
                
                if frame.pc >= proto.code.len() {
                    // proto ended: implicit return (empty)
                    self.pop_frame_and_propagate(None);
                    continue;
                }

                let instr = proto.code[frame.pc];
                frame.pc += 1;

                let opcode = instr & ((1 << OPCODE_BITS) - 1);
                let a = ((instr >> A_SHIFT) & 0xFF) as usize;
                let bx = (instr >> B_SHIFT) as usize;
                
                let b = ((instr >> B_SHIFT) & 0x1FF) as usize;
                let c = ((instr >> C_SHIFT) & 0x1FF) as usize;
                
                //prI32ln!("{:?}", Opcode::try_from(opcode).unwrap());
                
                match opcode {
                    x if x == Opcode::LoadConst as u32 => {
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("LoadConst: OOB bx={} proto={}", bx, frame.proto_index));
                        
                        frame.regs[a] = match const_val {
                            CV::I32(n) => RuntimeValue::I32(n),
                            CV::Bool(b) => RuntimeValue::Bool(b),
                            CV::Empty => RuntimeValue::Empty,
                            CV::USize(n) => RuntimeValue::USize(n),
                            CV::Str(s) => RuntimeValue::Str(s),
                            CV::Function(idx) => RuntimeValue::Function(idx),
                            CV::NativeFunction(idx) => RuntimeValue::NativeFunction(idx),
                            CV::Struct(idx) => RuntimeValue::Struct(idx),
                            other => panic!("LoadConst: unsupported const variant at runtime: {:?}", other),
                        };
                    }
                    x if x == Opcode::NewStruct as u32 => {
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("NewStruct: OOB bx={}", bx));

                        if let ConstValue::StructInst(struct_idx, concrete_type) = const_val {
                            let struct_proto_idx = self.struct_protos.len() - (struct_idx - 1);
                            let struct_proto = &self.struct_protos[struct_proto_idx];

                            let instance = StructInstance {
                                struct_idx,
                                concrete_type: concrete_type.clone(),
                                fields: vec![RuntimeValue::Empty; struct_proto.fields.len()],
                            };

                            let stack_idx = self.stack.len();
                            self.stack.push(StackSlot::Struct(instance));

                            let struct_name = struct_proto.name.clone();
                            frame.regs[a] = RuntimeValue::Ptr(
                                VType::Struct(struct_name, concrete_type.clone()),
                                stack_idx
                            );
                        } else {
                            panic!("NewStruct: expected StructInst, got {:?}", const_val);
                        }
                    }

                    x if x == Opcode::StoreStructField as u32 => {
                        let raw_value = frame.regs[c].clone();

                        let stack_idx = match &frame.regs[a] {
                            RuntimeValue::Ptr(VType::Struct(_, _), idx) => *idx,
                            other => panic!("StoreStructField: not a struct pointer: {:?}", other),
                        };

                        // Take the struct out of the stack temporarily
                        let mut s = match std::mem::replace(&mut self.stack[stack_idx], StackSlot::Empty) {
                            StackSlot::Struct(s) => s,
                            _ => panic!("StoreStructField: not a struct"),
                        };

                        let struct_proto_idx = self.struct_protos.len() - (s.struct_idx - 1);
                        let struct_proto = &self.struct_protos[struct_proto_idx];
                        let (_, proto_ty) = &struct_proto.fields[b];

                        let field_type = resolve_type(proto_ty, &s.concrete_type, &struct_proto.generics);
                        let value = cast_value(raw_value, &field_type, &mut self.stack);

                        s.fields[b] = value;
                        self.stack[stack_idx] = StackSlot::Struct(s);
                    }

                    x if x == Opcode::LoadStructField as u32 => {
                        let stack_idx = match &self.frames[top_idx].regs[b] {
                            RuntimeValue::Ptr(VType::Struct(..), idx) => *idx,
                            other => panic!("LoadStructField: not a struct pointer: {:?}", other),
                        };
                        // frame borrow ends here ^
                        let val = match self.stack.get(stack_idx) {
                            Some(StackSlot::Struct(s)) => {
                                if c >= s.fields.len() { panic!("LoadStructField: field OOB {}", c); }
                                s.fields[c].clone()
                            }
                            _ => panic!("LoadStructField: stack slot is not a struct"),
                        };
                        self.frames[top_idx].regs[a] = val;
                    }
                    x if x == Opcode::Move as u32 => {
                        if bx >= frame.regs.len() || a >= frame.regs.len() {
                            panic!("Move: reg OOB a={} bx={}", a, bx);
                        }
                        frame.regs[a] = frame.regs[bx].clone();
                    }
                    x if x == Opcode::Ret as u32 => {
                        let return_value = if a == 0 {
                            None
                        } else {
                            let ret_reg = a - 1;
                            if ret_reg >= frame.regs.len() {
                                panic!("Ret: return register OOB ret_reg={}", ret_reg);
                            }
                            Some(frame.regs[ret_reg].clone())
                        };
                        
                        self.pop_frame_and_propagate(return_value);
                    }
                    x if x == Opcode::Call as u32 => {
                        let num_args = bx;
                        if a >= frame.regs.len() {
                            panic!("Call: function register OOB a={}", a);
                        }
                        let func_slot = frame.regs[a].clone();
                        match func_slot {
                            RuntimeValue::Function(proto_idx) => {
                                let stack_base = self.stack.len();
                                let regs = vec![RuntimeValue::Empty; self.frame_regs];
                                let mut callee_frame = CallFrame { 
                                    proto_index: proto_idx,
                                    pc: 0, 
                                    regs, 
                                    stack_base,
                                    local_var_bases: Vec::new()
                                };

                                // Copy arguments to callee's registers
                                for i in 0..num_args {
                                    let src_idx = a + 1 + i;
                                    if src_idx >= frame.regs.len() {
                                        panic!("CALL: caller arg register OOB");
                                    }
                                    callee_frame.regs[i] = frame.regs[src_idx].clone();
                                }

                                self.frames.push(callee_frame);
                            }
                            RuntimeValue::NativeFunction(native_idx) => {
                                let native = self.native_functions.get(native_idx)
                                    .expect("Invalid native function index");

                                let mut args = Vec::with_capacity(num_args);
                                for i in 0..num_args {
                                    let arg_reg = a + 1 + i;
                                    if arg_reg >= frame.regs.len() {
                                        panic!("CALL (native): arg reg OOB");
                                    }
                                    args.push(frame.regs[arg_reg].clone());
                                }

                                let ret = (native.func)(&args);
                                frame.regs[a] = ret;
                            }
                            other => panic!("Call target not a function: {:?}//[{}]{:?}", other, a, frame.regs),
                        }
                    }
                    x if x == Opcode::Add as u32
                        || x == Opcode::Sub as u32
                        || x == Opcode::Mul as u32
                        || x == Opcode::Div as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Binary op: register OOB a={} b={} c={}", a, b, c);
                        }

                        let left = &frame.regs[b];
                        let right = &frame.regs[c];

                        let result = match opcode {
                            x if x == Opcode::Add as u32 => apply_binop!(left, right, +),
                            x if x == Opcode::Sub as u32 => apply_binop!(left, right, -),
                            x if x == Opcode::Mul as u32 => apply_binop!(left, right, *),
                            x if x == Opcode::Div as u32 => apply_binop!(left, right, /),
                            _ => unreachable!(),
                        };

                        frame.regs[a] = result;
                    }
                    x if x == Opcode::Eq as u32 => {
                        // A = target, B = left, C = right
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Eq: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => *l == *r,
                            (RuntimeValue::Str(l), RuntimeValue::Str(r)) => l == r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l == r,
                            (RuntimeValue::Empty, RuntimeValue::Empty) => true,
                            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => l == r,
                            // You can add more cross-type comparisons if needed
                            _ => false,
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Lt as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Lt: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => l < r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l < r,
                            // fallthrough / panic for invalid compares could be used instead
                            _ => panic!("Lt: unsupported operand types: {:?} {:?}", frame.regs[b], frame.regs[c]),
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Le as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("Le: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = match (&frame.regs[b], &frame.regs[c]) {
                            (RuntimeValue::I32(l), RuntimeValue::I32(r)) => l <= r,
                            (RuntimeValue::USize(l), RuntimeValue::USize(r)) => l <= r,
                            _ => panic!("Le: unsupported operand types: {:?} {:?}", frame.regs[b], frame.regs[c]),
                        };
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::LNot as u32 => {
                        // A = target, Bx encodes source register in this encoding; compiler uses pack_i_abx(Opcode::LNot, reg, reg)
                        // In your code you emitted LNot as pack_i_abx(Opcode::LNot, reg, reg) earlier.
                        // We'll read source from bx (same as how LoadConst reads).
                        let src = bx as usize;
                        if a >= frame.regs.len() || src >= frame.regs.len() {
                            panic!("LNot: register OOB a={} src={}", a, src);
                        }
                        let res = !truthy(&frame.regs[src]);
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::LAnd as u32 => {
                        // A = target, B = left, C = right
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("LAnd: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = truthy(&frame.regs[b]) && truthy(&frame.regs[c]);
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::LOr as u32 => {
                        if a >= frame.regs.len() || b >= frame.regs.len() || c >= frame.regs.len() {
                            panic!("LOr: register OOB a={} b={} c={}", a, b, c);
                        }
                        let res = truthy(&frame.regs[b]) || truthy(&frame.regs[c]);
                        frame.regs[a] = RuntimeValue::Bool(res);
                    }
                    x if x == Opcode::Jmp as u32 => {
                        // A = cond reg, Bx = target ip
                        let cond_reg = a;
                        let target = bx as usize;
                        if cond_reg >= frame.regs.len() {
                            panic!("Jmp: cond reg OOB a={}", cond_reg);
                        }
                        if truthy(&frame.regs[cond_reg]) {
                            frame.pc = target;
                        }
                    }
                    x if x == Opcode::Jn as u32 => {
                        // A = cond reg, Bx = target ip
                        let cond_reg = a;
                        let target = bx as usize;
                        if cond_reg >= frame.regs.len() {
                            panic!("Jn: cond reg OOB a={}", cond_reg);
                        }
                        if !truthy(&frame.regs[cond_reg]) {
                            frame.pc = target;
                        }
                    }
                    x if x == Opcode::BeginBlock as u32 => {
                        let current_stack_size = self.stack.len();
                        self.block_stack_bases.push(current_stack_size);
                        
                        let idx = self.frames.len() - 1;
                        let frame = &mut self.frames[idx];
                        
                        let mut local_base = frame.regs.len() - 1;
                        frame.local_var_bases.push(local_base);
                    }
                    
                    x if x == Opcode::EndBlock as u32 => {
                        if let Some(block_base) = self.block_stack_bases.pop() {
                            self.cleanup_stack(block_base);
                        } else {
                            panic!("EndBlock without matching BeginBlock");
                        }
                        
                        let idx = self.frames.len() - 1;
                        let frame = &mut self.frames[idx];
                        if let Some(local_base) = frame.local_var_bases.pop() {
                            for i in local_base..frame.regs.len() - 1 {
                                frame.regs[i] = RuntimeValue::Empty;
                            }
                        } else {
                            panic!("EndBlock without matching BeginBlock for locals");
                        }
                    }
                    x if x == Opcode::Cast as u32 => {
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("Cast: OOB bx={}", bx));
                        let target_type = if let ConstValue::Type(t) = const_val {
                            t
                        } else {
                            panic!("Cast: not a Type const at {}", bx);
                        };

                        match (&frame.regs[a].clone(), &target_type) {
                            (RuntimeValue::Ptr(VType::Array(_), stack_idx), VType::Array(to_elem)) => {
                                let stack_idx = *stack_idx;
                                let to_elem = *to_elem.clone();

                                // Take ownership of the elements vector
                                let old_elems = match std::mem::replace(
                                    &mut self.stack[stack_idx],
                                    StackSlot::Array(Vec::new()),
                                ) {
                                    StackSlot::Array(v) => v,
                                    _ => panic!("Ptr does not point to an array at stack index {}", stack_idx),
                                };

                                // Recursively cast each element
                                let new_elems: Vec<RuntimeValue> = old_elems
                                    .into_iter()
                                    .map(|v| cast_value(v, &to_elem, &mut self.stack))
                                    .collect();

                                // Put the array back into the stack
                                self.stack[stack_idx] = StackSlot::Array(new_elems);

                                frame.regs[a] = RuntimeValue::Ptr(target_type, stack_idx);
                            }
                            _ => {
                                let src = frame.regs[a].clone();
                                frame.regs[a] = cast_value(src, &target_type, &mut self.stack);
                            }
                        }
                    }

                    x if x == Opcode::NewArray as u32 => {
                        let const_val = proto.ctx.consts.get(bx)
                            .cloned()
                            .or_else(|| self.consts.get(bx).cloned())
                            .expect(&format!("NewArray: OOB bx={}", bx));

                        let elem_type = if let ConstValue::Array(elem) = const_val {
                            *elem
                        } else {
                            panic!("NewArray: constant at index {} is not Array, got {:?}", bx, const_val);
                        };

                        let stack_idx = self.stack.len();
                        self.stack.push(StackSlot::Array(Vec::new()));
                        frame.regs[a] = RuntimeValue::Ptr(VType::Array(Box::new(elem_type)), stack_idx);
                    }

                    x if x == Opcode::GetArrayIdx as u32 => {
                        let idx = match &frame.regs[c] {
                            RuntimeValue::USize(n) => *n,
                            RuntimeValue::I32(n) if *n >= 0 => *n as usize,
                            other => panic!("GetArrayIdx: bad index: {:?}", other),
                        };
                        let stack_idx = match &frame.regs[b] {
                            RuntimeValue::Ptr(VType::Array(..), i) => *i,
                            other => panic!("GetArrayIdx: not an array ptr: {:?}", other),
                        };
                        let val = match self.stack.get(stack_idx) {
                            Some(StackSlot::Array(elems)) => {
                                if idx >= elems.len() { panic!("GetArrayIdx: OOB {} len={}", idx, elems.len()); }
                                elems[idx].clone()
                            }
                            _ => panic!("GetArrayIdx: stack slot is not an array"),
                        };
                        frame.regs[a] = val;
                    }

                    x if x == Opcode::SetArrayIdx as u32 => {
                        let idx = match &frame.regs[b] {
                            RuntimeValue::USize(n) => *n,
                            RuntimeValue::I32(n) if *n >= 0 => *n as usize,
                            other => panic!("SetArrayIdx: bad index: {:?}", other),
                        };
                        let value = frame.regs[c].clone();
                        let stack_idx = match &frame.regs[a] {
                            RuntimeValue::Ptr(VType::Array(..), i) => *i,
                            other => panic!("SetArrayIdx: not an array ptr: {:?}", other),
                        };
                        match self.stack.get_mut(stack_idx) {
                            Some(StackSlot::Array(elems)) => {
                                if idx >= elems.len() {
                                    elems.push(value);
                                } else {
                                    elems[idx] = value;
                                }
                            }
                            _ => panic!("SetArrayIdx: stack slot is not an array"),
                        }
                    }
                    x if x == Opcode::ArrayLen as u32 => {
                        let stack_idx = match &frame.regs[a] {
                            RuntimeValue::Ptr(VType::Array(..), i) => *i,
                            other => panic!("ArrayLen: not an array ptr: {:?}", other),
                        };
                        let len = match self.stack.get(stack_idx) {
                            Some(StackSlot::Array(elems)) => elems.len(),
                            _ => panic!("ArrayLen: stack slot is not an array"),
                        };
                        frame.regs[bx] = RuntimeValue::USize(len);
                    }
                    _ => {
                        panic!("Unknown opcode {} in proto {}", opcode, frame.proto_index);
                    }
                }
            }
        }
    }

    fn push_frame(&mut self, proto_index: usize) -> usize {
        let stack_base = self.stack.len();  // Current stack size becomes this frame's base
        let regs = vec![RuntimeValue::Empty; self.frame_regs];
        let frame = CallFrame { 
            proto_index, 
            pc: 0, 
            regs, 
            stack_base,
            local_var_bases: Vec::new()
        };
        let len = self.frames.len();
        self.frames.push(frame);
        len
    }

    /// Pop the top frame and propagate its return value to the caller.
    /// Clean up all stack-allocated structs from this frame.
    fn pop_frame_and_propagate(&mut self, ret: Option<RuntimeValue>) {
        let callee = self.frames.pop().expect("pop_frame on empty stack");

        let is_top_level = self.frames.is_empty();

        // Clean up all structs allocated in this frame
        self.cleanup_stack(callee.stack_base);

        let return_val = if is_top_level {
            // No caller => no pointer check
            ret.or_else(|| callee.regs.get(0).cloned())
        } else {
            // Caller exists => run validity check
            ret.or_else(|| {
                match callee.regs.get(0) {
                    Some(RuntimeValue::Ptr(other, stack_idx)) => {
                        if *stack_idx > callee.stack_base {
                            println!("Warning: returning invalid struct pointer");
                            Some(RuntimeValue::Empty)
                        } else {
                            Some(RuntimeValue::Ptr(other.clone(), *stack_idx))
                        }
                    }
                    Some(other) => Some(other.clone()),
                    None => Some(RuntimeValue::Empty),
                }
            })
        };

        if let Some(caller) = self.frames.last_mut() {
            let callee_proto_idx = callee.proto_index;
            let target_reg = caller.regs.iter().position(|rv|
                matches!(rv, RuntimeValue::Function(idx) if *idx == callee_proto_idx)
            );

            if let Some(ridx) = target_reg {
                caller.regs[ridx] = return_val.unwrap_or(RuntimeValue::Empty);
            } else {
                caller.regs[0] = return_val.unwrap_or(RuntimeValue::Empty);
            }
        }
    }
}

fn truthy(rv: &RuntimeValue) -> bool {
    match rv {
        RuntimeValue::Bool(b) => *b,
        RuntimeValue::Empty => false,
        other => true
    }
}
