
#[derive(Debug, Clone)]
pub enum VType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I64,
    U64,
    I32,
    F32,
    F64,
    USize,
    Empty,
    Bool,
    String,
    Unresolved(String),
    Inferred,
    Struct(String, Vec<VType>), // Proto name, instantiated generics
    Function(Vec<VType>, Vec<VType>, Box<VType>), // Generics, params, return
    Generic(String), // name
    Type(Box<VType>),
    Ref(
        Box<VType>
    ),
    Array(
        Box<VType>,
    ),
    Namespace,
    Auto
}

pub fn is_vtype(name: &str) -> bool {
    matches!(name,
    |   "u8"
    |   "i8"
    |   "u16"
    |   "i16"
    |   "i32"
    |   "u32"
    |   "i64"
    |   "u64"
    |   "f32"
    |   "f64"
    |   "empty"
    |   "function"
    |   "boolean"
    |   "string"
    |   "usize"
    |   "int"
    |   "array"
    |   "auto"
    )
}

pub fn vty_from_str(name: &str) -> Result<VType, String> {
    match name {
        "u8" => Ok(VType::U8),
        "i8" => Ok(VType::I8),
        "u16" => Ok(VType::U16),
        "i16" => Ok(VType::I16),
        "i32" => Ok(VType::I32),
        "u32" => Ok(VType::U32),
        "i64" => Ok(VType::I64),
        "u64" => Ok(VType::U64),
        "f32" => Ok(VType::F32),
        "f64" => Ok(VType::F64),
        "empty" => Ok(VType::Empty),
        "fun" => Ok(VType::Function(vec![], vec![], Box::new(VType::Empty))),
        "boolean" => Ok(VType::Bool),
        "string" => Ok(VType::String),
        "usize" => Ok(VType::USize),
        "int" => Ok(VType::I32),
        "array" => Ok(VType::Array(Box::new(VType::Empty))),
        "auto" => Ok(VType::Auto),
        _ => Err("invalid enum name".to_string())
    }
}

pub fn sizeof(v: VType) -> usize {
    match v {
        VType::U8 => 1,
        VType::I8 => 1,
        VType::U16 => 2,
        VType::I16 => 2,
        _ => 1
    }
}

impl PartialEq for VType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VType::Array(a_ty), VType::Array(b_ty)) => a_ty == b_ty,
            (VType::Ref(a), VType::Ref(b)) => a == b,
            (VType::Struct(a, generics_a), VType::Struct(b, generics_b))
                => a == b && generics_a == generics_b,
            (VType::U8, VType::U8) => true,
            (VType::I8, VType::I8) => true,
            (VType::U16, VType::U16) => true,
            (VType::I16, VType::I16) => true,
            (VType::U32, VType::U32) => true,
            (VType::I32, VType::I32) => true,
            (VType::F32, VType::F32) => true,
            (VType::I64, VType::I64) => true,
            (VType::U64, VType::U64) => true,
            (VType::F64, VType::F64) => true,
            (VType::USize, VType::USize) => true,
            (VType::Function(..), VType::Function(..)) => true,
            (VType::Empty, VType::Empty) => true,
            (VType::Bool, VType::Bool) => true,
            (VType::String, VType::String) => true,
            _ => false,
        }
    }
}