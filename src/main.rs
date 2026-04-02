#![allow(unused)]

mod lexer;
mod compiler;
mod ast;
mod vm;
mod v_type;

use lexer::tokenize;
use ast::parse;
use compiler::{compile_stmt, RegAlloc, CompileCtx};
use vm::{RuntimeValue, VM};

use std::fs;
use std::env;
use std::ops::Index;
use std::process;

use crate::compiler::pack_i_abx;
use crate::compiler::ConstValue;
use crate::compiler::Opcode;
use crate::v_type::VType;
use crate::vm::NativeFunction;

fn decode_instruction_abx(instr: u32) -> (u32, usize, usize) {
    let opcode = instr & 0b111111;
    let a = ((instr >> 6) & 0xFF) as usize;
    let bx = (instr >> 14) as usize;
    (opcode, a, bx)
}

fn decode_instruction_abc(instr: u32) -> (u32, usize, usize, usize) {
    let opcode = instr & 0b111111;
    let a = ((instr >> 6) & 0xFF) as usize;
    // Assuming B_SHIFT and C_SHIFT are 14 and 23 (adjust if needed)
    let b = ((instr >> 14) & 0x1FF) as usize; // 9 bits for B
    let c = ((instr >> 23) & 0x1FF) as usize; // 9 bits for C
    (opcode, a, b, c)
}

fn dump_protos(ctx: &CompileCtx) {
    println!("=== ctx.consts ===");
    for (i, c) in ctx.consts.iter().enumerate() {
        println!("  [{}] {:?}", i, c);
    }

    for (pi, p) in ctx.protos.iter().enumerate() {
        println!("\n[{}] {}() =>", pi, p.name);
        println!("  proto.consts:");
        for (i, c) in p.ctx.consts.iter().enumerate() {
            println!("    [{}] {:?}", i, c);
        }
        println!("  code:");

        // Pass 1: find max length before comments
        let mut max_len = 0;
        let decoded: Vec<(String, String)> = p.code.iter().enumerate().map(|(ip, instr)| {
            let opcode = instr & 0b111111;
            let mut line = String::new();
            let comment: String;

            if matches!(opcode, x if x == Opcode::LoadConst as u32
                               || x == Opcode::Call as u32
                               || x == Opcode::Move as u32
                               || x == Opcode::Jmp as u32
                               || x == Opcode::Jn as u32
                               || x == Opcode::BeginBlock as u32
                               || x == Opcode::Ret as u32
                               || x == Opcode::EndBlock as u32
                               || x == Opcode::NewStruct as u32
                               || x == Opcode::ArrayLen as u32
                               || x == Opcode::NewArray as u32
                        )
            {
                let (opcode, a, bx) = decode_instruction_abx(*instr);
                line = format!("    {:04} {:08x} op={} A={} BX={}", ip, instr, opcode, a, bx);

                comment = if opcode == Opcode::LoadConst as u32 {
                    let from_proto = p.ctx.consts.get(bx).map(|v| format!("{:?}", v))
                        .unwrap_or("<none>".into());
                    format!("; LOAD-CONST ([{}] {})", bx, from_proto)
                }
                else if opcode == Opcode::NewStruct as u32 {
                    "; NEW-STRUCT".into()
                }
                else if opcode == Opcode::Call as u32 {
                    "; CALL-FN".into()
                }
                else if opcode == Opcode::Move as u32 {
                    "; MOV".into()
                }
                else if opcode == Opcode::Jmp as u32 {
                    "; JMP".into()
                }
                else if opcode == Opcode::Jn as u32 {
                    "; JN".into()
                }
                else if opcode == Opcode::Ret as u32 {
                    "; RET".into()
                }
                else if opcode == Opcode::BeginBlock as u32 {
                    "; BEGIN-BLOCK".into()
                }
                else if opcode == Opcode::EndBlock as u32 {
                    "; END-BLOCK".into()
                }
                else if opcode == Opcode::ArrayLen as u32 {
                    "; ARRAY-LEN".into()
                }
                else if opcode == Opcode::NewArray as u32 {
                    "; NEW-ARRAY".into()
                }
                else {
                    "; OP-CODE(2wide)".into()
                };
            }
            else if matches!(opcode, x if x == Opcode::Add as u32
                                       || x == Opcode::Sub as u32
                                       || x == Opcode::Mul as u32
                                       || x == Opcode::Div as u32
                                       || x == Opcode::Eq as u32
                                       || x == Opcode::Le as u32
                                       || x == Opcode::Lt as u32
                                       || x == Opcode::StoreStructField as u32
                                       || x == Opcode::LoadStructField as u32
                                       || x == Opcode::SetArrayIdx as u32
                                       || x == Opcode::GetArrayIdx as u32
                            )
            {
                let (opcode, a, b, c) = decode_instruction_abc(*instr);
                line = format!("    {:04} {:08x} op={} A={} B={} C={}", ip, instr, opcode, a, b, c);

                comment = if opcode == Opcode::StoreStructField as u32 {
                    "; STORE-STRUCT-FIELD".into()
                }
                else if opcode == Opcode::LoadStructField as u32 {
                    "; LOAD-STRUCT-FIELD".into()
                }
                else if opcode == Opcode::Add as u32 {
                    "; ADD".into()
                }
                else if opcode == Opcode::Sub as u32 {
                    "; SUB".into()
                }
                else if opcode == Opcode::Mul as u32 {
                    "; MUL".into()
                }
                else if opcode == Opcode::Div as u32 {
                    "; DIV".into()
                }
                else if opcode == Opcode::Eq as u32 {
                    "; EQ".into()
                }
                else if opcode == Opcode::Le as u32 {
                    "; LE".into()
                }
                else if opcode == Opcode::Lt as u32 {
                    "; LT".into()
                }
                else if opcode == Opcode::SetArrayIdx as u32 {
                    "; ARR-SET".into()
                }
                else if opcode == Opcode::GetArrayIdx as u32 {
                    "; ARR-GET".into()
                }
                else if opcode == Opcode::Lt as u32 {
                    "; LT".into()
                }
                else {
                    "; OP-CODE(3wide)".into()
                };
            }
            else {
                line = format!("    {:04} {:08x} op={} (unknown format)", ip, instr, opcode);
                comment = String::new();
            }

            max_len = max_len.max(line.len());
            (line, comment)
        }).collect();

        // Pass 2: print with aligned comments
        for (line, comment) in decoded {
            if comment.is_empty() {
                println!("{}", line);
            } else {
                println!("{:<width$} {}", line, comment, width = max_len + 2);
            }
        }
    }
}

fn main() {
    use std::{fs, process};
    let contents = match fs::read_to_string("./input.ath") {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", "./input.ath", e);
            process::exit(1);
        }
    };

    let tokens = tokenize(contents);
    let ast = parse(tokens).unwrap();
    println!("{:#?}", ast);
    
    let mut ctx = CompileCtx::new();
    let native_fns: Vec<NativeFunction> = vec![
        NativeFunction {
            name: "print".into(),
            func: |args| {
                println!("{:?}", args[0]);
                RuntimeValue::Empty
            }
        }
    ];
    
    ctx.register_native_fns(&native_fns);
    
    let mut code = Vec::new();
    compile_stmt(ast, &mut ctx, &mut code);
    dump_protos(&ctx);
    let main_proto_index = ctx.find_fn_addr_by_name("main")
        .expect("main function not found");

    let mut vm = VM::new(native_fns, ctx.protos.clone(), ctx.structs, ctx.consts);
    vm.run_from_proto(main_proto_index);
}