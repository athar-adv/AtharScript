//main.rs

mod parser;
mod compiler;
mod lexer;
mod vm;
mod bytecode_debug;

mod ty;
mod operator;

use std::env;
use std::fs;

use parser::{LucyParser};
use compiler::{LucyCompiler};
use vm::{RuntimeValue, LucyVM, Closure};

fn main()
{
    let mut cli_args = env::args();
    cli_args.next(); // Skip main file name

    let input_file_path = cli_args.next().unwrap();
    let source = fs::read_to_string(input_file_path).unwrap();

    let tokens = lexer::tokenize(source);
    let mut parser = LucyParser::new(tokens);

    let program_ast = parser.parse_file_source();
    println!("{:#?}", program_ast);

    let mut compiler = LucyCompiler::new();

    compiler.lulib_openlib("@std/io", |ns| ns
        .function("println", 1, |args| {
            println!("{:?}", args[0]);
            RuntimeValue::Empty
        })
    );
    
    compiler.compile(&program_ast);

    bytecode_debug::dump_bytecode(&compiler);

    let mut vm = LucyVM::new();
    for np in compiler.native_protos.drain(..) {
        vm.native_protos.push(np);
    }
    
    let main_proto  = compiler.proto_stack.pop().expect("no proto after compilation");
    let main_idx    = vm.load_proto(main_proto);
    
    let module_closure = Closure { proto_idx: main_idx, upvalues: vec![] };

    vm.call_closure(module_closure, vec![]);
}