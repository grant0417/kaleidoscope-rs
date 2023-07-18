use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    context::Context,
    passes::PassManager,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};
use llvm_sys_150::support::LLVMAddSymbol;
use rustyline::DefaultEditor;

use crate::{codegen::CodeGen, parser::Parser};

pub mod ast;
pub mod codegen;
pub mod lexer;
mod parser;

fn main() {
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    let pass_manager = PassManager::create(&module);

    pass_manager.add_instruction_combining_pass();
    pass_manager.add_reassociate_pass();
    pass_manager.add_gvn_pass();
    pass_manager.add_cfg_simplification_pass();
    pass_manager.add_basic_alias_analysis_pass();
    pass_manager.add_promote_memory_to_register_pass();
    pass_manager.add_instruction_combining_pass();
    pass_manager.add_reassociate_pass();

    pass_manager.initialize();

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let binary_op_precedence = Rc::new(RefCell::new(
        [('=', 2), ('<', 10), ('+', 20), ('-', 20), ('*', 40)]
            .into_iter()
            .collect(),
    ));

    let mut codegen = CodeGen {
        context: &context,
        module,
        builder,
        pass_manager,
        named_values: HashMap::new(),
        execution_engine: ee,
        binary_op_precedence,
    };

    unsafe {
        LLVMAddSymbol(
            std::ffi::CStr::from_bytes_with_nul_unchecked(b"printd\0").as_ptr(),
            printd as *mut std::ffi::c_void,
        )
    };

    unsafe {
        LLVMAddSymbol(
            std::ffi::CStr::from_bytes_with_nul_unchecked(b"putchard\0").as_ptr(),
            putchard as *mut std::ffi::c_void,
        )
    };

    let arg1 = std::env::args().nth(1);

    if let Some(arg1) = arg1 {
        let file_contents = std::fs::read_to_string(arg1).unwrap();

        let parse = Parser::new(&file_contents, codegen.binary_op_precedence.clone());
        for expr in parse {
            match expr {
                Ok(expr) => {
                    // println!("Node: {expr}");
                    if let Err(err) = expr.codegen(&mut codegen, false) {
                        println!("Error: {err}");
                        break;
                    }
                }
                Err(err) => println!("Error: {err:?}"),
            }
        }

        Target::initialize_all(&InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: false,
            info: true,
            machine_code: true,
        });

        let triple = TargetMachine::get_default_triple();
        let native_target = Target::from_triple(&triple).unwrap();

        let target_machine = native_target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        codegen.module.set_triple(&target_machine.get_triple());
        codegen
            .module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());

        target_machine
            .write_to_file(
                &codegen.module,
                inkwell::targets::FileType::Object,
                std::path::Path::new("output.o"),
            )
            .unwrap();
    } else {
        loop {
            let mut rl = DefaultEditor::with_config(
                rustyline::Config::builder()
                    .auto_add_history(true)
                    .history_ignore_space(true)
                    .build(),
            )
            .unwrap();
            let readline = rl.readline("> ");
            match readline {
                Ok(line) => {
                    let parse = Parser::new(&line, codegen.binary_op_precedence.clone());
                    for expr in parse {
                        match expr {
                            Ok(expr) => {
                                // println!("Node: {expr}");
                                if let Err(err) = expr.codegen(&mut codegen, true) {
                                    println!("Error: {err}");
                                    break;
                                }
                            }
                            Err(err) => println!("Error: {err:?}"),
                        }
                    }
                }
                Err(_) => {
                    println!("EXIT");
                    break;
                }
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print!("{}", x as u8 as char);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 1] = [printd];
