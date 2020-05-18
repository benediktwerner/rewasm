use std::rc::Rc;

use clap::{App, Arg};

use rewasm::analysis;
use rewasm::cfg::{Cfg, CfgBuildError};
use rewasm::fmt;
use rewasm::ssa;
use rewasm::structuring;
use rewasm::wasm;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args = App::new("rewasm")
        .version(VERSION)
        .arg(
            Arg::with_name("show-graph")
                .long("show-graph")
                .help("Print the constructed CFG in dot format before structuring"),
        )
        .arg(
            Arg::with_name("file")
                .help("The wasm binary to decompile")
                .required(true),
        )
        .arg(Arg::with_name("function").help("The index of the function to decompile"))
        .get_matches();

    let file_path = args.value_of("file").unwrap();
    let wasm = match wasm::Instance::from_file(file_path) {
        Ok(instance) => Rc::new(instance),
        Err(error) => {
            eprintln!("{}", error);
            return;
        }
    };

    let show_graph = args.is_present("show-graph");

    if let Some(func_index) = args.value_of("function") {
        let func_index = func_index.parse().unwrap();
        match decompile_func(wasm, func_index, show_graph) {
            Ok(()) => (),
            Err(CfgBuildError::NoSuchFunc) => eprintln!("No function with index {}", func_index),
            Err(CfgBuildError::FuncIsImported) => {
                eprintln!("Function {} is imported and can not be decompiled", func_index)
            }
        }
    } else {
        for (i, func) in wasm.module().functions().iter().enumerate() {
            if !func.is_imported() {
                eprintln!("Decompiling f{}", i);
                decompile_func(Rc::clone(&wasm), i as u32, show_graph).unwrap();
                println!();
            }
        }
    }
}

fn decompile_func(wasm: Rc<wasm::Instance>, func_index: u32, print_graph: bool) -> Result<(), CfgBuildError> {
    let mut cfg = Cfg::build(Rc::clone(&wasm), func_index)?;
    let mut def_use_map = ssa::transform_to_ssa(&mut cfg);

    analysis::propagate_expressions(&mut cfg, &mut def_use_map);
    analysis::eliminate_dead_code(&mut cfg, &mut def_use_map);

    ssa::transform_out_of_ssa(&mut cfg);

    if print_graph {
        println!("{}", cfg.dot_string());
    }

    let (decls, code) = structuring::structure(cfg);
    fmt::CodeWriter::printer(wasm, func_index).write_func(func_index, &decls, &code);
    Ok(())
}
