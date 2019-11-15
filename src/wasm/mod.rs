#![allow(dead_code)]

pub use parity_wasm::elements::{
    BlockType, CustomSection, ExportEntry, External, GlobalType, ImportEntry, Instruction, Internal, ResizableLimits,
    ValueType,
};
pub use parity_wasm::SerializationError;
pub use wasmi_validation::Error as ValidationError;

pub mod nan_preserving_float;
pub mod value;

pub use value::Value;

use std::fmt;
use std::iter::{self, FromIterator};

use failure::Fail;
use parity_wasm::elements as pwasm;

pub const PAGE_SIZE: u32 = 64 * 1024; // 64 KiB

#[derive(Debug, Fail)]
pub enum LoadError {
    #[fail(display = "Error while loading file: {}", _0)]
    SerializationError(#[fail(cause)] SerializationError),
    #[fail(display = "Error while validating file: {}", _0)]
    ValidationError(#[fail(cause)] ValidationError),
}

#[derive(Clone)]
pub struct FunctionType {
    type_ref: u32,
    params: Vec<ValueType>,
    return_type: Option<ValueType>,
}

impl FunctionType {
    fn new(type_ref: u32, func_type: &pwasm::FunctionType) -> Self {
        FunctionType {
            type_ref,
            params: Vec::from(func_type.params()),
            return_type: func_type.return_type(),
        }
    }
    pub const fn type_ref(&self) -> u32 {
        self.type_ref
    }
    pub fn params(&self) -> &[ValueType] {
        &self.params
    }
    pub fn param_count(&self) -> u32 {
        self.params.len() as u32
    }
    pub const fn return_type(&self) -> Option<ValueType> {
        self.return_type
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let return_type = match self.return_type {
            Some(return_type) => return_type.to_string(),
            None => String::from("()"),
        };
        write!(f, "({}) -> {}", params, return_type)
    }
}

pub struct Function {
    name: String,
    func_type: FunctionType,
    is_imported: bool,
    locals: Vec<ValueType>,
    instructions: Vec<Instruction>,
}

impl Function {
    const fn new(
        name: String,
        func_type: FunctionType,
        locals: Vec<ValueType>,
        instructions: Vec<Instruction>,
    ) -> Self {
        Function {
            name,
            func_type,
            is_imported: false,
            locals,
            instructions,
        }
    }

    const fn new_imported(name: String, func_type: FunctionType) -> Self {
        Function {
            name,
            func_type,
            is_imported: true,
            locals: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub const fn func_type(&self) -> &FunctionType {
        &self.func_type
    }
    pub const fn type_ref(&self) -> u32 {
        self.func_type.type_ref()
    }
    pub fn params(&self) -> &[ValueType] {
        self.func_type.params()
    }
    pub fn param_count(&self) -> u32 {
        self.func_type.param_count()
    }
    pub const fn return_type(&self) -> Option<ValueType> {
        self.func_type().return_type()
    }
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub fn locals(&self) -> &[ValueType] {
        &self.locals
    }
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}{}", self.name, self.func_type)
    }
}

pub enum InitExpr {
    Const(Value),
    Global(u32),
}

#[allow(clippy::fallible_impl_from)]
impl From<&pwasm::InitExpr> for InitExpr {
    fn from(init_expr: &pwasm::InitExpr) -> Self {
        let instrs = init_expr.code();
        assert!(instrs.len() == 2, "Init expr has invalid length: {}", instrs.len());
        assert!(instrs[1] == Instruction::End, "Init expr has multiple instructions");
        match &instrs[0] {
            Instruction::I32Const(val) => InitExpr::Const((*val).into()),
            Instruction::I64Const(val) => InitExpr::Const((*val).into()),
            Instruction::F32Const(val) => InitExpr::Const((*val).into()),
            Instruction::F64Const(val) => InitExpr::Const((*val).into()),
            Instruction::GetGlobal(index) => InitExpr::Global(*index),
            other => panic!("Invalid instruction in init expr: {}", other),
        }
    }
}

pub struct Global {
    name: String,
    is_imported: bool,
    is_mutable: bool,
    value_type: ValueType,
    init_expr: InitExpr,
}

impl Global {
    fn from_parity(name: String, global: &pwasm::GlobalEntry) -> Self {
        let global_type = global.global_type();
        Global {
            name,
            is_imported: false,
            is_mutable: global_type.is_mutable(),
            value_type: global_type.content_type(),
            init_expr: global.init_expr().into(),
        }
    }
    fn from_import(name: String, index: u32, global_type: pwasm::GlobalType) -> Self {
        Global {
            name,
            is_imported: true,
            is_mutable: global_type.is_mutable(),
            value_type: global_type.content_type(),
            init_expr: InitExpr::Global(index),
        }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub const fn is_imported(&self) -> bool {
        self.is_imported
    }
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable
    }
    pub const fn value_type(&self) -> ValueType {
        self.value_type
    }
    pub const fn init_expr(&self) -> &InitExpr {
        &self.init_expr
    }
}

#[derive(Clone)]
pub enum TableElement {
    Null,
    Func(u32),
}

impl Default for TableElement {
    fn default() -> Self {
        TableElement::Null
    }
}

pub struct Table {
    elements: Vec<TableElement>,
}

impl Table {
    fn new(size: u32) -> Self {
        Table {
            elements: vec![TableElement::Null; size as usize],
        }
    }

    pub fn elements(&self) -> &[TableElement] {
        &self.elements
    }
}

pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    fn new(size: u32) -> Self {
        Memory {
            data: vec![0; (size * PAGE_SIZE) as usize],
        }
    }
}

pub struct Module {
    types: Vec<FunctionType>,
    functions: Vec<Function>,
    globals: Vec<Global>,
    tables: Vec<Table>,
    memories: Vec<Memory>,
    start_func: Option<u32>,
    custom_sections: Vec<CustomSection>,
}

impl Module {
    pub fn from_file(file_path: &str) -> Result<Self, LoadError> {
        match parity_wasm::deserialize_file(file_path) {
            Ok(module) => {
                if let Err(error) = wasmi_validation::validate_module::<wasmi_validation::PlainValidator>(&module) {
                    return Err(LoadError::ValidationError(error));
                }
                Ok(Module::from_parity_module(module))
            }
            Err(error) => Err(LoadError::SerializationError(error)),
        }
    }

    fn from_parity_module(module: pwasm::Module) -> Self {
        // TODO: What happens when multiple functions have the same name?
        let module = match module.parse_names() {
            Ok(module) => module,
            Err((_, module)) => module,
        };

        let types = get_types(&module);

        let mut globals = Vec::new();
        let mut functions = Vec::new();

        if let Some(import_sec) = module.import_section() {
            for entry in import_sec.entries() {
                let name = format!("{}.{}", entry.module(), entry.field());
                match entry.external() {
                    pwasm::External::Function(type_ref) => {
                        let func_type = types[*type_ref as usize].clone();
                        functions.push(Function::new_imported(name, func_type))
                    }
                    pwasm::External::Global(global_type) => {
                        globals.push(Global::from_import(name, globals.len() as u32, *global_type))
                    }
                    _ => eprintln!("Unsupported import: {:?}", entry),
                }
            }
        }

        if let Some(global_sec) = module.global_section() {
            for global in global_sec.entries() {
                let name = format!("g{}", globals.len());
                globals.push(Global::from_parity(name, global));
            }
        }

        if let Some(func_sec) = module.function_section() {
            let func_bodies = module.code_section().map(|sec| sec.bodies()).unwrap_or(&[]);
            for (type_ref, body) in func_sec.entries().iter().zip(func_bodies.iter()) {
                let type_ref = type_ref.type_ref();
                let name = format!("f{}", functions.len());
                let func_type = types[type_ref as usize].clone();
                let locals = body
                    .locals()
                    .iter()
                    .flat_map(|locals| iter::repeat(locals.value_type()).take(locals.count() as usize))
                    .collect();
                let instructions = body.code().elements().to_vec();
                functions.push(Function::new(name, func_type, locals, instructions));
            }
        }

        let tables = get_tables(&module);
        let memories = get_memories(&module);

        if let Some(export_sec) = module.export_section() {
            for export in export_sec.entries() {
                match export.internal() {
                    Internal::Function(index) => functions[*index as usize].name = export.field().to_string(),
                    Internal::Global(index) => globals[*index as usize].name = export.field().to_string(),
                    _ => (),
                }
            }
        }

        if let Some(name_sec) = module.names_section() {
            if let Some(func_names) = name_sec.functions() {
                for (i, name) in func_names.names() {
                    functions[i as usize].name = name.clone();
                }
            }
        }

        Module {
            types,
            functions,
            globals,
            tables,
            memories,
            start_func: module.start_section(),
            custom_sections: Vec::from_iter(module.custom_sections().cloned()),
        }
    }

    pub fn types(&self) -> &[FunctionType] {
        &self.types
    }
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
    pub fn func(&self, index: u32) -> &Function {
        &self.functions[index as usize]
    }
    pub fn get_func(&self, index: u32) -> Option<&Function> {
        self.functions.get(index as usize)
    }
    pub fn globals(&self) -> &[Global] {
        &self.globals
    }
    pub fn tables(&self) -> &[Table] {
        &self.tables
    }
    pub fn memories(&self) -> &[Memory] {
        &self.memories
    }
    pub const fn start_func(&self) -> Option<u32> {
        self.start_func
    }
    pub fn custom_sections(&self) -> &[CustomSection] {
        &self.custom_sections
    }
}

fn get_types(module: &pwasm::Module) -> Vec<FunctionType> {
    match module.type_section() {
        Some(type_sec) => type_sec
            .types()
            .iter()
            .enumerate()
            .map(|(i, t)| {
                let pwasm::Type::Function(func_type) = t;
                FunctionType::new(i as u32, func_type)
            })
            .collect(),
        None => Vec::new(),
    }
}

fn get_tables(module: &pwasm::Module) -> Vec<Table> {
    let mut tables = match module.table_section() {
        Some(table_sec) => table_sec
            .entries()
            .iter()
            .map(|table| Table::new(table.limits().initial()))
            .collect(),
        None => Vec::new(),
    };

    if let Some(element_sec) = module.elements_section() {
        for entry in element_sec.entries() {
            if let InitExpr::Const(val) = entry.offset().as_ref().unwrap().into() {
                let table = &mut tables[entry.index() as usize];
                let offset = val.to::<u32>().unwrap() as usize;
                for (i, member) in entry.members().iter().enumerate() {
                    let index = offset + i;
                    let new_ele = TableElement::Func(*member);
                    if index >= table.elements.len() {
                        table.elements.push(new_ele);
                    } else {
                        table.elements[index] = new_ele;
                    }
                }
            }
        }
    }
    tables
}

fn get_memories(module: &pwasm::Module) -> Vec<Memory> {
    let mut memories = match module.memory_section() {
        Some(memory_sec) => memory_sec
            .entries()
            .iter()
            .map(|memory| Memory::new(memory.limits().initial()))
            .collect(),
        None => Vec::new(),
    };

    if let Some(data_sec) = module.data_section() {
        for entry in data_sec.entries() {
            if let InitExpr::Const(val) = entry.offset().as_ref().unwrap().into() {
                // TODO: handle imported memory
                if let Some(memory) = memories.get_mut(entry.index() as usize) {
                    let offset = val.to::<u32>().unwrap() as usize;
                    if offset + entry.value().len() > memory.data.len() {
                        memory.data.resize(offset + entry.value().len(), 0);
                    }
                    memory.data[offset..offset + entry.value().len()].copy_from_slice(entry.value());
                }
            }
        }
    }
    memories
}
