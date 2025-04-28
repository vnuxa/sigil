// use crate::bytecode::main::Opcode;

use super::parser::Expressions;

// pub fn to_bytecode(expression: Expressions) -> Opcode {}

pub struct BlockData {
    first_phi: usize,         // usize temporarily
    first_instruction: usize, // usize temporarily
    last_instruction: usize,  // usize temporarily
}
