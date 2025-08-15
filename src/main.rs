// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

use std::{cell::RefCell, rc::Rc};

use bytecode::main::{FunctionPrototype, VM, CallFrame};
use index_vec::{IndexVec, index_vec};
// use bytecode::main::VM;
use interpreter::{
    parser::generate,
};

use crate::interpreter::codegen2::{BlockFrame, Functions, InstructionData, Module};
//
mod bytecode;
mod index_vec;
mod interpreter;
// mod vm;

// NOTE: this runs a repl for quick test
// maybe in the future i could support this feature
fn main() {
    // let mut language = VM::new();
    //
    // language.run();
    let input = "

local test = 2 + 4

fn test_func() {
    fn new_func() {
        test = 1
    }

    new_func()
    if test == 2 {
        test = 7
    }
}

test_func()
print(test)



"
    .to_string();

    // nwo the problem is in  the compield byteciode
    // in the compiuled bytecode whewn there si a jumpifueqal instruction it iwll nto work becausze
    // there is no other block to jumop to so bnecause of htat it will just move forwar5df and the
    // condiitioonal will be ignored and if there was a function it will not be follow3ed
    // i need to find a way how ot makle it so that i could make it where the program ocunter of
    // the jumpcifequal if iti wasnt equal wit qwikll be added so that this
    let syntax_tree = generate(input);
    let mut ir = Module::new();
    let mut first_function = Functions {
        name: None,
        frame: BlockFrame::new(0, None),
        upindex_amount: 0,
        definition_id: 0,
        id: 0,
        predecessor_block: None,
        predecessor_function: None,
    };
    ir.variables.push(IndexVec::new());
    ir.new_block(&mut first_function.frame.blocks, None);
    ir.functions.push(first_function);
    let mut block_id = 0;
    for expresion in syntax_tree {
        let mut first_frame = ir.functions[0].frame.clone();
        // let block = first_frame.blocks.len() - 1;
        if let Some(new_block) =  ir.build_expression(expresion, &mut first_frame, block_id) {
            println!("----- changing block id {:?} to {:?}", block_id, new_block);
            let last_new_block_instruction = first_frame.blocks[new_block - 1].end.unwrap();
            match first_frame.instructions[last_new_block_instruction].data {
                InstructionData::JumpConditional(_ ,_ ,_) | InstructionData::Jump(_) => {},
                _ => {
                    Module::add_instruction(&mut first_frame, new_block - 1, InstructionData::Jump(new_block));
                }
            }
            block_id = new_block;
        }
        ir.functions[0].frame = first_frame;
    }
    for function in &ir.functions {
        println!("func: {:?} ok so instructions for block: ALSO predecessor {:?}", function.id, function.predecessor_function);
        for block in &function.frame.blocks {
            let mut at = block.start;
            println!("ok block starts with: {:?}", at);
            while let Some(id) = at {
                let instruction = &function.frame.instructions[id];
                at = instruction.next;
                println!("      the {:?} instruction is {:?} in block {:?}", instruction.id, instruction.data, instruction.block);

            }
            println!("ok block ends with: {:?}", at);
        }
    }
    let mut frontiers = IndexVec::with_capacity(ir.functions.len());
    for function in &ir.functions {
        frontiers.push(index_vec![None; function.frame.blocks.len()]);
    }

    ir.dominance_frontier(&ir.functions[0].frame, 0, &mut frontiers);
    for (index, function) in frontiers.iter().enumerate() {
        println!("frontier function {:?}", index);
        for (block_index, block) in function.iter().enumerate() {
            println!("      function block: {:?} with value: {:?}", block_index, block);
        }
    }
    let mut first_frame = ir.functions[0].frame.clone();
    let ssa = ir.make_ssa(&mut first_frame, frontiers);
    for function in &ir.functions {
        println!(" funcc {:?} ok so instructions for block:", function.id);
        for block in &function.frame.blocks {
            let mut at = block.start;
            println!("ok block starts with: {:?} ends {:?}", at, block.end);
            while let Some(id) = at {
                let instruction = &function.frame.instructions[id];
                at = instruction.next;
                println!("      the {:?} instruction is {:?} in block {:?}", instruction.id, instruction.data, instruction.block);
            }
        }
    }
    // ir.functions[0].frame = first_frame;
    println!("---");
    for (index, map) in ssa.iter().enumerate() {
        println!("PHI {:?} MAP: ", index);
        for (function, instruction) in map {
            println!("      FUNCTION: {:?} INSTRUCTION ID {:?}", function, instruction);
        }
    }
    println!("ssa is {:?} big", ssa.len());

    let mut post_order = Vec::new();
    println!("way before post oder");
    ir.post_order(&ir.functions[0].frame, &0, 0, &mut post_order);
    let mut order_indices = index_vec![IndexVec::new(); ir.functions.len()];

    println!("----\n instructions in post order");
    for (index, (order_function, order_id)) in post_order.iter().enumerate() {
        let frame = &ir.functions[*order_function].frame;
        let instr = &frame.instructions[*order_id];
        println!("     index: {:?} function {:?} id: {:?}, data: {:?} || block {:?}", index, order_function, order_id, instr.data, instr.block)
    }
    println!("before");
    for (index, (indice_function, indice)) in post_order.iter().enumerate() {
        if order_indices[*indice_function].len() > *indice {
            println!("ok so capacity for it was: {:?}", order_indices[*indice_function].len());
            order_indices[*indice_function][*indice] = index;
        } else {
            order_indices[*indice_function].resize(*indice + 1, 0);
            order_indices[*indice_function][*indice] = index;
        }
    }
    println!("after");
    let liveness = ir.get_liveness(&ssa, &post_order, &order_indices);
    let register = ir.allocate_registers(&ssa, &post_order, &order_indices, liveness);


    // for (index, register) in register.iter().enumerate() {
        // let (order_function, order_id) = post_order[index];
        // let frame = &ir.functions[order_function].frame;
        // let instr = &frame.instructions[order_id];
        // println!("    index is {:?}, function id {:?} block id {:?}, data: {:?} || target register: {:?}", index, order_function, instr.id, instr.data, register);
        // println!("     index: {:?} function {:?} id: {:?}, data: {:?} || block {:?}", index, order_function, order_id, instr.data, instr.block)
    // }

        println!("register is {:?} ", register);
    for (index, (order_function, order_id)) in post_order.iter().enumerate() {
        let frame = &ir.functions[*order_function].frame;
        let instruction = &frame.instructions[*order_id];
        println!("the index is {:?} instruction data: {:?}, function id {:?}, instr id: {:?} || register {:?}", index, instruction.data, order_function, order_id, register[index]);
    }
    let bytecodes = ir.compile_bytecode(&post_order, &order_indices, &register);
    let mut virtual_machine = VM::new();
    let mut protos = Vec::new();

    for (function_index, bytecode) in bytecodes.iter().enumerate() {
        println!(".. when inserting function {:?} : {:?}", function_index, bytecode);
        // essentially the issue right now is that function 2 has no use thus it will not have any
        // instructions because the register end on them is fake and thus wont be allocated i
        // believe??
        protos.push(FunctionPrototype {
            // i might want to make it so that instructions will be in the vm not in each function
            // and a callframe manages everything mostly
            instructions: bytecode.to_vec(), // to_vec might nto be good
            locals: Vec::new(),
            number_params: 0, // NOTE: do this later
            //
            // base: 0, // IMPORTANT: bassicaly, change this base value
                     // the base just says the highest register (+ 1) for the previous function
        });
    }
    virtual_machine.functions = protos;

    virtual_machine.registers = vec![bytecode::main::SigilTypes::Empty; register.len()];
    let mut new_callframe = CallFrame::new(0, 0, 0);
    virtual_machine.execute_function(&mut new_callframe);
}
