// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

use index_vec::{IndexVec, index_vec};
// use bytecode::main::VM;
use interpreter::{
    codegen::{Block, Instruction, Module},
    parser::generate,
};
//
// mod bytecode;
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

local test = 2 + 4 + 9
if test == 6 {
    test = 1
    if test == 1 {
        test = 2
    }
    if test == 9 {
        test = 8
    }
}
local asd = test + 5


"
    .to_string();

    let syntax_tree = generate(input);
    let mut ir = Module::new();
    ir.new_block();
    // let mut block = &mut ir.blocks[0];
    let mut block = ir.blocks[0].clone();
    let mut block_id = 0;
    for expression in syntax_tree {
        println!("block id {:?}", block_id);
        // println!(
        //     "!!!!!!! PARSER OUTPUT {:?}",
        //     ir.build_instruction(&mut block, expression)
        // )
        if let Some(new_block) = ir.build_instruction(&mut block, expression) {
            // if new_block.len {
            //
            // }
            // if let Some(Instruction::Jump) = block.instructions.last() {
            //
            // }
            println!(
                "current block id {:?} and then changing it to a new one {:?}",
                block_id, new_block
            );
            println!(
                "!!? instructions: {:?}",
                ir.blocks[if block_id == 0 { 0 } else { block_id - 1 }].instructions
            );
            // block.instructions);

            // if block_id > 0 {
            //     if ir.blocks[block_id - 1].instructions.is_empty() {
            //         println!(
            //             "|||||||||||||||||||||||||||//////////////////////// IT WAS EMPTY! {:?}",
            //             new_block
            //         );
            //         ir.blocks.remove(block_id - 1);
            //         continue;
            //     }
            // }
            // block_id = new_block;
            // if !block.instructions.is_empty() {
            ir.blocks[block_id] = block;
            block_id = new_block;
            block = ir.blocks[new_block].clone();
            // }
        }
    }
    ir.blocks[block_id] = block;

    ir.blocks
        .last_mut()
        .unwrap()
        .instructions
        .push(interpreter::codegen::Instruction::EndOfFile);

    for scope in &mut ir.blocks {
        match scope.instructions.last() {
            // ignore
            Some(Instruction::Jump(_) | Instruction::JumpConditional(_, _, _)) => {}
            Some(Instruction::EndOfFile) => {}
            // add/remove instructions
            Some(_) => {
                // add jump instruction
                scope.instructions.push(Instruction::Jump(scope.id + 1));
            }
            None => {
                scope.instructions.push(Instruction::Jump(scope.id + 1));
                // remove block
            }
        }
    }

    // TODO: probably not use clone?
    // ir.blocks[0] = block;
    for block in &ir.blocks {
        println!("im in block: {:?}", block.id);
        for (index, instruction) in block.instructions.iter().enumerate() {
            println!(
                "      tjhe block {:?} instruction is {:?}",
                index, instruction
            );
        }
        // println!("          succesors length {:?}", block.successors);
        // for predl in &block.predecessors {
        //     println!("      BLOCK {:?} HAS PREDECESSOR {:?}", block.id, predl);
        // }
        // for predl in &block.successors {
        //     println!("      BLOCK {:?} HAS SUCESSOR {:?}", block.id, predl);
        // }
    }

    let predecessors = ir.get_predecessors();
    // let dominance = IndexVec::with_capacity(ir.blocks.len());
    // let mut dominance = index_vec![Vec::new(); ir.blocks.len()];
    let mut dominance = ir.build_dominance(&predecessors);
    println!("DOMINANCE: {:?}", dominance);
    // println!(
    //     "------------------------------------ OLD DOMINANCE: {:?}",
    //     old_dominance
    // );
    // let mut dominance = index_vec!(0, 0, 0);

    println!("post building dominance |||");
    let frontier = ir.dominance_frontiers(&dominance, &predecessors);
    println!("post dominance frontier !! ");

    for obj in &frontier {
        println!("dommy {:?}", obj);
    }
    // for obj in dominance {
    //     println!("dominance: {:?}", obj);
    // }
    let tree = ir.make_dominance_tree(dominance);
    ir.make_ssa(frontier, &tree, &predecessors);

    println!("||||");
    for block in &ir.blocks {
        println!("im in block: {:?}", block.id);
        for (index, instruction) in block.instructions.iter().enumerate() {
            println!(
                "      tjhe block {:?} instruction is {:?}",
                index, instruction
            );
        }
    }

    for phi in ir.phi_instructions {
        println!("theres a phi instruction! {:?}", phi);
    }
    // for intr in block.instructions {
    //     println!("testing: {:?}", intr);
    // }

    // println!("ir is {:?}",)
}
