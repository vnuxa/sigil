// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

use index_vec::{IndexVec, index_vec};
// use bytecode::main::VM;
use interpreter::{
    codegen::{Block, Module},
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
}

local asd = test + 5


"
    .to_string();

    let syntax_tree = generate(input);
    let mut ir = Module::new();
    ir.new_block();
    let mut block = ir.blocks[0].clone();
    for expression in syntax_tree {
        ir.build_instruction(&mut block, expression);
    }
    // TODO: probably not use clone?
    ir.blocks[0] = block;
    for block in &ir.blocks {
        println!("im in block: {:?}", block.id);
        for (index, instruction) in block.instructions.iter().enumerate() {
            println!(
                "      tjhe block {:?} instruction is {:?}",
                index, instruction
            );
        }
    }

    for block in &ir.blocks[0].successors {
        println!("BLOCK HAS SUCESSORS: {:?} ", block)
    }
    println!("the length of block: {:?}", ir.blocks.len());
    // let dominance = IndexVec::with_capacity(ir.blocks.len());
    let mut dominance = index_vec![Vec::new(); ir.blocks.len()];
    ir.build_dominance(&mut dominance);

    println!("post building dominance |||");
    let frontier = ir.dominance_frontiers(&dominance);
    println!("post dominance frontier !! ");

    for obj in &frontier {
        println!("dommy {:?}", obj);
    }
    // for obj in dominance {
    //     println!("dominance: {:?}", obj);
    // }
    ir.make_ssa(frontier, &dominance);

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
