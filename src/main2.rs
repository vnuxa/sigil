// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

use std::{cell::RefCell, rc::Rc};

use bytecode::main::{FunctionPrototype, VM};
use index_vec::{IndexVec, index_vec};
// use bytecode::main::VM;
use interpreter::{
    codegen::{Block, Functions, Instruction, InstructionData, Module},
    parser::generate,
};
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

if test == 6{
    test = 1
    if test == 1 {
        test = 2
        if test == 2 {
            test = 3
        }
    }
    print(test)
    if test == 3 {
        test = 0
    }
}
print(test)



"
    .to_string();

    let syntax_tree = generate(input);
    let mut ir = Module::new();

    let mut functions: IndexVec<usize, Functions> = IndexVec::new();
    // functions.push();
    let mut first_function = Functions {
        name: None,
        // variables: IndexVec::new(),
        // instructions: IndexVec::new(),
        upvalues: Vec::new(),
        phi_instructions: IndexVec::new(),
        instructions: IndexVec::new(),
        blocks: Vec::new(),
        number_params: 0,
        definition_location: 0,
        predecessor_block: 0,
    };
    let mut highest_variable = 0;
    ir.variables.push(IndexVec::new());
    {
        let main_function = &mut first_function;
        ir.new_block(&mut main_function.blocks, None);

        let mut block_id = 0;
        let mut function_id = 0;
        for expression in &syntax_tree {
            println!("block id {:?}", block_id);
            // println!(
            //     "!!!!!!! PARSER OUTPUT {:?}",
            //     ir.build_instruction(&mut block, expression)
            // )
            let mut main_data = InstructionData::Unimplemented;
            let built_instruction = ir.build_instruction(
                // &mut main_function.variables,
                &mut main_function.instructions,
                &mut main_function.blocks,
                block_id,
                expression.clone(),
                &mut highest_variable,
                &mut |new_function| {
                    functions.push(new_function);
                    functions.len()
                }

            );
            println!("ok after adding the blocks for function 0 should be {:?}", main_function.blocks.len()

            );
            //     , &mut |id, data| {
            //     // ir.add_instruction(id, data, 0);
            //     main_id = id;
            //     main_data = data;
            //     // add instruction in main as the instructions that are within other functions are
            //     // being handled by the build instruction function within the functionblock cqase
            // });
            // IMPORTANT: make it so that it will not only post this instruction to the main function
            // maybe?
            // ir.add_instruction(main_id, main_data, 0);
            if let Some(new_block) = built_instruction {
                println!(
                    "current block id {:?} and then changing it to a new one {:?}",
                    block_id, new_block
                );
                block_id = new_block;
            }
        }
    }
    println!("ok after loop it now is {:?} and its index is {:?}", first_function.blocks.len(), functions.len());
    functions.push(first_function);
    println!("----------------------------------------------------------------- THE FIRST FUNCTIONS ITER WAS DONE");
    // let mut loop_functions = rc_functions.clone();
    // let main_function = &mut ir.functions[0];
    // 0 is main function
    // let mut block = &mut ir.blocks[0].clone();
    // let mut block = ir.blocks[0];
    // let mut programs = Vec::with_capacity(functions.len());
    let mut required_registers = 0;
    // let mut all_registers: IndexVec<usize, usize> = IndexVec::new();
    let mut all_registers: Vec<usize> = Vec::new();
    let mut previous_highest = 0;
    let mut protos = Vec::with_capacity(functions.len());
    for (index, main_function) in functions.iter_mut().enumerate().rev() {
        println!("============================== A FUNCTIONW AS DONE");
        // let mut functions = rc_functions.borrow_mut();
        // IMPORTANT: make this 0 into reflective to the current index of function
        // ir.blocks[block_id] = block;
        //
        // ir.blocks
        //     .last_mut()
        //     .unwrap()
        //     .instructions
        //     .push(interpreter::codegen::Instruction::EndOfFile);
        // ir.add_instruction(ir.blocks.last().unwrap().id, InstructionData::EndOfFile);


        // IMPORTANT: make this in reverse order and make it so taht instead of adding + 1 it would
        // just insert in the rpevious block a jumop instruction to the current block
        // these lbocks are joined together by being in the same function i believe
        //
        // this needs to be recursive
        // a list of where jumps should be inserted
        let mut jumplist = Vec::new();
        // a list of where returns should be isnerted
        // let mut endlist = Vec::new();
        'main: for scope in main_function.blocks.clone() {
            println!("in scope {:?}", scope.id);
            if let Some(scope_last) = scope.end {
                match main_function.instructions[scope_last].data {
                    // ignore
                    InstructionData::Jump(_) | InstructionData::Return(_) => {}

                    // NOTE: replace this with a jumpiftrue rather than this
                    // and use jumpiffalse bytecode afterward
                    InstructionData::JumpConditional(_, block_true, block_after) => {
                        for (origin, after) in &mut jumplist {
                            if *origin == scope.id {

                                *origin = block_after;
                                // after = origin;
                                // continue 'main;
                                break;
                            }
                        }
                        jumplist.push((block_true, block_after));
                        // if let Some() =  {
                        //
                        // }
                        // if ir.instructions[ir.blocks[block_after].en]{
                        //
                        // }
                        // ir.add_instruction(block_true, InstructionData::Jump(block_after), scope.function);
                    }

                    _ => {}
                    // add/remove instructions
                    // _ => {
                    // println!("LAST BLOCK WAS {:?} AND IT SHOULD JUMP TO {:?}", scope.previous_block, scope.id);
                    // if let Some(previous_block) = scope.previous_block {
                    //
                    // }
                    // ir.add_instruction(scope.id, InstructionData::Jump(scope.id + 1), scope.function);
                    // ir.insert_instruction(, reference, instruction_id);
                    // add jump instruction
                    // scope.instructions.push(InstructionData::Jump(scope.id + 1));
                    // }
                }
            } else {
                println!("LAST BLOCK WAS {:?} AND IT SHOULD JUMP TO {:?}", scope.previous_block, scope.id);

            }
        }
        for (origin, point) in jumplist {
            ir.add_instruction(
                &mut main_function.instructions,
                &mut main_function.blocks,
                origin,
                InstructionData::Jump(point),
                // main_function.blocks[origin].function
            );
        }
        // for scope in ir.blocks.clone() {
        //     if let Some(scope_last) = scope.end {
        //         match ir.instructions[scope_last].data {
        //             // ignore
        //             InstructionData::Jump(_) | InstructionData::JumpConditional(_, _, _) => {}
        //             InstructionData::EndOfFile => {}
        //             // add/remove instructions
        //             _ => {
        //                 ir.add_instruction(scope.id, InstructionData::Jump(scope.id + 1), scope.function);
        //                 // ir.insert_instruction(, reference, instruction_id);
        //                 // add jump instruction
        //                 // scope.instructions.push(InstructionData::Jump(scope.id + 1));
        //             }
        //         }
        //     } else {
        //         ir.add_instruction(scope.id, InstructionData::Jump(scope.id + 1), scope.function);
        //     }
        //
        //     // match scope.instructions.last() {
        //     //     // ignore
        //     //     Some(InstructionData::Jump(_) | InstructionData::JumpConditional(_, _, _)) => {}
        //     //     Some(InstructionData::EndOfFile) => {}
        //     //     // add/remove instructions
        //     //     Some(_) => {
        //     //         // add jump instruction
        //     //         scope.instructions.push(InstructionData::Jump(scope.id + 1));
        //     //     }
        //     //     None => {
        //     //         scope.instructions.push(InstructionData::Jump(scope.id + 1));
        //     //         // remove block
        //     //     }
        //     // }
        // }

        // TODO: probably not use clone?
        // ir.blocks[0] = block;
        for block in &main_function.blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            ir.for_block(block, &mut main_function.instructions, |instruction| {
                println!(
                    "      tjhe block {:?} instruction is {:?}",
                    instruction.id, instruction.data
                );
                // index += 1;
            });
            // println!("          succesors length {:?}", block.successors);
            // for predl in &block.predecessors {
            //     println!("      BLOCK {:?} HAS PREDECESSOR {:?}", block.id, predl);
            // }
            // for predl in &block.successors {
            //     println!("      BLOCK {:?} HAS SUCESSOR {:?}", block.id, predl);
            // }
        }

        println!("ok im done with that");
        let predecessors = ir.get_predecessors(&mut main_function.instructions, &mut main_function.blocks);
        println!("in function {:?}", index);
        let post_order = {

            let mut current_order = Vec::new(); //maybe make it wiht the capacity of all blocks?
            let mut visited = index_vec!(false; main_function.blocks.len());


            ir.post_order(
                main_function,
                0,
                // &main_function.blocks[0],
                // &mut main_function.blocks,
                // &mut main_function.instructions,
                &mut current_order,
                &mut visited
            );

            current_order
        };
        // let dominance = IndexVec::with_capacity(ir.blocks.len());
        // let mut dominance = index_vec![Vec::new(); ir.blocks.len()];
        let mut dominance = ir.build_dominance(&mut main_function.blocks, &predecessors, post_order.clone());
        println!("DOMINANCE: {:?}", dominance);
        // println!(
        //     "------------------------------------ OLD DOMINANCE: {:?}",
        //     old_dominance
        // );
        // let mut dominance = index_vec!(0, 0, 0);

        println!("post building dominance |||");
        let frontier = ir.dominance_frontiers(&mut main_function.blocks, &dominance, &predecessors);
        println!("post dominance frontier !! ");

        for obj in &frontier {
            println!("dommy {:?}", obj);
        }
        // for obj in dominance {
        //     println!("dominance: {:?}", obj);
        // }
        let tree = ir.make_dominance_tree(&mut main_function.blocks, dominance);
        ir.make_ssa(
            &mut main_function.phi_instructions,
            &mut main_function.instructions,
            &mut main_function.blocks,
            highest_variable,
            frontier,
            &tree,
            &predecessors
        );

        println!("[[[[[[[[[[[[[[[[[[[[[[[[[ IN TURN SSA IS");
        for phi in &main_function.phi_instructions {
            println!("          {:?} is a phi", phi);
        }

        ir.copy_propogation(
            &mut main_function.instructions,
            &mut main_function.blocks,
            &tree,
            &mut main_function.phi_instructions
        );
        ir.dead_copy_elimination(
            &mut main_function.instructions,
            &mut main_function.blocks,
            &main_function.phi_instructions
        );
        for block in &main_function.blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            ir.for_block(block, &mut main_function.instructions, |instruction| {
                println!(
                    "      tjhe block {:?} instruction is {:?}",
                    instruction.id, instruction.data
                );
            });
        }
        // println!("=======================================");
        // println!("NOW CSSA");
        println!("=======================================");

        // ir.convert_to_cssa(&predecessors);
        println!("||||");
        // for block in &ir.blocks {
        //     println!("im in block: {:?}", block.id);
        //     for (index, instruction) in block.instructions.iter().enumerate() {
        //         println!(
        //             "      tjhe block {:?} instruction is {:?}",
        //             index, instruction
        //         );
        //     }
        // }
        //
        for block in &main_function.blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            ir.for_block(block, &mut main_function.instructions, |instruction| {
                println!(
                    "      tjhe block {:?} instruction is {:?}",
                    instruction.id, instruction.data
                );
            });
        }

        for phi in &main_function.phi_instructions {
            println!("theres a phi instruction! {:?}", phi);
        }
        println!("||||||||||||||||||||||||||||||||||||||||||");
        println!("all of the instructions");
        println!("||||||||||||||||||||||||||||||||||||||||||");


        for test in &main_function.instructions {
            println!("      INSTRUCTION {:?} WITH DATA {:?}", test.id, test.data);

        }


        let mut instruction_indices = index_vec![0; main_function.instructions.len()];
        let instruction_order  = ir.instruction_post_order(&mut main_function.instructions, &main_function.blocks, &post_order);

        for (index, indice) in instruction_order.iter().enumerate() {
            instruction_indices[*indice] = index;
        }
        let liveness = ir.live_intervals(
            &mut main_function.instructions,
            &mut main_function.blocks,
            &main_function.phi_instructions,
            &post_order,
            &instruction_indices,
            IndexVec::new()
        );
        let (registers, highest) = ir.allocate_registers(
            &mut main_function.instructions,
            &main_function.phi_instructions,
            liveness,
            &instruction_order,
            &instruction_indices,
            &mut all_registers,
            0
            // previous_highest
        );

        required_registers += registers.len();

        // IMPORTANT: make base and number params and locals work!
        protos.push(FunctionPrototype {
            instructions: ir.compile_bytecode(
                &main_function.instructions,
                &main_function.blocks,
                &instruction_indices,
                instruction_order,
                &post_order,
                &registers
            ),
            internal_counter: 0,
            base: previous_highest,
            locals: Vec::new(),
            number_params: main_function.number_params,
        });
        previous_highest = highest + 1;

        // for intr in block.instructions {
        //     println!("testing: {:?}", intr);
        // }

        // println!("ir is {:?}",)
    }
    let mut virtual_machine = VM::new();

    // for (index, function) in functions.iter().enumerate() {
    // }
    for (index, proto) in protos.iter().enumerate() {
        println!("proto for function {:?} ahs base {:?}", index, proto.base);
    }
    virtual_machine.functions = protos;
    // virtual_machine.program = program;
    virtual_machine.registers = vec![bytecode::main::SigilTypes::Empty; required_registers];

    // panic!("not executing");
    virtual_machine.execute_function(0);

    for register in virtual_machine.registers {
        println!("value of register {:?}", register);
    }
    // println!("all protos: {:?}", protos)
}
