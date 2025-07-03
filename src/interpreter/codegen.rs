// use crate::bytecode::main::Opcode;

use std::{collections::HashMap, rc::Rc};

use chumsky::container::{Container, Seq};

use crate::{bytecode::main::{Opcode, SigilTypes}, index_vec::{index_vec, IndexVec}};

use super::parser::Expressions;

// pub fn to_bytecode(expression: Expressions) -> Opcode {}

#[derive(Clone, Debug)]
pub enum InstructionData {
    // Value(Value),
    LoadNumber(isize),
    LoadBool(bool),
    LoadFloat(f32), // might have to use f64?
    LoadString(String),

    MakeLocal(String), // name

    GetVariable(usize),        // variable id
    SetVariable(usize, SigilTypes), // variable id, value
    Jump(usize),               // Jumps to a general instruction id
    Unimplemented,             // IMPORTANT: delete this once done

    Add(usize, usize), // instruction id 1 and 2
    Subtract(usize, usize),
    Multiply(usize, usize),
    Divide(usize, usize),
    Remainder(usize, usize),
    Phi(usize), // id to a phi map, which contains a list of bloock id and instruction or variable id
    Copy(usize),
    CompareEqual(usize, usize),           // instruction id, instruction id
    JumpConditional(usize, usize, usize), // instruction id, jump if true, jump if false
    ParallelCopy(usize, usize),           // instruction_id, copy id
    Closure(usize, usize), // function id, variable_id
    Print(usize), // instruction id
    Return(Option<usize>), // returns a register?
    CallFunction(usize, Vec<usize>), // function id, vector of instruction ids as arguments!
    EndOfFile,
}

pub enum Value {
    String(String),
    Number(isize),
    Bool(bool),
}

// #[derive(Clone)]
// pub struct Block {
//     pub instructions: Vec<InstructionData>,
//     pub id: usize,
//     // pub successors: Vec<usize>,
//     // pub predecessors: Vec<usize>, // mgiht be useless?
// }

#[derive(Clone)]
pub struct Block {
    pub id: usize,
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub size: usize,
    // pub function: usize,
    pub previous_block: Option<usize>,
}

#[derive(Clone)]
pub struct Instruction {
    pub data: InstructionData,
    pub block: Option<usize>,
    pub next: Option<usize>,
    pub previous: Option<usize>,
    // pub function: usize,
    pub id: usize,
}

pub struct Module {
    // pub blocks: Vec<Block>,
    // pub blocks: Vec<Block>,
    // pub variables: IndexVec<usize, String>,
    // pub instructions: IndexVec<usize, Instruction>,
    pub variable_definitions: IndexVec<usize, Vec<usize>>, // variable index -  blocks that define it
    // pub phi_instructions: IndexVec<usize, Vec<(usize, usize)>>, // Vec<block_id, instruction_id>
                                                           // pub phi_instructions: IndexVec<usize, Vec<usize>>, // IndexVec<block_id, // Vec<isntruction_id>
    // pub functions: IndexVec<usize, Functions>, // (instruction_definition_id (remobed for now), Vec<upvalue_id>, scope)
    pub upvalues: IndexVec<usize, (usize, usize)> // (instruction_id, function_id (if its useful,
    // option<name>))
}

pub struct Functions {
    pub name: Option<String>,
    // pub instructions_start: usize,
    // pub instructions_end: usize,
    pub instructions: IndexVec<usize, Instruction>,
    pub phi_instructions: IndexVec<usize, Vec<(usize, usize)>>,
    pub blocks: Vec<Block>,

    pub variables: IndexVec<usize, String>,
    pub upvalues: Vec<usize>, // instruction id
}

impl Module {
    pub fn new() -> Self {
        Self {
            // blocks: Vec::new(),
            // blocks: Vec::new(),
            // variables: IndexVec::new(),
            variable_definitions: IndexVec::new(),
            // instructions: IndexVec::new(),
            // phi_instructions: IndexVec::new(),
            // functions: IndexVec::new(),
            upvalues: IndexVec::new(),
        }
    }
    pub fn new_block(&mut self, blocks: &mut Vec<Block>, current_block: Option<usize>) -> usize {
        let id = blocks.len();
        let block = Block {
            previous_block: current_block,
            // start: self.instructions.len(),
            start: None,
            end: None,
            size: 0,
            // function,
            id,
        };
        // let block = Block {
        //     instructions: Vec::new(),
        //     // successors: Vec::new(),
        //     // predecessors: Vec::new(),
        //     id,
        // };
        blocks.push(block);

        id
    }

    pub fn add_instruction(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        block_id: usize,
        instruction: InstructionData,
        // function: usize
    ) -> usize {
        let block = &mut blocks[block_id];
        let old_last = block.end;

        let id = instructions.len();
        if let Some(end_id) = old_last {
            instructions[end_id].next = Some(id);
            block.end = Some(id);
            block.size += 1;
        } else {
            block.start = Some(id);
            block.end = Some(id);
            block.size = 1;
        }

        instructions.push(Instruction {
            data: instruction,
            previous: old_last,
            next: None,
            block: Some(block_id),
            id: instructions.len(),
            // function
        });

        id
    }

    // creates a new instruction, without adding it
    pub fn new_instruction(&mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        instruction: InstructionData,
        // function: usize
    ) -> usize {
        let id = instructions.len();

        instructions.push(Instruction {
            data: instruction,
            previous: None,
            next: None,
            block: None,
            id,
            // function
        });

        id
    }

    pub fn remove_instruction(&mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        instruction_id: usize

    ) -> Option<usize> {
        println!("REMOVE INSTRUCTION WAS CALLED FOR {:?}", instruction_id);
        let instruction = &mut instructions[instruction_id];
        // let Some(instruction_block) = instruction.block else {
        //     return None;
        // };
        let block = &mut blocks[instruction.block?];

        let old_previous = instruction.previous;
        let old_next = instruction.next;
        instruction.previous = None;
        instruction.next = None;
        instruction.block = None;

        if let Some(previous) = old_previous {
            instructions[previous].next = old_next;
        } else {
            block.start = old_next;
        }

        if let Some(next) = old_next {
            instructions[next].previous = old_previous;
        } else {
            block.end = old_previous;
        }

        block.size -= 1;

        old_next
    }

    pub fn insert_instruction(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        block_id: usize,
        reference: Option<usize>,
        instruction_id: usize,
    ) {
        let block = &mut blocks[block_id];

        let (previous, next) = {
            if let Some(reference_id) = reference {
                (Some(reference_id), instructions[reference_id].next)
            } else {
                (None, block.start)
            }
        };

        let instruction = &mut instructions[instruction_id];

        instruction.block = Some(block_id);
        instruction.previous = previous;
        instruction.next = next;
        block.size += 1;

        if let Some(prev) = previous {
            instructions[prev].next = Some(instruction_id);
        } else {
            block.start = Some(instruction_id);
        }

        if let Some(next) = next {
            instructions[next].previous = Some(instruction_id);
        } else {
            block.end = Some(instruction_id);
        }
    }

    pub fn instruction_args<F: FnMut(usize)>(
        &self,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        instruction: &InstructionData,
        mut callback: F,
    ) {
        match instruction {
            InstructionData::Copy(data) => callback(*data),
            InstructionData::Print(data) => callback(*data),
            InstructionData::Phi(map_id) => {
                // let mut map = core::mem::take(&self.phi_instructions[map_id]);

                // let map = &mut module.phi_instructions[map_id];
                for (_, data) in &phi_instructions[*map_id] {
                    println!("              within phi \\/");
                    callback(*data);
                }
                // println!("ok data after: {:?}", map);

                // self.phi_instructions[map_id] = map;
            }
            InstructionData::Add(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }
            InstructionData::Subtract(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }
            InstructionData::Multiply(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }
            InstructionData::Divide(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }
            InstructionData::Remainder(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }
            InstructionData::CompareEqual(num_1, num_2) => {
                callback(*num_1);
                callback(*num_2);
            }

            InstructionData::JumpConditional(instruction, _, _) => callback(*instruction),

            // TODO: add more comparisons, return, calls, tuples, lists here too
            _ => {}
        }
    }
    pub fn replace_instruction<F: FnMut(&mut usize, &mut IndexVec<usize, Instruction>)>(
        &mut self,
        phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>,
        instructions: &mut IndexVec<usize, Instruction>,
        instruction: &mut InstructionData,
        mut callback: F,
    ) -> Option<InstructionData> {
        match instruction {
            InstructionData::Copy(data) =>  {  println!("copy before {:?}", data); callback(data, instructions); println!("copy should be {:?}", data); Some(InstructionData::Copy(*data))},
            InstructionData::Print(data) => { callback(data, instructions); Some(InstructionData::Print(*data)) },
            InstructionData::Phi(map_id) => {
                let mut map = core::mem::take(&mut phi_instructions[*map_id]);

                // let map = &mut module.phi_instructions[map_id];
                for (_, data) in &mut map {
                    println!("              within phi \\/");
                    callback(data, instructions);
                }
                println!("ok data after: {:?}", map);

                phi_instructions[*map_id] = map;

                Some(InstructionData::Phi(*map_id))
            }
            InstructionData::Add(num_1, num_2) => {
                callback(num_1, instructions);
                callback(num_2, instructions);

                Some(InstructionData::Add(*num_1, *num_2))
            }
            InstructionData::Subtract(num_1, num_2) => {
                callback(num_1, instructions);
                callback(num_2, instructions);

                Some(InstructionData::Subtract(*num_1, *num_2))
            }
            InstructionData::Multiply(num_1, num_2) => {
                callback(num_1, instructions);
                callback(num_2, instructions);

                Some(InstructionData::Multiply(*num_1, *num_2))
            }
            InstructionData::Divide(num_1, num_2) => {
                callback(num_1, instructions);
                callback(num_2, instructions);

                Some(InstructionData::Divide(*num_1, *num_2))
            }
            InstructionData::Remainder(num_1, num_2) => {
                callback(num_1, instructions);
                callback(num_2, instructions);

                Some(InstructionData::Remainder(*num_1, *num_2))
            }
            InstructionData::CompareEqual(num_1, num_2) => {
                println!("ok num1 before: {:?}", num_1);
                callback(num_1, instructions);
                println!("ok num1 after: {:?}", num_1);
                println!("ok num2 before: {:?}", num_1);
                callback(num_2, instructions);
                println!("ok num2 after: {:?}", num_1);

                Some(InstructionData::CompareEqual(*num_1, *num_2))
            }

            InstructionData::JumpConditional(instruction, v1, v2) => { callback(instruction, instructions); Some(InstructionData::JumpConditional(*instruction, *v1, *v2))},

            // TODO: add more comparisons, return, calls, tuples, lists here too
            e => {
                // unimplemented!("{:?}", e)
                None
            }
        }
    }

    // pub fn for_function<F: FnMut(&Module, &mut Functions)>(
    //     &mut self,
    //     // block: &Block,
    //     // instructions: &mut IndexVec<usize, Instruction>,
    //     // block_id: usize,
    //     mut callback: F
    // ) {
    //     for function in &mut self.functions {
    //         callback(self, function);
    //     }
    //     // let mut at = block.start;
    //     // while let Some(id) = at {
    //     //     let instruction = &instructions[id];
    //     //     callback(instruction);
    //     //
    //     //     at = instruction.next;
    //     // }
    // }
    pub fn for_block<F: FnMut(&Instruction)>(
        &self,
        block: &Block,
        instructions: &mut IndexVec<usize, Instruction>,
        // block_id: usize,
        mut callback: F
    ) {
        let mut at = block.start;
        while let Some(id) = at {
            let instruction = &instructions[id];
            callback(instruction);

            at = instruction.next;
        }
    }
    // for block loop with an early terminator
    pub fn for_block_terminator<F: FnMut(&Instruction) -> bool>(&self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &Vec<Block>,
        block_id: usize,
        mut callback: F
    ) {
        let mut at = blocks[block_id].start;
        while let Some(id) = at {
            let instruction = &instructions[id];
            if !callback(instruction) {
                break;
            }

            at = instruction.next;
        }
    }
    pub fn for_block_mut<F: FnMut(&mut Instruction)>(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        block_id: usize,
        mut callback: F
    ) {
        let mut at = blocks[block_id].start;
        while let Some(id) = at {
            let instruction = &mut instructions[id];
            callback(instruction);

            at = instruction.next;
        }
    }
    pub fn for_block_reverse<F: FnMut(&Instruction)>(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        // blocks: &mut Vec<Block>,
        block: &Block,
        mut callback: F
    ) {
        let mut at = block.end; // blocks[block_id].end;
        while let Some(id) = at {
            let instruction = &instructions[id];
            callback(instruction);

            at = instruction.previous;
        }
    }

    // loops over every instruction and runs a function for every single argument they have
    pub fn block_replace_args<F: FnMut(&mut usize, &mut IndexVec<usize, Instruction>)>(
        &mut self,
        block: &Block,
        // blocks: &mut Vec<Block>,
        instructions: &mut IndexVec<usize, Instruction>,
        phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>,
        // block_id: usize,
        mut callback: F,
    ) {
        let mut at = block.start;
        while let Some(id) = at {
            // TODO: might not have to clone?
            println!("replace instruction: at id {:?}", at);
            let mut data = instructions[id].data.clone();
            println!("before: {:?}", instructions[id].data);
            if let Some(new_data) =  self.replace_instruction(phi_instructions, instructions,  &mut data, &mut callback) {
                instructions[id].data = new_data;
            }
            // instructions[id].data = self.replace_instruction(phi_instructions, instructions,  &mut data, &mut callback);
            println!("ok after: {:?}", instructions[id].data);
            // callback(self, instruction.id);

            let instr = &mut instructions[id];
            // instr.data = data;
            at = instr.next;
            // at = self.instructions[id].next;
        }
    }

    pub fn all_args<F: FnMut(usize)>(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        blocks: &mut Vec<Block>,
        mut callback: F
    ) {
        for block in blocks {
            let mut at = block.start;
            while let Some(id) = at {
                let instruction = &instructions[id];
                self.instruction_args(phi_instructions, &instruction.data, &mut callback);

                at = instruction.next;
            }
        }
    }

    pub fn get_successors<F: FnMut(
        usize,
        &mut IndexVec<usize, Instruction>,
    )>(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        block: &Block,
        mut callback: F
    ) {
        // let block = &blocks[block_id];

        // let last_instruction = block.instructions.last().unwrap();

        match instructions[block.end.unwrap()].data {
            InstructionData::Jump(target) => callback(target, instructions),
            InstructionData::JumpConditional(_, target_1, target_2) => {
                callback(target_1, instructions);
                callback(target_2, instructions)
            }
            InstructionData::EndOfFile => {}
            // _ => unimplemented!("tried calling successors on an unterminated block"),
            _ => println!("unterminated block right here {:?}", block.id),
        }
    }

    pub fn get_predecessors(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,

    ) -> IndexVec<usize, IndexVec<usize, usize>> {
        let mut predecessors = index_vec!(IndexVec::new(); blocks.len());

        for block in blocks {
            self.get_successors(instructions, block, |successor, _| predecessors[successor].push(block.id));
        }

        predecessors
    }
    pub fn build_instruction<F: FnMut(Functions) -> usize>
    (
        &mut self,
        variables: &mut IndexVec<usize, String>,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        context_id: usize,
        expressions: Expressions,
        callback: &mut F
    ) -> Option<usize> {
        match expressions {
            Expressions::Int(num) => {
                self.add_instruction(instructions, blocks, context_id, InstructionData::LoadNumber(num));
            }
            Expressions::Bool(bool) => {
                self.add_instruction(instructions, blocks, context_id, InstructionData::LoadBool(bool));
            }
            Expressions::String(value) => {
                self.add_instruction(instructions, blocks, context_id, InstructionData::LoadString(value));
            }
            Expressions::Variable(value) => {
                self.add_instruction(
                    instructions,
                    blocks,
                    context_id,
                    InstructionData::GetVariable(variables.len() - 1),
                );
            }
            Expressions::LocalDefine(name, value) => {
                let Expressions::Variable(var_name) = *name else {
                    panic!()
                };

                // self.add_instruction(instructions, blocks, context_id, InstructionData::MakeLocal(var_name.clone()));
                // context.instructions.push();

                // IMPORTANT: make a way to add to the variable_defintions
                // and a way to track current block?
                if let Some(value) = value {
                    let _ = self.build_instruction(variables, instructions, blocks, context_id,*value, callback);
                    let context = &blocks[context_id];
                    variables.push(var_name);
                    self.add_instruction(instructions, blocks,
                        context.id,
                        InstructionData::SetVariable(
                            variables.len() - 1,
                            // context.instructions.len() - 1,
                            context.end.unwrap(),
                        ),
                    );
                };
            }
            Expressions::Assign(name, value) => {
                self.build_instruction(variables, instructions, blocks, context_id, *value, callback);
                let mut id = None;
                for (index, value) in variables.iter().enumerate() {
                    if name == *value {
                        id = Some(index);
                        break;
                    }
                }

                let context = &blocks[context_id];
                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::SetVariable(
                        id.unwrap(),
                        context.end.unwrap(),
                        // context.instructions.len(),
                    ),
                );
                // variables.push(name);
                // context.instructions.push(Instruction::SetVariable(
                //     id.unwrap(),
                //     context.instructions.len(),
                // ));
            }
            Expressions::Add(left_side, right_side) => {
                self.build_instruction(variables, instructions, blocks, context_id, *left_side, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *right_side, callback);
                let context = &blocks[context_id];
                // let length = context.instructions.len() - 1;

                // println!("CONTEXT LEN: {:?}", blocks[context.id].size);
                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::Add(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Subtract(left_side, right_side) => {
                self.build_instruction(variables,  instructions, blocks, context_id, *left_side, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *right_side, callback);
                let context = &blocks[context_id];

                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::Subtract(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),

                );
            }
            Expressions::Multiply(left_side, right_side) => {
                self.build_instruction(variables, instructions, blocks, context_id, *left_side, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *right_side, callback);

                let context = &blocks[context_id];
                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::Multiply(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),

                );
            }
            Expressions::Divide(left_side, right_side) => {
                self.build_instruction(variables, instructions, blocks, context_id, *left_side, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *right_side, callback);
                let context = &blocks[context_id];

                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::Divide(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),

                );
            }
            Expressions::Remainder(left_side, right_side) => {
                self.build_instruction(variables, instructions, blocks, context_id, *left_side, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *right_side, callback);
                let context = &blocks[context_id];

                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::Remainder(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                    // function_id
                );
            }
            Expressions::Scope(new_scope) => {
                println!("CONTEXT ID IS {:?}", context_id);
                let mut new_block_id =
                // if let None = blocks.last().unwrap().instructions.last() {
                //     blocks.len() - 1
                // } else {
                    self.new_block(blocks, Some(context_id));
                println!("AND NEW BLOCK ID IS {:?}", new_block_id);
                // };

                // println!("NEW SCOPE!!!!!!!!!!!!!");
                // let mut predecessors = context.predecessors.clone();
                // predecessors.push(context.id);
                // let new_block_id = self.new_block_with(context);
                // let mut new_block = blocks[new_block_id].clone();

                // println!(
                //     "the predecessors of that block is {:?}",
                //     new_block.predecessors
                // );
                // println!(
                //     "the successors of this block is {:?}",
                //     context.successors // blocks[context.id].successors
                // );
                // TODO: make ti so that this new block will only hjave direct predecessors
                // as currently, it iwll contain the asme predecors as previopus bloick and the
                // block aferr that and etc..
                // and make it so that there can be predecessoors of this new block too!
                // so that it doesnt always just use the same ol
                // let mut new_block = blocks[blocks.len() - 1].clone();
                // let index = new_block.id;
                let mut first_id = new_block_id;
                for expression in new_scope {
                    // let new_block = &mut blocks[new_block_id];
                    if let Some(obj) = self.build_instruction(variables, instructions, blocks, new_block_id, expression, callback) {
                        if blocks[blocks.len() - 1].size == 0 {
                            blocks.pop();
                        }
                        // blocks[new_block_id] = new_block;
                        // new_block_id = self.new_block();
                        // new_block = blocks[new_block_id].clone();
                        new_block_id = self.new_block(blocks, Some(new_block_id));
                    }

                    // println!("wow the instruction was {:?}", wow);
                }

                // TODO: best case in performance would be not to have these blocks that are made by the if
                // function that are untracked in the first place
                if blocks[new_block_id].size == 0 {
                    blocks.swap_remove(new_block_id);
                }
                // if !new_block.instructions.is_empty() {
                // } else {
                //     blocks.swap_remove(new_block_id);
                // }

                // context.successors.push(blocks.len());

                // split the current block instructions
                // as previous block (context) does not garuantee that the new block (scope block) will be executed
                // let split_block = self.new_block_with(context.id);

                println!("!!!!!!!!!!!!!!!!!!!!!!!!!!! RETURNING TRUE");
                return Some(first_id);
                // need this to return a number..
            }
            Expressions::If(conditional, scope) => {
                // maybe make a new block for the condition though i dont think that is nesscessary
                // let condition
                let first_scope = self.build_instruction(variables, instructions, blocks, context_id, *conditional, callback);
                let scope = self.build_instruction(variables, instructions, blocks, context_id, *scope, callback).unwrap();
                let mut new_block = self.new_block(blocks, None);
                let context = &blocks[context_id];

                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::JumpConditional(
                        context.end.unwrap(),
                        // context.instructions.len() - 1,
                        scope,
                        blocks.len() - 1, // blocks.len() - 1,
                    ),
                );
                // context.instructions.push(Instruction::JumpConditional(
                //     context.instructions.len() - 1,
                //     scope,
                //     blocks.len() - 1, // blocks.len() - 1,
                // ));

                println!("RETURNING BLOCK: {:?}", new_block);
                // make it jump to the new block made thats after the if scope
                // if the condition was false

                return Some(new_block);
            }
            Expressions::CompareEqual(primary_variable, other_variable) => {
                // maybe make a new block for the condition though i dont think that is nesscessary
                // let condition
                self.build_instruction(variables, instructions, blocks, context_id, *primary_variable, callback);
                self.build_instruction(variables, instructions, blocks, context_id, *other_variable, callback);
                let context = &blocks[context_id];

                self.add_instruction(instructions, blocks,
                    context.id,
                    InstructionData::CompareEqual(
                        instructions[context.end.unwrap()].previous.unwrap(),
                        // context.instructions.len() - 2,
                        context.end.unwrap(), // context.instructions.len() - 1,
                                              // context.instructions.len() - 1,
                    ),

                );
                // context.instructions.push(Instruction::CompareEqual(
                //     context.instructions.len() - 2,
                //     context.instructions.len() - 1,
                //     // context.instructions.len() - 1,
                // ));
            }
            Expressions::FunctionBlock(name, arguments, scope) => {
                // let instruction_id = instructions.len();
                // let context = &blocks[context_id];
                // let function = functions.len();

                let block = blocks.len();

                let mut instruction_arguments: Vec<InstructionData> = Vec::with_capacity(arguments.len());
                for argument in arguments {
                    println!("ARGUMENT IS {:?}", argument)
                    // self.build_instruction(variables, instructions, blocks,
                    //     context_id,
                    //     function_id,
                    //     argument,
                    //     &mut |
                    //     id,
                    //     instruction| {
                    //     println!("within the function block i am adding an rgument of {:?}", instruction);
                    //     instruction_arguments.push(instruction);
                    // });
                }
                let mut instruction_data: IndexVec<usize, InstructionData> = IndexVec::new();
                // let function_definition = instructions.len();
                // self.build_instruction(variables, instructions, blocks, functions, context_id, function_id, *scope, &mut |id, data| {
                //     // self.add_instruction(instructions, blocks, block, data, function);
                //     instruction_data.push(data);
                // });

                // self.add_instruction(instructions, blocks, , instruction, function)
                let mut new_blocks = Vec::new();
                let mut new_instruction = IndexVec::new();
                let mut new_variables = IndexVec::new();
                // let block = self.new_block(&mut new_blocks, None);
                let scope_value = self.build_instruction(&mut new_variables, &mut new_instruction, &mut new_blocks, 0, *scope, callback);

                let Expressions::Variable(name) = *name else { panic!() };
                let function = callback(
                    Functions {
                    name: Some(name),
                    phi_instructions: IndexVec::new(),
                    blocks: new_blocks,
                    instructions: new_instruction,
                    upvalues: Vec::new(),
                    variables: new_variables,
                });
                self.add_instruction(instructions, blocks, context_id, InstructionData::Closure(function));
                // self.new_block(function_id, None);
                println!("OUTPUT OF SCOPE THINGY IN FUNCTION {:?}", scope_value);
                let mut instructions_start = instructions.len() - 1;
                // functions.push(Functions {
                //     name: Some(name),
                //     phi_instructions: IndexVec::new(),
                //     blocks: Vec::new(),
                //     instructions: IndexVec::new(),
                //     upvalues: Vec::new(),
                //     variables: IndexVec::new(),
                // });
                for data in instruction_data {
                    let id = self.add_instruction(instructions, blocks, block, data);
                    // instructions.push(instructions[id].clone()); // dont know naything better
                    // than a clone
                }
                let mut instructions_end = instructions.len() - 1;

                // if let InstructionData::Return(_) = instructions[instructions_end].data {
                // } else {
                //     self.add_instruction(instructions, blocks, instructions_end, instruction, function)
                //     instructions_end += 1;
                // }


                return Some(blocks.len() - 1);

                // arguments is merely a lsit of variable isntructions that would be used

                // function blocks will be inserted within blocks
            }
            Expressions::CallFunction(function_variable, variadic) => {
                if *function_variable == Expressions::Variable("print".to_string()) {
                    self.build_instruction(variables, instructions, blocks, context_id, variadic[0].clone(), callback);
                    let context = &blocks[context_id];


                    self.add_instruction(instructions, blocks,
                        context.id,
                        InstructionData::Print(context.end.unwrap()),


                    );
                    return None;
                }
                // let mut new_function_id = 0;
                println!("got {:?} and {:?}", function_variable, variadic);
                // for (index, function) in functions.iter().enumerate() {
                //     println!("matching {:?} with {:?}", function.name, function_variable);
                //     if let Some(name) = &function.name {
                //         if *function_variable == Expressions::Variable(name.to_string()) {
                //             new_function_id = index;
                //         }
                //     }
                // }

                // if new_function_id == 0 {
                //     panic!("No function with name {:?} was found", function_variable);
                // }
                let mut variadic_vec = Vec::with_capacity(variadic.len());
                for variable in &variadic {
                    self.build_instruction(variables, instructions, blocks, context_id, variable.clone(), callback);
                    variadic_vec.push(instructions.len() - 1);
                }
                self.build_instruction(variables, instructions, blocks, context_id, *function_variable, callback);
                // let Exp new_function_id
                // println!("instructions is {:?}", instructions);
                println!("ok so it is now {:?}", instructions[instructions.len() - 1].data);
                // let InstructionData::Closure(closure_value) = instructions[instructions.len() - 1].data else { panic!("not a closure!!") };
                self.add_instruction(instructions, blocks, context_id, InstructionData::CallFunction(instructions.len() - 1, variadic_vec));
            }
            e => {
                println!("instruction {:?}, not yet implemented", e);
            }
        }

        None
        // should change block?
        // None
        // context.instructions.push(instruction);
    }

    pub fn post_order(
        &self,
        function: &Functions,
        // block: &Block,
        block_id: usize,
        // blocks: &mut Vec<Block>,
        // instructions: &mut IndexVec<usize, Instruction>,
        current_post_order: &mut Vec<usize>,
        visited: &mut IndexVec<usize, bool>
    ) {

        current_post_order.push(block_id);

        println!("this function ahs {:?} blocks ", function.blocks.len());
        function.blocks[block_id].get_successors(
            &function.instructions,
            |successor, _| {
                println!("succesor {:?}", successor);
                if !visited[successor] {
                    visited[successor] = true;
                    // [successor]
                    // dont know if i cna do something about this clone
                    self.post_order(function, successor, current_post_order, visited);
                }

            }
        );
        // self.get_successors(
        //     instructions,
        //     block,
        //     // &blocks[block_id],
        // });
    }

    pub fn build_dominance(
        &self,
        blocks: &mut Vec<Block>,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
        post_order: Vec<usize>
    ) -> IndexVec<usize, usize> {
        // dominance - block id, vector of block ids
        // TODO: i dont know if this is correct
        // but iterate over reverse *post* order
        // due to this, the language cannot have a `goto` statement
        // should look into adding something similar to a goto, or fulfills a similar purpose

        let mut changed = true;

        // let post_order = vec![0, 1, 4, 5, 2, 3, 6];
        // let post_order = {
        //
        //     let mut current_order = Vec::new(); //maybe make it wiht the capacity of all blocks?
        //     let mut visited = index_vec!(false; blocks.len());
        //
        //
        //     self.post_order(0, &mut current_order, &mut visited);
        //
        //     current_order
        // };

        let post_indices = {
            let mut indices = index_vec!(None; blocks.len());
            println!("BLOCK LEN: {:?}", blocks.len());
            for thing in &mut *blocks {
                println!("      there is {:?}", thing.id);
            }
            for (index, block) in post_order.iter().enumerate() {
                println!("INDICE: {:?} HAS BLOCK: {:?}", index, block);
                indices[*block] = Some(index);
            }

            indices
        };
        println!("//////////////////////////////////////");
        for thing in &post_order {
            println!("POST ORDER IS {:?}", thing);
        }

        let mut domination = index_vec!(None; post_order.len());

        domination[0] = Some(0);
        let mut changed = true;

        while changed {
            changed = false;

            for block_id in post_order.iter().rev().copied() {
                if block_id == 0 {
                    continue;
                }

                let block = &blocks[block_id];
                let preds = &predecessors[block_id];
                // println!("BLOCK THINGY HAS {:?} SIZE AND {:?}", block.size, block.previous_block);
                println!("preds 0 was {:?} for block {:?}", preds[0], block_id);

                let mut new_dom = post_indices.get(preds[0]).unwrap().expect("wow"); // mighjt be erorr
                // prone?

                println!(
                    "block: {:?} has predecessors {:?}, and the first one is {:?}",
                    block.id,
                    predecessors.len(),
                    new_dom
                );
                // TODO: rewrite this whole thing
                for predecessor in preds.iter().skip(1).copied() {
                    println!(" || PREDECESSOR: {:?} OF BLOCK {:?}", predecessor, block_id);
                    let Some(Some(pred)) = post_indices.get(predecessor) else {
                        continue;
                    };
                    let pred = *pred;
                    if domination[pred].is_some() {
                        let mut x = new_dom;
                        let mut y = pred;
                        println!("what checking x: {:?} || and then y is: {:?}", x, y);

                        while x != y {
                            println!("loop 1");
                            while x < y {
                                println!(
                                    "loop 2 where x: {:?} and domination x is {:?}",
                                    x, domination[x]
                                );
                                // NOTE: i have no clue if i should do this but when x is 0 that
                                // means its domination is also 0 which results in an infinite loop
                                // where x will never be bigger than y (which is 1)
                                if x == 0 {
                                    println!("x is 0 breaking");
                                    break;
                                }
                                x = domination[x].unwrap();
                            }
                            while y < x {
                                println!("loop 3");
                                y = domination[y].unwrap();
                            }
                            if x == 0 {
                                println!("x is 0 breaking 2");
                                break;
                            }
                        }

                        new_dom = x;
                    }
                }

                let block_indice = post_indices.get(block_id).unwrap().expect("wow");
                println!("block indice: {:?}", block_indice);
                println!("||||||||| DOMINANCE: {:?}", domination);
                if domination[block_indice] != Some(new_dom) {
                    println!("ADDING {:?} TO INDICE: {:?}", new_dom, block_indice);
                    domination[block_indice] = Some(new_dom);
                    changed = true;
                }
            }
        }
        // let domination = index_vec!(Some(0), Some(0), Some(0), Some(1), Some(3), Some(0));

        // for dommy in &domination {
        //     println!("DOMINIATION {:?}", dommy);
        // }
        let mut output = IndexVec::with_capacity(blocks.len());
        for block in blocks {
            if let Some(Some(post_index)) = post_indices.get(block.id) {
                println!("got indice: {:?}, current block {:?}", post_index, block.id);
                if let Some(immediate_index) = domination[*post_index] {
                    output.push(post_order[immediate_index]);
                }
                // immediate_index = domination[*post_index];
            }
        }

        output

        // for block in self.blocks.iter() {
        //     let mut predecessor_dominance = Vec::with_capacity(block.predecessors.len());
        //     let mut output_dominance = vec![block.id];
        //
        //     for predecessor in &block.predecessors {
        //         if let Some(dominators) = dominance.get(*predecessor) {
        //             predecessor_dominance.push(dominators);
        //         }
        //     }
        //
        //     if let Some(pred) = predecessor_dominance.first() {
        //         'outer_dom: for dominator in pred.iter() {
        //             let mut iter = predecessor_dominance.iter();
        //             iter.next();
        //             for other_dominator in iter {
        //                 if (*other_dominator).contains(dominator) {
        //                     output_dominance.push(*dominator);
        //                     continue 'outer_dom;
        //                 }
        //             }
        //         }
        //     }
        //
        //     dominance[block.id] = output_dominance;
        // }
    }

    // IMPORTANT: finish dominence frontiers!!!!
    // NOTE: maybe dominance tree as its also a useful utility?
    pub fn dominance_frontiers(
        &self,
        blocks: &mut Vec<Block>,
        dominance: &IndexVec<usize, usize>,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
    ) -> IndexVec<usize, Vec<usize>> {
        let mut frontier = index_vec!(vec![]; blocks.len());

        for dommy in dominance {
            println!("!!! immediate dommy {:?}", dommy);
        }
        for block in blocks {
            let block_predecessors = &predecessors[block.id];
            // if block_predecessors.len() < 2 {
            //     continue;
            // }

            let Some(dominator) = dominance.get(block.id) else {
                println!("no exist on block id {:?}", block.id);
                continue;
            };

            for predecessor in block_predecessors {
                if dominance.get(*predecessor).is_none() {
                    println!("its none on dominance {:?}", predecessor);
                    continue;
                }

                let mut at = predecessor;
                println!("CHECKING {:?} == {:?}", at, dominator);
                while at != dominator {
                    let dominance_frontier = &mut frontier[*at];
                    // println!(
                    //     "      CHECKING INNER {:?} == {:?} || WITH FRONTIER: {:?} || BLOCK ID {:?}",
                    //     at,
                    //     dominator,
                    //     dominance.get(*at),
                    //     block.id
                    // );
                    if !dominance_frontier.contains(&block.id) {
                        // println!("adding dominance frontgier");
                        dominance_frontier.push(block.id);
                    }
                    // println!("at is {:?} dominator is {:?}", at, dominator);

                    at = dominance.get(*at).unwrap();
                }
            }
        }

        // for block in self.blocks.iter().rev() {
        //     if block.predecessors.len() < 2 {}
        //     continue;
        //
        //     let Some(dominator) = dominance.get(block.id) else {
        //         continue;
        //     };
        //     let mut current_frontier = Vec::new();
        //
        //     let current_dominance = dominance.get(block.id).unwrap();
        //     for predecessor in current_dominance {
        //         for other_dominance in &dominance[*predecessor] {
        //             if current_dominance.contains(other_dominance) {
        //                 continue;
        //             }
        //             current_frontier.push(*other_dominance);
        //         }
        //     }
        //
        //     if !current_frontier.is_empty() {
        //         frontier[block.id] = current_frontier;
        //     }
        // }

        frontier
    }

    pub fn make_dominance_tree(
        &self,
        blocks: &mut Vec<Block>,
        dominance: IndexVec<usize, usize>,
    ) -> IndexVec<usize, Vec<usize>> {
        let mut tree = index_vec![Vec::new(); blocks.len()];

        for block in blocks.iter().skip(1) {
            if let Some(dominator) = dominance.get(block.id) {
                tree[*dominator].push(block.id);
            }
        }

        tree
    }

    pub fn make_ssa(
        &mut self,
        phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        variables: &mut IndexVec<usize, String>,
        dominance_frontier: IndexVec<usize, Vec<usize>>,
        dominance_tree: &IndexVec<usize, Vec<usize>>,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
    ) {
        let mut phis = {
            let mut visited =
                index_vec!(index_vec!(false; variables.len()); blocks.len());

            let mut stack = Vec::new();
            for block in &mut *blocks {
                self.for_block(block, instructions, |instruction| {
                    if let InstructionData::SetVariable(id, _) = instruction.data {
                        if !visited[block.id][id] {
                            visited[block.id][id] = true;
                            stack.push((block.id, id));
                        }
                    }
                    // let Instruction::SetVariable(id, _) = instruction else {
                    // };
                });
                // for instruction in &block.instructions {
                //     let Instruction::SetVariable(id, _) = instruction else {
                //         continue;
                //     };
                //
                //     if !visited[block.id][*id] {
                //         visited[block.id][*id] = true;
                //         stack.push((block.id, id));
                //     }
                // }
            }

            let mut phis: IndexVec<usize, Vec<(usize, Vec<(usize, Option<usize>)>, usize)>> =
                index_vec!(vec![]; blocks.len());

            let mut map_size = 0;
            while let Some((from_block, variable_id)) = stack.pop() {
                for to_block in dominance_frontier[from_block].iter().copied() {
                    let mut to_phis = &mut phis[to_block];

                    // checks if this variable does not have a phi instruction already added within
                    // this block
                    let is_new = to_phis
                        .iter()
                        .find(|(num, _, _)| num == &variable_id)
                        .is_none();
                    if is_new {
                        let predecessor = &predecessors[to_block];
                        // let predecessor = &block_predecessors;
                        // variable_id isntead??

                        let map = predecessor.iter().map(|pred| (*pred, None)).collect();
                        let phi = self.new_instruction(instructions, InstructionData::Phi(0));
                        // self.phi_instructions.push()
                        // map_size += 1;

                        // TODO: try not to clone
                        to_phis.push((
                            variable_id,
                            map,
                            phi, // predecessor.clone(),
                                 // self.phi_instructions.len(),
                        ));

                        // to_block now defines a variable
                        if !visited[from_block][variable_id] {
                            visited[from_block][variable_id] = true;
                            stack.push((from_block, variable_id));
                        }
                        // let map = predecessor.iter().map(|predecessor_id| predecessor_id).collect();
                    }
                }
            }

            phis
        };

        // rename variables
        {
            fn visit(
                block_id: usize,
                blocks: &mut Vec<Block>,
                instructions: &mut IndexVec<usize, Instruction>,
                mut new_names: IndexVec<usize, Option<usize>>,
                phis: &mut IndexVec<usize, Vec<(usize, Vec<(usize, Option<usize>)>, usize)>>,
                // IndexVec<usize, Vec<(usize, IndexVec<usize, usize>, usize)>>,
                dominance_tree: &IndexVec<usize, Vec<usize>>,
                module: &mut Module,
            ) {
                let block = &mut blocks[block_id];
                for (variable_id, _map, instruction) in &phis[block.id] {
                    new_names[*variable_id] = Some(*instruction);
                }

                // println!("the length of instructions: {:?}", block.instructions.len());
                // println!("block id is {:?}", block.id);
                // let mut index = 0;
                block.for_instruction_mut(instructions, |instr_id,  new_instructions, new_block| {
                    match new_instructions[instr_id].data {
                        // IMPORTANT: instructions might not be proprely applying due to
                        // cloning
                        InstructionData::GetVariable(id) => {
                            println!("got it for variable {:?}", id);
                            // println!("current index: {:?}", index);
                            let new_name = new_names[id].unwrap();
                            // println!("new name: {:?}", new_name);
                            // TOOD: make a copy instruction
                            // FIXME: get some sort of way to get the id of the id of the
                            // instruction
                            new_names[id] = Some(instr_id);
                            println!(
                                "ok its after set and the value for both is: old: {:?} | new: {:?}",
                                new_name, new_names[id]
                            );
                            new_instructions[instr_id].data = InstructionData::Copy(new_name);
                        }

                        InstructionData::SetVariable(id, to_value) => {
                            println!("set var to id: {:?} and {:?}", id, to_value);
                            new_names[id] = Some(instr_id);
                            new_instructions[instr_id].data = InstructionData::Copy(to_value);

                        } // TODO: make it so that every use of variable will d this
                        // i think this might be enough? but idk
                        _ => {}

                    }
                });
                //
                // module.for_block_mut(
                //     instructions,
                //     blocks,
                //     block_id,
                //     |instruction| {
                //         match instruction.data {
                //             // IMPORTANT: instructions might not be proprely applying due to
                //             // cloning
                //             InstructionData::GetVariable(id) => {
                //                 println!("got it for variable {:?}", id);
                //                 // println!("current index: {:?}", index);
                //                 let new_name = new_names[id].unwrap();
                //                 // println!("new name: {:?}", new_name);
                //                 // TOOD: make a copy instruction
                //                 // FIXME: get some sort of way to get the id of the id of the
                //                 // instruction
                //                 new_names[id] = Some(instruction.id);
                //                 println!(
                //                     "ok its after set and the value for both is: old: {:?} | new: {:?}",
                //                     new_name, new_names[id]
                //                 );
                //                 instruction.data = InstructionData::Copy(new_name);
                //             }
                //
                //             InstructionData::SetVariable(id, to_value) => {
                //                 println!("set var to id: {:?} and {:?}", id, to_value);
                //                 // new_names[id] = Some(instruction.id);
                //                 // instruction.next = None;
                //                 // instruction.previous = None;
                //                 // instruction.block = None;
                //
                //                 instruction.data = InstructionData::Copy(to_value);
                //             } // TODO: make it so that every use of variable will d this
                //             // i think this might be enough? but idk
                //             _ => {}
                //         }
                //         // index += 1;
                //     });
                // for (index, mut instruction) in block.instructions.iter_mut().enumerate() {
                // index += 1;
                // }

                // let mut map_size = 0;
                module.get_successors(
                    instructions,
                    &blocks[block_id],
                    // block_id,
                    |sucessor, _| {
                    for (variable_id, map, map_id) in &mut phis[sucessor] {
                        // let mut entry = map.iter_mut().find(|from| from == sucessor).unwrap();
                        // for mut predecessor in map.iter() {
                        //     if predecessor == &sucessor {
                        //         predecessor = &mut new_names[*variable_id].unwrap();
                        //         // dont know if this break is needed?
                        //         // test to see if it changes a lot
                        //         break;
                        //     }
                        // }
                        // TODO: dont know if this should be how it works?
                        let entry = map
                            .iter_mut()
                            .find(|(from_block, _)| *from_block == block_id)
                            .unwrap();
                            println!("TRYING OT CHANGE THIS BLOCK {:?} TO {:?}", entry, new_names[*variable_id]);
                        entry.1 = Some(new_names[*variable_id].unwrap());
                        // module.phi_instructions.push(map);
                        // map_size += 1;

                        // entry = &mut new_names[*variable_id].unwrap()
                    }
                });

                for dominated in &dominance_tree[block_id] {
                    if *dominated == block_id {
                        println!("its the same!!!!!!!!!!!!!!!!!!!!");
                        break;
                    }
                    println!("block is being dominated by: {:?}", dominated);
                    visit(*dominated, blocks, instructions, new_names.clone(), phis, dominance_tree, module);
                }
            }

            let mut new_names = index_vec!(None; variables.len());
            visit(0,blocks, instructions,  new_names, &mut phis, dominance_tree, self);
        }
        println!("IN THE SSA THINGY");
        for block in &mut *blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            self.for_block(block, instructions, |instruction| {
                println!(
                    "      tjhe block {:?} instruction is {:?}",
                    instruction.id, instruction.data
                );
            });
        }

        // make phis have proper values
        {
            for (block_index, phis) in phis.iter().enumerate() {
                // let block = self.blocks[block_index];
                let mut at = None;

                for (block_id, map, instruction_id) in phis {
                    let map: Vec<(usize, usize)> = map
                        .into_iter()
                        .map(|(from_block, instruction)| (*from_block, instruction.unwrap()))
                        .collect();

                    println!(
                        "GOT BLOCK ID {:?} AT IS {:?} INSTRUCTION ID {:?}",
                        block_id, at, instruction_id
                    );
                    // {
                    //     let InstructionData::Phi(mut map_id) =
                    //         self.instructions[*instruction_id].data
                    //     else {
                    //         panic!()
                    //     };
                    //
                    //     // self.phi_instructions.push(map);
                    //     println!(
                    //         "map id {:?} for instruction id {:?}",
                    //         map_id, instruction_id
                    //     );
                    //     // TODO: mightr be a more efficient way than inserting
                    //     self.phi_instructions.inner_mut().insert(map_id, map);
                    //     // self.phi_instructions[map_id] = map;
                    // }
                    instructions[*instruction_id].data =
                        InstructionData::Phi(phi_instructions.len());
                    self.insert_instruction(instructions, blocks, block_index, at, *instruction_id);
                    // self.phi_instructions.len()
                    phi_instructions.push(map);

                    at = Some(*instruction_id);
                }
            }
        }
    }

    pub fn copy_propogation(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        dominance_tree: &IndexVec<usize, Vec<usize>>,
        phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>

    ) {
        fn visit(
            instructions: &mut IndexVec<usize, Instruction>,
            blocks: &mut Vec<Block>,
            phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>,
            block_id: usize,
            module: &mut Module,
            dominance_tree: &IndexVec<usize, Vec<usize>>,
        ) {
            module.block_replace_args(
                &blocks[block_id],
                instructions,
                phi_instructions,
                |instruction_id, new_instructions| {
                    println!("heres the instruction: {:?} with the id of {:?}", new_instructions[*instruction_id].data, instruction_id);
                    if let InstructionData::Copy(source) = new_instructions[*instruction_id].data
                    {
                        println!(
                            "i see source {:?} for instruction id {:?}",
                            source, instruction_id
                        );
                        let mut new_arguments = source;
                        while let InstructionData::Copy(source) = new_instructions[new_arguments].data {
                            // panic!("wow");
                            println!("trying to find source {:?} for {:?}", source, new_instructions[*instruction_id].data);
                            if new_arguments == source {
                                panic!("they are the same");
                            }
                            new_arguments = source;
                        }
                        println!("      new instruction id {:?} for thing {:?}", new_arguments, instruction_id);

                        // instructions[*instruction_id]
                        *instruction_id = new_arguments;
                        println!("instruction id is now {:?}", instruction_id);
                    }
                });

            for dominated in dominance_tree[block_id].iter() {
                visit(instructions, blocks, phi_instructions, *dominated, module, dominance_tree);
            }
            // module.for_block_mut(block_id, |instruction| {});
        }

        visit(instructions, blocks, phi_instructions, 0, self, dominance_tree);

        println!("ok within this thing lets see ||||||| pppppppppppppppppppppppppppppppppppppppppp");

        for block in blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            self.for_block(
                block,
                instructions,
                // block.id,
                |instruction| {
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

        for phi in phi_instructions {
            println!("theres a phi instruction! {:?}", phi);
        }
    }

    pub fn dead_copy_elimination(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>
    ) {
        let mut visited = index_vec![false; instructions.len()];
        self.all_args(instructions, phi_instructions, blocks, |argument| {
            println!("visited {:?}", argument);
            visited[argument] = true;
        });

        for block_id in 0..blocks.len() {
            let mut current_block = blocks[block_id].start;
            // let mut at = current_block.start;

            while let Some(instruction_id) = current_block {
                let instruction = &instructions[instruction_id];
                let next = instruction.next;
                println!(
                    "checking {:?} and {:?} the data {:?}",
                    instruction_id, visited[instruction_id], instruction.data
                );
                if !visited[instruction_id] {
                    if let InstructionData::Copy(source) = instruction.data {
                        // let source_value = core::mem::take(&mut source.)
                        println!("removing copy {:?}", instruction_id);
                        self.remove_instruction(instructions, blocks, instruction_id);
                    }
                }

                current_block = next;
            }
        }
    }

    // convert to conventional ssa form
    pub fn convert_to_cssa(
        &mut self,
        blocks: &mut Vec<Block>,
        instructions: &mut IndexVec<usize, Instruction>,
        phi_instructions: &mut IndexVec<usize, Vec<(usize, usize)>>,
        predecessors: &IndexVec<usize,
        IndexVec<usize, usize>>
    ) {
        let mut predecessor_copy_ids = index_vec![None; blocks.len()];

        // for block_id in 0..self.bl {
        //
        // }
        let mut last_parrallel_id = 1;
        for block in blocks.iter() {

            let start_instruction = &instructions[block.start.unwrap()];
            if let InstructionData::Phi(_) = start_instruction.data  {
            // if let InstructionData::Phi(first_phi) = instructions[block.start.unwrap()].data {
                let phi_parralel_copy = last_parrallel_id;
                println!("----------------------------> PARRALEL COPY {:?}", phi_parralel_copy);
                last_parrallel_id += 1;


                {
                    fn visit(predecessor_copy_ids: &mut IndexVec<usize, Option<usize>>, block_id: usize, mut last_parrallel_id: usize, predecessors: &IndexVec<usize, IndexVec<usize, usize>>) {

                        for predecessor in &predecessors[block_id] {
                            println!("adding {:?} predecessor to parralel ids as {:?} within the block {:?}", predecessor, last_parrallel_id, block_id);
                            // issue here is that within the loop, it does not get ALL of the
                            // predecessors
                            // it only gets the immediate predecessors of a given block
                            // which does not work
                            // we need to get all of the predecessors, including for example block 0
                            predecessor_copy_ids[*predecessor] = Some(last_parrallel_id);
                            last_parrallel_id += 1;

                            // visit(predecessor_copy_ids, *predecessor, last_parrallel_id, predecessors);
                        }
                    }

                    visit(&mut predecessor_copy_ids, block.id, last_parrallel_id, predecessors);
                }

                println!("----------------------------< AFTER  PARRALEL COPY {:?}", phi_parralel_copy);
                // for predecessor in &predecessors[block.id] {
                //     println!("adding {:?} predecessor to parralel ids as {:?} within the block {:?}", predecessor, last_parrallel_id, block.id);
                //     // issue here is that within the loop, it does not get ALL of the
                //     // predecessors
                //     // it only gets the immediate predecessors of a given block
                //     // which does not work
                //     // we need to get all of the predecessors, including for example block 0
                //     predecessor_copy_ids[*predecessor] = Some(last_parrallel_id);
                //     last_parrallel_id += 1;
                //
                // }

                // for predecessor in &predecessors[block.id] {
                //     println!("adding {:?} predecessor to parralel ids as {:?} within the block {:?}", predecessor, last_parrallel_id, block.id);
                //         // issue here is that within the loop, it does not get ALL of the
                //     // predecessors
                //     // it only gets the immediate predecessors of a given block
                //     // which does not work
                //     // we need to get all of the predecessors, including for example block 0
                //     predecessor_copy_ids[*predecessor] = Some(last_parrallel_id);
                //     last_parrallel_id += 1;
                // }

                let mut old_phi_cursor = Some(start_instruction.id);
                let mut new_phi_cursor = None;
                while let Some(at) = old_phi_cursor {
                    let InstructionData::Phi(map) = instructions[at].data else { break; };
                    // let map = &mut self.phi_instructions[at];

                    // parallel copies for each phi arguments in predecessors
                    // TODO: try not to clone

                    // FIXME: i dont think phi_instructions[at] would work.
                    // At is an instruction not a phi map so in order to get the proper map i need
                    // to get it from the phi instruction value using a let-else itself
                    for (predecessor, source) in &mut phi_instructions[map] {
                        println!("tryign to find predecessor {:?} within {:?}", predecessor, predecessor_copy_ids);
                        let copy_id = predecessor_copy_ids[*predecessor].unwrap();
                        let copy = instructions.len();


                        // insert parralel copy before the end of the block
                        // let last_block = blocks[*predecessor].clone();
                        let last_id = blocks[*predecessor].end.unwrap();
                        let last_instruction = &instructions[last_id];

                        instructions.push(Instruction {
                            data: InstructionData::ParallelCopy(*source, copy_id),
                            previous: last_instruction.previous,
                            next: Some(last_id),
                            // function: last_instruction.function,
                            block: Some(*predecessor),
                            id: copy,
                        });

                        // FIXME: might not work? check if works
                        *source = copy;

                    }

                    // copies for phi outputs
                    // replace phi uses with parralel copies of that phi use

                    let new_phi = instructions.len();
                    println!("THE DATA {:?}", instructions[map].data.clone());

                    instructions.push(Instruction {
                        data: instructions[map].data.clone(),
                        block: Some(block.id),
                        next: None,
                        previous: new_phi_cursor,
                        id: new_phi,
                        // function: block.function
                    });

                    // insert
                    if let Some(new_cursor) = new_phi_cursor {
                        println!("the old next for the new_phi_cursor was {:?}", new_cursor);
                        instructions[new_cursor].next = Some(new_phi);
                    }


                    let phi = &mut instructions[map];
                    phi.data = InstructionData::ParallelCopy(new_phi, phi_parralel_copy);
                    // phi.data = self.instructions[new_phi].data;

                    old_phi_cursor = phi.next;

                    new_phi_cursor = Some(new_phi);

                }

                for copy_id in &mut predecessor_copy_ids {
                    *copy_id = None;
                }
                // let mut new_phi_cursor
            }
        }
    }

    pub fn block_gen_kill(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>


    ) -> (IndexVec<usize, IndexVec<usize, bool>>, IndexVec<usize, IndexVec<usize, bool>>){
        let mut gens = IndexVec::with_capacity(blocks.len());
        let mut kills = IndexVec::with_capacity(blocks.len());

        for block in &mut *blocks {
            let mut generate = index_vec![false; instructions.len()]; // perhaps this is too long?
            let mut kill = index_vec![false; instructions.len()];

            self.for_block_reverse(
                instructions,
                block,
                // block.id,
                |instruction| {
                    if instruction.has_value() {
                        kill[instruction.id] = true;
                        generate[instruction.id] = false;
                    }

                    if let InstructionData::Phi(_) = instruction.data{
                    } else {
                        self.instruction_args(&phi_instructions, &instruction.data, |argument| {
                            generate[argument] = true;
                        });
                    }

                });

            gens.push(generate);
            kills.push(kill);
        }

        (gens, kills)
    }

    // TODO: name the types way better than just this
    // this outputs `live_ins` and `live_outs`
    pub fn block_live_in_out(
        &self,
        blocks: &mut Vec<Block>,
        instructions: &mut IndexVec<usize, Instruction>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>
    ) -> (IndexVec<usize, IndexVec<usize, bool>>, IndexVec<usize, IndexVec<usize, bool>>){
        let (gens, kills) = self.block_gen_kill(instructions, blocks, phi_instructions);

        let mut live_ins = index_vec![index_vec![false; instructions.len()]; blocks.len()];
        let mut live_outs = index_vec![index_vec![false; instructions.len()]; blocks.len()];

        let mut changed = true;
        while changed {
            changed = false;

            for block in blocks.iter() {
                let generate = &gens[block.id];
                let kill = &kills[block.id];

                let mut new_live_out = index_vec![false; instructions.len()];

                self.get_successors(instructions, block, |successor, instructions| {
                    for (i, live) in live_ins[successor].iter().enumerate() {
                        if *live {
                            new_live_out[i] = true;
                        }
                    }

                    self.for_block_terminator(instructions, blocks, successor, |instruction| {
                        if let InstructionData::Phi(map_id) = instruction.data {
                            // let map = self.phi_instructions[map_id];
                            let source = phi_instructions[map_id].get(block.id).unwrap();
                            new_live_out[source.1] = true;

                            return true;
                        }

                        false
                    });


                });

                let mut new_live_in = new_live_out.clone();
                for (i, kill) in kill.iter().enumerate() {
                    if *kill {
                        new_live_in[i] = false;
                    }
                }

                for (i, generated) in generate.iter().enumerate() {
                    if *generated {
                        new_live_in[i] = true;
                    }
                }

                if new_live_in != live_ins[block.id] {
                    changed = true;
                    live_ins[block.id] = new_live_in;
                }
                if new_live_out != live_outs[block.id] {
                    changed = true;
                    live_outs[block.id] = new_live_out;
                }
            }
        }

        (live_ins, live_outs)
    }

    pub fn instruction_post_order(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &Vec<Block>,
        post_order: &Vec<usize>
    )  -> Vec<usize> {
        let mut output = Vec::with_capacity(instructions.len());
        for block in post_order {
            self.for_block(&blocks[*block], instructions, |insturction| {
                output.push(insturction.id);
            });

        }

        output
    }
    // pub fn function_post_order(&self, function_id: usize, function_order: &Vec<usize>)  -> Vec<usize> {
    //     let function = &self.functions[function_id];
    //     let mut output = Vec::with_capacity(function.instructions_end - function.instructions_start);
    //     for block in function_order {
    //         self.for_block(*block, |insturction| {
    //             output.push(insturction.id);
    //         });
    //
    //     }
    //
    //     output
    // }

    // pub fn function_intervals(
    //     &self,
    //     instructions: &mut IndexVec<usize, Instruction>,
    //     phi_instructions: IndexVec<usize, Vec<(usize, usize)>>,
    //     function_id: usize,
    //     function_order: &Vec<usize>,
    //     function_indices: &IndexVec<usize, usize>,
    //     live: &mut IndexVec<usize, Option<usize>>
    // )  {
    //     for (order_id, instruction_id) in function_order.iter().enumerate().rev() {
    //         let mut instruction = &instructions[*instruction_id];
    //
    //         if instruction.has_value() {
    //             if let InstructionData::Closure(new_function) = instruction.data {
    //
    //             } else if let InstructionData::Phi(map_id) = instruction.data {
    //                 let mut phi_map = &phi_instructions[map_id];
    //                 for (_, arg) in phi_map {
    //                     let indice = function_indices[*arg];
    //                 }
    //
    //             } else {
    //
    //             }
    //         }
    //     }
    // }

    pub fn live_intervals(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        blocks: &mut Vec<Block>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        post_order: &Vec<usize>,
        instruction_indices: &IndexVec<usize, usize>
    ) -> IndexVec<usize, Option<usize>>{

        let instruction_order = self.instruction_post_order(instructions, blocks, post_order);
        let mut live = index_vec![None; instruction_order.len()];


        for (order_id, instruction_id) in instruction_order.iter().enumerate().rev() {
            let instruction = &instructions[*instruction_id];

            println!("checking if {:?} has value", instruction.data);
            if instruction.has_value() {
                println!("          it does!");
                if let InstructionData::Phi(map_id) = instruction.data {
                    let phi_map = &phi_instructions[map_id];
                    for (block, arg) in phi_map {
                        let indice = instruction_indices[*arg];
                        live[indice] = live[order_id];
                    }
                } else {
                    self.instruction_args(phi_instructions, &instruction.data, |arg| {
                        println!("In {:?}", instruction.data);
                        let indice = instruction_indices[arg];
                        if live[indice].is_none() {
                            live[indice] = Some(order_id);
                        }
                    });
                }

            }
            // a liveness of none couudl mean 2 things
            // phi functions: maybe?
            // or a variable that isnt used
        }

        for (index, value) in live.iter().enumerate() {
            println!("!! Liveness index: {:?} -> with end {:?}", index, value);
            // if let Some(data) = index {
                let instruction_id = instruction_order[index];
                let instruction = &instructions[instruction_id];
                println!("          with data {:?}", instruction.data);
            // }
        }

        live
    }

    // loop over phi maps instructions
    // so that i can make it so that if allocate register phi function points to another phi
    // function
    // then it jsut also loops over the other phi functions instructions as well
    // recursively
    pub fn for_phi<F: FnMut(usize)>(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        map_id: &usize,
        callback: &mut F
    ) {
        for (block, phi_arg) in &phi_instructions[*map_id] {
            println!("ok in for phi now, map id {:?}", map_id);
            if let InstructionData::Phi(new_map) = instructions[*phi_arg].data {
                println!("has phi thingies {:?}", new_map);
                if new_map == *map_id {
                    panic!("same??");
                }
                // println!("|| met a phi! {:?} with map {:?}", phi_arg, map_id);
                self.for_phi(instructions, phi_instructions, &new_map, callback);
                callback(*phi_arg,);
            } else {
                println!("no phi hingieis");
                callback(*phi_arg)
            }
        }
    }
    // tewlls which instruction ids/variables require what register
    pub fn allocate_registers(
        &self,
        instructions: &mut IndexVec<usize, Instruction>,
        // blocks: &mut Vec<Block>,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        liveness: IndexVec<usize, Option<usize>>,
        instruction_order: &Vec<usize>,
        instruction_indices: &IndexVec<usize, usize>,
        all_registers: &mut Vec<usize>,
        offset: usize,
    ) -> (IndexVec<usize, Option<usize>>, usize) {

        // this needs to know when a register ends so its safe to make a new one
        // and when  to use old one

        let mut instruction_registers = index_vec![None; instruction_order.len()];
        println!("size of instruction order: {:?}", instruction_order.len());
        all_registers.resize(all_registers.len() + instruction_order.len(), 0);
        // let mut all_registers: IndexVec<usize, usize> = index_vec![0; instruction_order.len()];
        let mut phi_registers: Vec<(usize, usize)> = vec![];
        println!("THE LIVENESS FOR EVERYTHING {:?} LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL", liveness);
        let mut highest_register = 0;
        // let mut instruction_indices = Vec::with_capacity(instruction_order.len());
        for (order_index, liveness_end) in liveness.iter().enumerate() {
            let instruction = &instructions[instruction_order[order_index]];
            println!("post borrow within index {:?}", order_index);

            if let InstructionData::Phi(map_id) = instruction.data {
                println!("in phi");
                let mut first_instruction = None;
                let mut first_register = None;
                // println!("first instruction is {:?} and is -> {:?} || which ahs register {:?}", instruction_indices[*first_instruction], first_instruction, first_register);
                self.for_phi(instructions, phi_instructions, &map_id, &mut |arg| {
                    println!("in for phi");
                    println!("hello {:?}, first instruction: {:?}", arg, first_instruction);
                    if first_instruction.is_none() {
                        let phi_arg_order = instruction_indices[arg];
                        first_instruction = Some(arg);
                        // first_register = Some(std::mem::swap(&mut first_register, instruction_registers[phi_arg_order].unwrap()));
                        // std::mem::swap(&mut first_register, &mut  instruction_registers[phi_arg_order]);
                        first_register =
                            instruction_registers[phi_arg_order];
                        // instruction_registers[phi_arg_order] = first_register;
                    } else {
                        let phi_arg_order = instruction_indices[arg];
                        instruction_registers[phi_arg_order] = first_register;
                        println!("setting {:?} to {:?}", phi_arg_order, first_register);
                    }
                });
                println!("after for phi?");
                all_registers[instruction_indices[first_instruction.unwrap()]] = liveness_end.unwrap();
                println!("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PHI {:?} MET LIVENESS END {:?}", map_id,liveness_end);
                instruction_registers[order_index] = first_register;
                continue;
            }
            // NOTE: make it s othat comapre equals will ahve a lifetime so that it can do stuff
            // wit hthe registers!!
            if liveness_end.is_none() {
                println!("its none! {:?}", order_index);
                continue;
            }

            // for (index, register_end) in all_registers.iter_mut().enumerate() {
            //     if *register_end < order_index {
            //         instruction_registers
            //     }
            // }
            for (index, register_end) in all_registers.iter_mut().enumerate().skip(offset) {
                println!("{:?} < {:?}", register_end, order_index);
                println!("      INDEX {:?}", index);
                // if let  =  {
                //
                // }
                if *register_end < order_index || order_index == 0 {
                    *register_end = liveness_end.unwrap();
                    println!("      index is {:?} and end now is {:?} || order: {:?}", index, register_end, order_index);
                    instruction_registers[order_index] = Some(index);
                    if index > highest_register {
                        highest_register = index;
                    }
                    println!("      instruction registers now {:?}", instruction_registers);
                    break;

                }
            }
            // println!("              register end aftrer {:?}", all_registers[instruction_registers[order_index].unwrap_or(0)])
        }

        for (index, value) in instruction_registers.iter().enumerate() {
                let instruction_id = instruction_order[index];
                let instruction = &instructions[instruction_id];
            println!("Ordered index: {:?} -> with value {:?} || {:?} || {:?} || ", index, value, instruction.data, instruction.id);
            // if let Some(data) = index {
                // println!("          with data {:?}", instruction.data);
            // }
        }

        (instruction_registers, highest_register)
    }

    pub fn compile_bytecode(
        &self,
        instructions: &IndexVec<usize, Instruction>,
        blocks: &Vec<Block>,
        instruction_indices: &IndexVec<usize, usize>,
        instruction_order: Vec<usize>,
        post_order: &Vec<usize>,
        variable_registers: &IndexVec<usize, Option<usize>>
    ) -> Vec<u8>{
        // IMPORTANT: looping over instructions in post order by blocks make it so that copies are
        // gone
        // to fix make copies proprely implemented in their respective blocks, as currently they
        // are somehow ignored?

        // let variable_registers = IndexVec::new();
        let mut output: Vec<u8> = vec![];
        // let mut functions = Vec::new();

        // let instruction_order: Vec<usize> = self.instruction_post_order(post_order);
        println!("--- the bytercode");
        for (index, instruction_id) in instruction_order.iter().enumerate() {
            println!("          got instruction {:?} which is {:?}, and block {:?}", index, instructions[*instruction_id].data, instructions[*instruction_id].block.unwrap());
        }
        println!("--- the instructions of everything");
        for (index, instruction_id) in instructions.iter().enumerate() {
            println!("          got instruction {:?} which is {:?} deleted -> {:?}", index, instruction_id.data, instruction_id.block);
        }
        // for function in &self.functions {
        //     println!("ok so function start: {:?} and end {:?}", function.instructions_start, function.instructions_end);
        // }
        println!("--- the other tgh ing");
        // let mut decoded_instructions = index_vec![None; instructions.len()];
        let mut inserted_ids = 0;
        let mut replace_instructions = Vec::new();
        for (order_id, instruction_id) in instruction_order.iter().enumerate() {
            let instruction = &instructions[*instruction_id];
            println!("instruction data is {:?}", instruction.data);
            println!("Instruction id is {:?}", instruction_id);

            match &instruction.data {
                InstructionData::LoadNumber(num) => {
                    println!("decoded {:?}", instruction_id);
                    // decoded_instructions[*instruction_id] = Some(inserted_ids);
                    inserted_ids = output.len();
                    println!(" {:?} Adding load", order_id);
                    output.push(Opcode::Load as u8);
                    output.push(*num as u8);
                    println!("var registers: {:?} || order id {:?}", variable_registers, order_id);
                    println!("{:?}\n{:?}", num, variable_registers[order_id]);
                    output.push(variable_registers[order_id].unwrap() as u8);
                    println!("---")
                }
                InstructionData::Add(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        println!("decoded {:?}", instruction_id);
                        // decoded_instructions[*instruction_id] = Some(inserted_ids);
                        inserted_ids = output.len();
                        println!(" {:?} Adding add opcode", order_id);
                        println!("{:?}\n{:?}\n{:?}", register_1, register_2, variable_registers[order_id]);
                        println!("---");
                        output.push(Opcode::Add as u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        // output.push(*register_2 as u8);
                        output.push(register_id as u8);
                    }
                    // output.push(variable_registers[order_id].unwrap() as u8);
                    // output.push(variable_registers[instruction_id]);
                }
                InstructionData::Subtract(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        // decoded_instructions[*instruction_id] = Some(inserted_ids);
                        println!("decoded {:?}", instruction_id);
                        inserted_ids = output.len();
                        println!(" {:?} Adding subtract opcode", order_id);
                        println!("{:?}\n{:?}\n{:?}", register_1, register_2, variable_registers[order_id]);
                        println!("---");
                        output.push(Opcode::Subtract as u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        // output.push(*register_1 as u8);
                        // output.push(*register_2 as u8);
                        output.push(register_id as u8);
                    }
                    // println!("adding opcode Subtract {:?}", Opcode::Subtract as u8);
                    // output.push(variable_registers[order_id].unwrap() as u8);
                    // output.push(variable_registers[instruction_id]);
                }
                InstructionData::Multiply(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        // decoded_instructions[*instruction_id] = Some(inserted_ids);
                        // println!("decoded {:?}", instruction_id);
                        inserted_ids = output.len();
                        println!(" {:?} Adding multiply opcode", order_id);
                        println!("{:?}\n{:?}\n{:?}", register_1, register_2, variable_registers[order_id]);
                        println!("---");
                        output.push(Opcode::Multiply as u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        // output.push(*register_1 as u8);
                        // output.push(*register_2 as u8);
                        output.push(register_id as u8);
                    }
                    // println!("adding opcode Multiply {:?}", Opcode::Multiply as u8);
                    // output.push(variable_registers[order_id].unwrap() as u8);
                    // output.push(variable_registers[instruction_id]);
                }
                InstructionData::Divide(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        println!("decoded {:?}", instruction_id);
                        // decoded_instructions[*instruction_id] = Some(inserted_ids);
                        inserted_ids = output.len();
                        println!(" {:?} Adding divide opcode", order_id);
                        println!("{:?}\n{:?}\n{:?}", register_1, register_2, variable_registers[order_id]);
                        println!("---");
                        output.push(Opcode::Divide as  u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        // output.push(*register_1 as u8);
                        // output.push(*register_2 as u8);
                        output.push(register_id as u8);
                    }
                    println!("adding opcode divide {:?}", Opcode::Divide as u8);
                    // output.push(variable_registers[instruction_id]);
                }
                InstructionData::Remainder(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        println!("decoded {:?}", instruction_id);
                        // decoded_instructions[*instruction_id] = Some(inserted_ids);
                        inserted_ids = output.len();
                        println!(" {:?} Adding remainder opcode", order_id);
                        println!("{:?}\n{:?}\n{:?}", register_1, register_2, variable_registers[order_id]);
                        println!("---");
                        output.push(Opcode::Remainder as  u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        // output.push(*register_1 as u8);
                        // output.push(*register_2 as u8);
                        output.push(register_id as u8);
                    }
                    println!("adding opcode remainder {:?}", Opcode::Remainder as u8);
                    // output.push(variable_registers[instruction_id]);
                }
                InstructionData::CompareEqual(register_1, register_2) => {
                    if let Some(register_id) = variable_registers[order_id] {
                        inserted_ids = output.len();
                        output.push(Opcode::Equal as u8);
                        output.push(variable_registers[instruction_indices[*register_1]].unwrap() as u8);
                        output.push(variable_registers[instruction_indices[*register_2]].unwrap() as u8);
                        output.push(register_id as u8);
                    }

                }
                InstructionData::Jump(block) => {
                    // decoded_instructions[*instruction_id] = Some(inserted_ids);
                    // println!("decoded {:?}", instruction_id);
                    inserted_ids = output.len();
                    println!(" {:?} Adding jump opcode", order_id);
                    // println!("{:?} which should be {:?}", decoded_instructions[instruction_indices[self.blocks[*block].start.unwrap()]],self.blocks[*block].start);
                    println!("---");
                    // println!("adding opcode jump {:?}", Opcode::Jump as u8);
                    output.push(Opcode::Jump as u8);
                    // println!("Jump is {:?}", block)
                    // TODO: dont use this as block jump as this will jump t oan instruction
                    // NOT bytecode order!!!
                    // make it so that it setsl ikea  reminder or something that on this specific
                    // instruction, you need to go back and modify the jumps id to it
                    // like a jumplist  vec for example
                    // replace_instructions.push((self.blocks[*block].start.unwrap(), output.len()));
                    let mut start = blocks[*block].start.unwrap();
                    loop {
                        let instruction = &instructions[start];
                        match instruction.data {
                            InstructionData::Phi(_) | InstructionData::MakeLocal(_) => {
                                println!("{:?} in start START WAS A PHI! MOVIONG ON TO {:?}", instruction.id, instruction.next);
                                // NOTE: fi i were to remove makelocal then all i need to do is
                                // just get how many phis are ina block and skip p ast those in
                                // order to get to actual instructions
                                start = instruction.next.unwrap();
                            },
                            _ => break,
                        }
                    }
                    replace_instructions.push((start, output.len()));
                    output.push(98);
                    // output.push(decoded_instructions[instruction_indices[self.blocks[*block].start.unwrap()]].unwrap() as u8);
                    // output.push(decoded_instructions[self.blocks[*block].start.unwrap()].unwrap() as u8);
                }
                // NOTE: from my observations, only the false part of the jumpconditional is
                // important, as the truethy part will always be after the jumpconditional in post
                // order instructions

                InstructionData::JumpConditional(comparison, target_true, target_false) => {
                    println!("decoded {:?}", instruction_id);
                    // decoded_instructions[*instruction_id] = Some(inserted_ids);
                    inserted_ids = output.len();
                    println!(" {:?} Adding jump conditional opcode", order_id);
                    // println!("{:?} which should be {:?}", decoded_instructions[instruction_indices[blocks[*target].start.unwrap()]],blocks[*target].start);
                    println!("---");
                    // got the comparison as t is the instruction executed before
                    output.push(Opcode::JumpConditional as u8);
                    let mut true_start = blocks[*target_true].start.unwrap();
                    loop {
                        let instruction = &instructions[true_start];
                        println!("TRUE START IS {:?}", instruction.data);
                        match instruction.data {
                            InstructionData::Phi(_) | InstructionData::MakeLocal(_) => {
                                println!("{:?} in true start START WAS A PHI! MOVIONG ON TO {:?}", instruction.id, instruction.next);
                                // NOTE: fi i were to remove makelocal then all i need to do is
                                // just get how many phis are ina block and skip p ast those in
                                // order to get to actual instructions
                                true_start = instruction.next.unwrap();
                            },
                            _ => break,
                        }
                    }
                    replace_instructions.push((true_start, output.len()));
                    println!("''''''''''''''''''''''''''''''' I NEED A JUMP CONDITIONAL FALSE IN {:?}", output.len());
                    output.push(97);
                    // output.push(true_start);
                    let mut false_start = blocks[*target_false].start.unwrap();
                    loop {
                        let instruction = &instructions[false_start];
                        println!("FALSE START IS {:?}", instruction.data);
                        match instruction.data {
                            InstructionData::Phi(_) | InstructionData::MakeLocal(_) => {
                                println!("{:?} in false start START WAS A PHI! MOVIONG ON TO {:?}", instruction.id, instruction.next);
                                // NOTE: fi i were to remove makelocal then all i need to do is
                                // just get how many phis are ina block and skip p ast those in
                                // order to get to actual instructions
                                false_start = instruction.next.unwrap();
                            },
                            _ => break,
                        }
                    }
                    replace_instructions.push((false_start, output.len()));
                    // output.push(self.blocks[*target_true].start.unwrap() as u8);
                    // replace_instructions.push((self.blocks[*target_false].start.unwrap(), output.len()));
                    // output.push(false_start as u8);
                    output.push(99);
                    // replace_instructions.push((self.blocks, output.len()));
                    // output.push(self.blocks[*target].start.unwrap() as u8);
                    // println!("got {:?}", decoded_instructions[self.blocks[*target_true].start.unwrap()]);
                    // println!("from index {:?}", self.blocks[*target_true].start.unwrap());
                    // output.push(decoded_instructions[self.blocks[*target_true].start.unwrap()].unwrap() as u8);
                    // output.push(decoded_instructions[self.blocks[*target_false].start.unwrap()].unwrap() as u8);
                    // output.push(decoded_instructions[instruction_indices[self.blocks[*target_false].start.unwrap()]].unwrap() as u8);
                }
                InstructionData::Print(target_instruction) => {
                    inserted_ids = output.len();

                    output.push(Opcode::Print as u8);
                    // output.push(*target_instruction as u8);
                    // output.push(instruction_indices[*target_instruction] as u8);
                    output.push(variable_registers[instruction_indices[*target_instruction]].unwrap() as u8);
                }
                InstructionData::Closure(function_id) => {


                }
                InstructionData::CallFunction(function_id, arguments) => {
                    // println!("ok so {:?}", instructions[*function_id].data);
                    // let InstructionData::Closure(closure_function_id) = instructions[*function_id].data else { panic!("not a closure") };;
                    // inserted_ids = output.len();
                    // println!("------------------------------=zzzzzzzzzzzzzzzzzzzzzzz function id {:?}", closure_function_id);
                    // output.push(Opcode::CallFunction as u8);
                    // output.push(*function_id as u8);
                }
                // InstructionData::Phi()
                e => {
                    // skipped_instructions += 1;
                    println!("Unimplemented {:?} with id of {:?}", e, instruction_id);
                    continue;
                }

            }
            println!("inserted ids {:?}", inserted_ids);
            let mut removed = None;
            for (index, location) in replace_instructions.iter().enumerate() {
                println!("matching {:?} with {:?}", instruction_id, location.0);
                if *instruction_id == location.0 {
                    println!("!!!!!!!!!!!! REPLACING INSTRUCTION {:?} with {:?}", location.1, inserted_ids); // NOTE: not instruction_id, repalce it with bytecode!!
                    println!("its of id {:?}", instruction_id);
                    println!("first {:?}", instructions[location.0].data);
                    println!("second {:?}", Opcode::from(output[inserted_ids]));
                    println!("the target is of {:?}", output[location.1]);
                    output[location.1] = inserted_ids as u8;
                    removed = Some(index);
                    // break;
                    // might need a break? though  several instructions might have a replace thing
                }
            }
            if let Some(index) = removed {
                replace_instructions.remove(index);
            }
            // decoded_instructions[*instruction_id] = Some(inserted_ids);
        }

        for location in &replace_instructions {
            println!("DIDNT REMOVE: {:?} (it has {:?})", location, instructions[location.0].data);
        }

        println!("bytecode: ");
        let mut skip_amount = 0;
        let mut index = 0;
        for text in output.iter() {
            // println!("          {:?}", text);
            if skip_amount > 0 {
                skip_amount -= 1;
                println!("          {:?}", text);
                continue;
            }
            println!(")");
            println!("----");
            match Opcode::from(*text) {
                Opcode::Load => skip_amount = 2,
                Opcode::Add => skip_amount = 3,
                Opcode::Subtract => skip_amount = 3,
                Opcode::Multiply => skip_amount = 3,
                Opcode::Divide => skip_amount = 3,
                Opcode::Remainder => skip_amount = 3,
                Opcode::Jump => skip_amount = 1,
                Opcode::Equal => skip_amount = 3,
                Opcode::NotEqual => skip_amount = 3,
                Opcode::GreaterThan => skip_amount = 3,
                Opcode::GreaterThanOrEqual => skip_amount = 3,
                Opcode::LessThan => skip_amount = 3,
                Opcode::LessThanOrEqual => skip_amount = 3,
                Opcode::JumpIfEqual => skip_amount = 1,
                Opcode::JumpConditional => skip_amount = 2,
                Opcode::Print => skip_amount = 1,
                _ => {}
            }
            index += 1;
            println!("{:?}:    {:?}", index, Opcode::from(*text));
            println!("(");
        }
        // NOTE: i could do an optimization pass that would check if a jump instruction merely goes
        // 1 instruction further
        // aka
        // 11 instruction: JumpConditional(12; 14)
        // 12 instruction: load 2
        // 13 instruction: jump 14 // this jump is not needed
        // 14 instruction: load 5
        println!(")");

        output
    }
    // pub fn compile_bytecode(&self, post_order: Vec<usize>, immediate_dominators: IndexVec<usize, usize>, dominance_tree: &IndexVec<usize, Vec<usize>>) -> Vec<u8> {
    //
    //
    // }
}


impl Instruction {
    fn has_value(&self) -> bool {
        match self.data {

            InstructionData::SetVariable(_, _) |
            InstructionData::Jump(_) |
            InstructionData::MakeLocal(_) => false,
            _ => true
        }
    }
    fn needs_register_output(&self) -> bool {
        match self.data {

            InstructionData::GetVariable(_) | // shouldnt exist mostly
            InstructionData::Jump(_) |
            InstructionData::MakeLocal(_) | // IMPORTANT: should not exist in output bytecode
            InstructionData::JumpConditional(_, _, _) => false,
            _ => true
        }
    }
}

impl Block {

    pub fn get_successors<F: FnMut(
        usize,
        &IndexVec<usize, Instruction>,
    )>(
        &self,
        instructions: &IndexVec<usize, Instruction>,
        mut callback: F
    ) {
        match instructions[self.end.unwrap()].data {
            InstructionData::Jump(target) => callback(target, instructions),
            InstructionData::JumpConditional(_, target_1, target_2) => {
                callback(target_1, instructions);
                callback(target_2, instructions)
            }
            InstructionData::EndOfFile => {}
            // _ => unimplemented!("tried calling successors on an unterminated block"),
            _ => println!("unterminated block right here {:?}", self.id),
        }
    }

    pub fn for_instruction_mut<F: FnMut(usize, &mut IndexVec<usize, Instruction>, &mut Block)>(
        &mut self,
        instructions: &mut IndexVec<usize, Instruction>,
        mut callback: F
    ) {
        let mut at = self.start;
        while let Some(id) = at {
            // let instruction = ;
            callback(id, instructions, self);

            at = instructions[id].next;
        }
    }
}
