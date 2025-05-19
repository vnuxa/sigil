// use crate::bytecode::main::Opcode;

use std::{collections::HashMap, rc::Rc};

use chumsky::container::{Container, Seq};

use crate::index_vec::{IndexVec, index_vec};

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
    SetVariable(usize, usize), // variable id
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
}

#[derive(Clone)]
pub struct Instruction {
    pub data: InstructionData,
    pub block: Option<usize>,
    pub next: Option<usize>,
    pub previous: Option<usize>,
    pub id: usize,
}

pub struct Module {
    // pub blocks: Vec<Block>,
    pub blocks: Vec<Block>,
    pub variables: IndexVec<usize, String>,
    pub instructions: IndexVec<usize, Instruction>,
    pub variable_definitions: IndexVec<usize, Vec<usize>>, // variable index -  blocks that define it
    pub phi_instructions: IndexVec<usize, Vec<(usize, usize)>>, // Vec<block_id, instruction_id>
                                                           // pub phi_instructions: IndexVec<usize, Vec<usize>>, // IndexVec<block_id, // Vec<isntruction_id>
}

impl Module {
    pub fn new() -> Self {
        Self {
            // blocks: Vec::new(),
            blocks: Vec::new(),
            variables: IndexVec::new(),
            variable_definitions: IndexVec::new(),
            instructions: IndexVec::new(),
            phi_instructions: IndexVec::new(),
        }
    }
    pub fn new_block(&mut self) -> usize {
        let id = self.blocks.len();
        let block = Block {
            // start: self.instructions.len(),
            start: None,
            end: None,
            size: 0,
            id,
        };
        // let block = Block {
        //     instructions: Vec::new(),
        //     // successors: Vec::new(),
        //     // predecessors: Vec::new(),
        //     id,
        // };
        self.blocks.push(block);

        id
    }

    pub fn add_instruction(&mut self, block_id: usize, instruction: InstructionData) -> usize {
        let block = &mut self.blocks[block_id];
        let old_last = block.end;

        let id = self.instructions.len();
        if let Some(end_id) = old_last {
            self.instructions[end_id].next = Some(id);
            block.end = Some(id);
            block.size += 1;
        } else {
            block.start = Some(id);
            block.end = Some(id);
            block.size = 1;
        }

        self.instructions.push(Instruction {
            data: instruction,
            previous: old_last,
            next: None,
            block: Some(block_id),
            id: self.instructions.len(),
        });

        id
    }

    // creates a new instruction, without adding it
    pub fn new_instruction(&mut self, instruction: InstructionData) -> usize {
        let id = self.instructions.len();

        self.instructions.push(Instruction {
            data: instruction,
            previous: None,
            next: None,
            block: None,
            id,
        });

        id
    }

    pub fn remove_instruction(&mut self, instruction_id: usize) -> Option<usize> {
        let instruction = &mut self.instructions[instruction_id];
        // let Some(instruction_block) = instruction.block else {
        //     return None;
        // };
        let block = &mut self.blocks[instruction.block?];

        let old_previous = instruction.previous;
        let old_next = instruction.next;
        instruction.previous = None;
        instruction.next = None;
        instruction.block = None;

        if let Some(previous) = old_previous {
            self.instructions[previous].next = old_next;
        } else {
            block.start = old_next;
        }

        if let Some(next) = old_next {
            self.instructions[next].previous = old_previous;
        } else {
            block.end = old_previous;
        }

        block.size -= 1;

        old_next
    }

    pub fn insert_instruction(
        &mut self,
        block_id: usize,
        reference: Option<usize>,
        instruction_id: usize,
    ) {
        let block = &mut self.blocks[block_id];

        let (previous, next) = {
            if let Some(reference_id) = reference {
                (Some(reference_id), self.instructions[reference_id].next)
            } else {
                (None, block.start)
            }
        };

        let instruction = &mut self.instructions[instruction_id];

        instruction.block = Some(block_id);
        instruction.previous = previous;
        instruction.next = next;
        block.size += 1;

        if let Some(prev) = previous {
            self.instructions[prev].next = Some(instruction_id);
        } else {
            block.start = Some(instruction_id);
        }

        if let Some(next) = next {
            self.instructions[next].previous = Some(instruction_id);
        } else {
            block.end = Some(instruction_id);
        }
    }

    pub fn instruction_args<F: FnMut(usize)>(
        &self,
        instruction: &InstructionData,
        mut callback: F,
    ) {
        match instruction {
            InstructionData::Copy(data) => callback(*data),
            InstructionData::Phi(map_id) => {
                // let mut map = core::mem::take(&self.phi_instructions[map_id]);

                // let map = &mut module.phi_instructions[map_id];
                for (_, data) in &self.phi_instructions[*map_id] {
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
    pub fn replace_instruction<F: FnMut(&Module, &mut usize)>(
        &mut self,
        instruction: &mut InstructionData,
        mut callback: F,
    ) {
        match instruction {
            InstructionData::Copy(data) => callback(self, data),
            InstructionData::Phi(map_id) => {
                let mut map = core::mem::take(&mut self.phi_instructions[*map_id]);

                // let map = &mut module.phi_instructions[map_id];
                for (_, data) in &mut map {
                    println!("              within phi \\/");
                    callback(self, data);
                }
                println!("ok data after: {:?}", map);

                self.phi_instructions[*map_id] = map;
            }
            InstructionData::Add(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }
            InstructionData::Subtract(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }
            InstructionData::Multiply(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }
            InstructionData::Divide(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }
            InstructionData::Remainder(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }
            InstructionData::CompareEqual(num_1, num_2) => {
                callback(self, num_1);
                callback(self, num_2);
            }

            InstructionData::JumpConditional(instruction, _, _) => callback(self, instruction),

            // TODO: add more comparisons, return, calls, tuples, lists here too
            _ => {}
        }
    }
    pub fn for_block<F: FnMut(&Instruction)>(&self, block_id: usize, mut callback: F) {
        let mut at = self.blocks[block_id].start;
        while let Some(id) = at {
            let instruction = &self.instructions[id];
            callback(instruction);

            at = instruction.next;
        }
    }
    pub fn for_block_mut<F: FnMut(&mut Instruction)>(&mut self, block_id: usize, mut callback: F) {
        let mut at = self.blocks[block_id].start;
        while let Some(id) = at {
            let instruction = &mut self.instructions[id];
            callback(instruction);

            at = instruction.next;
        }
    }

    // loops over every instruction and runs a function for every single argument they have
    pub fn block_replace_args<F: FnMut(&Module, &mut usize)>(
        &mut self,
        block_id: usize,
        mut callback: F,
    ) {
        let mut at = self.blocks[block_id].start;
        while let Some(id) = at {
            // TODO: might not have to clone?
            let mut data = self.instructions[id].data.clone();
            println!("before: {:?}", self.instructions[id].data);
            self.replace_instruction(&mut data, &mut callback);
            println!("ok after: {:?}", self.instructions[id].data);
            // callback(self, instruction.id);

            let instr = &mut self.instructions[id];
            instr.data = data;
            at = instr.next;
            // at = self.instructions[id].next;
        }
    }

    pub fn all_args<F: FnMut(usize)>(&self, mut callback: F) {
        for block in &self.blocks {
            let mut at = block.start;
            while let Some(id) = at {
                let instruction = &self.instructions[id];
                self.instruction_args(&instruction.data, &mut callback);

                at = instruction.next;
            }
        }
    }

    pub fn get_successors<F: FnMut(usize)>(&self, block_id: usize, mut callback: F) {
        let block = &self.blocks[block_id];

        // let last_instruction = block.instructions.last().unwrap();

        match self.instructions[block.end.unwrap()].data {
            InstructionData::Jump(target) => callback(target),
            InstructionData::JumpConditional(_, target_1, target_2) => {
                callback(target_1);
                callback(target_2)
            }
            InstructionData::EndOfFile => {}
            _ => unimplemented!("tried calling successors on an unterminated block"),
        }
    }

    pub fn get_predecessors(&self) -> IndexVec<usize, IndexVec<usize, usize>> {
        let mut predecessors = index_vec!(IndexVec::new(); self.blocks.len());

        for block in &self.blocks {
            self.get_successors(block.id, |successor| predecessors[successor].push(block.id));
        }

        predecessors
    }
    pub fn build_instruction(
        &mut self,
        context_id: usize,
        expressions: Expressions,
    ) -> Option<usize> {
        match expressions {
            Expressions::Int(num) => {
                self.add_instruction(context_id, InstructionData::LoadNumber(num));
            }
            Expressions::Bool(bool) => {
                self.add_instruction(context_id, InstructionData::LoadBool(bool));
            }
            Expressions::String(value) => {
                self.add_instruction(context_id, InstructionData::LoadString(value));
            }
            Expressions::Variable(value) => {
                self.add_instruction(
                    context_id,
                    InstructionData::GetVariable(self.variables.len() - 1),
                );
            }
            Expressions::LocalDefine(name, value) => {
                let Expressions::Variable(var_name) = *name else {
                    panic!()
                };

                self.add_instruction(context_id, InstructionData::MakeLocal(var_name.clone()));
                // context.instructions.push();

                // IMPORTANT: make a way to add to the variable_defintions
                // and a way to track current block?
                if let Some(value) = value {
                    let _ = self.build_instruction(context_id, *value);
                    let context = &self.blocks[context_id];
                    self.variables.push(var_name);
                    self.add_instruction(
                        context.id,
                        InstructionData::SetVariable(
                            self.variables.len() - 1,
                            // context.instructions.len() - 1,
                            context.end.unwrap(),
                        ),
                    );
                };
            }
            Expressions::Assign(name, value) => {
                self.build_instruction(context_id, *value);
                let mut id = None;
                for (index, value) in self.variables.iter().enumerate() {
                    if name == *value {
                        id = Some(index);
                        break;
                    }
                }

                let context = &self.blocks[context_id];
                self.add_instruction(
                    context.id,
                    InstructionData::SetVariable(
                        id.unwrap(),
                        context.end.unwrap(),
                        // context.instructions.len(),
                    ),
                );
                // self.variables.push(name);
                // context.instructions.push(Instruction::SetVariable(
                //     id.unwrap(),
                //     context.instructions.len(),
                // ));
            }
            Expressions::Add(left_side, right_side) => {
                self.build_instruction(context_id, *left_side);
                self.build_instruction(context_id, *right_side);
                let context = &self.blocks[context_id];
                // let length = context.instructions.len() - 1;

                // println!("CONTEXT LEN: {:?}", self.blocks[context.id].size);
                self.add_instruction(
                    context.id,
                    InstructionData::Add(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Subtract(left_side, right_side) => {
                self.build_instruction(context_id, *left_side);
                self.build_instruction(context_id, *right_side);
                let context = &self.blocks[context_id];

                self.add_instruction(
                    context.id,
                    InstructionData::Subtract(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Multiply(left_side, right_side) => {
                self.build_instruction(context_id, *left_side);
                self.build_instruction(context_id, *right_side);

                let context = &self.blocks[context_id];
                self.add_instruction(
                    context.id,
                    InstructionData::Multiply(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Divide(left_side, right_side) => {
                self.build_instruction(context_id, *left_side);
                self.build_instruction(context_id, *right_side);
                let context = &self.blocks[context_id];

                self.add_instruction(
                    context.id,
                    InstructionData::Divide(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Remainder(left_side, right_side) => {
                self.build_instruction(context_id, *left_side);
                self.build_instruction(context_id, *right_side);
                let context = &self.blocks[context_id];

                self.add_instruction(
                    context.id,
                    InstructionData::Remainder(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    ),
                );
            }
            Expressions::Scope(new_scope) => {
                let mut new_block_id =
                // if let None = self.blocks.last().unwrap().instructions.last() {
                //     self.blocks.len() - 1
                // } else {
                    self.new_block();
                // };

                // println!("NEW SCOPE!!!!!!!!!!!!!");
                // let mut predecessors = context.predecessors.clone();
                // predecessors.push(context.id);
                // let new_block_id = self.new_block_with(context);
                // let mut new_block = self.blocks[new_block_id].clone();

                // println!(
                //     "the predecessors of that block is {:?}",
                //     new_block.predecessors
                // );
                // println!(
                //     "the successors of this block is {:?}",
                //     context.successors // self.blocks[context.id].successors
                // );
                // TODO: make ti so that this new block will only hjave direct predecessors
                // as currently, it iwll contain the asme predecors as previopus bloick and the
                // block aferr that and etc..
                // and make it so that there can be predecessoors of this new block too!
                // so that it doesnt always just use the same ol
                // let mut new_block = self.blocks[self.blocks.len() - 1].clone();
                // let index = new_block.id;
                let mut first_id = new_block_id;
                for expression in new_scope {
                    // let new_block = &mut self.blocks[new_block_id];
                    if let Some(obj) = self.build_instruction(new_block_id, expression) {
                        if self.blocks[self.blocks.len() - 1].size == 0 {
                            self.blocks.pop();
                        }
                        // self.blocks[new_block_id] = new_block;
                        // new_block_id = self.new_block();
                        // new_block = self.blocks[new_block_id].clone();
                        new_block_id = self.new_block();
                    }

                    // println!("wow the instruction was {:?}", wow);
                }

                // TODO: best case in performance would be not to have these blocks that are made by the if
                // function that are untracked in the first place
                if self.blocks[new_block_id].size == 0 {
                    self.blocks.swap_remove(new_block_id);
                }
                // if !new_block.instructions.is_empty() {
                // } else {
                //     self.blocks.swap_remove(new_block_id);
                // }

                // context.successors.push(self.blocks.len());

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
                let first_scope = self.build_instruction(context_id, *conditional);
                let scope = self.build_instruction(context_id, *scope).unwrap();
                let mut new_block = self.new_block();
                let context = &self.blocks[context_id];

                self.add_instruction(
                    context.id,
                    InstructionData::JumpConditional(
                        context.end.unwrap(),
                        // context.instructions.len() - 1,
                        scope,
                        self.blocks.len() - 1, // self.blocks.len() - 1,
                    ),
                );
                // context.instructions.push(Instruction::JumpConditional(
                //     context.instructions.len() - 1,
                //     scope,
                //     self.blocks.len() - 1, // self.blocks.len() - 1,
                // ));

                println!("RETURNING BLOCK: {:?}", new_block);
                // make it jump to the new block made thats after the if scope
                // if the condition was false

                return Some(new_block);
            }
            Expressions::CompareEqual(primary_variable, other_variable) => {
                // maybe make a new block for the condition though i dont think that is nesscessary
                // let condition
                self.build_instruction(context_id, *primary_variable);
                self.build_instruction(context_id, *other_variable);
                let context = &self.blocks[context_id];

                self.add_instruction(
                    context.id,
                    InstructionData::CompareEqual(
                        self.instructions[context.end.unwrap()].previous.unwrap(),
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
            e => {
                println!("instruction {:?}, not yet implemented", e);
            }
        }

        None
        // should change block?
        // None
        // context.instructions.push(instruction);
    }

    pub fn build_dominance(
        &self,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
    ) -> IndexVec<usize, usize> {
        // dominance - block id, vector of block ids
        // TODO: i dont know if this is correct
        // but iterate over reverse *post* order
        // due to this, the language cannot have a `goto` statement
        // should look into adding something similar to a goto, or fulfills a similar purpose

        let mut changed = true;

        // let post_order = vec![0, 1, 4, 5, 2, 3, 6];
        let post_order = {
            fn visit(
                module: &Module,
                block_id: usize,
                post_order: &mut Vec<usize>,
                visited: &mut IndexVec<usize, bool>,
            ) {
                post_order.push(block_id);
                module.get_successors(block_id, |successor| {
                    println!("succesor {:?}", successor);
                    if !visited[successor] {
                        visited[successor] = true;
                        visit(module, successor, post_order, visited);
                    }
                });
            }

            let mut post_order = Vec::new(); //maybe make it wiht the capacity of all blocks?
            let mut visited = index_vec!(false; self.blocks.len());

            visit(self, 0, &mut post_order, &mut visited);

            post_order
        };

        let post_indices = {
            let mut indices = index_vec!(None; self.blocks.len());
            println!("BLOCK LEN: {:?}", self.blocks.len());
            for thing in &self.blocks {
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

                let block = &self.blocks[block_id];
                let preds = &predecessors[block_id];

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
        let mut output = IndexVec::with_capacity(self.blocks.len());
        for block in &self.blocks {
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
        dominance: &IndexVec<usize, usize>,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
    ) -> IndexVec<usize, Vec<usize>> {
        let mut frontier = index_vec!(vec![]; self.blocks.len());

        for dommy in dominance {
            println!("!!! immediate dommy {:?}", dommy);
        }
        for block in &self.blocks {
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
        dominance: IndexVec<usize, usize>,
    ) -> IndexVec<usize, Vec<usize>> {
        let mut tree = index_vec![Vec::new(); self.blocks.len()];

        for block in self.blocks.iter().skip(1) {
            if let Some(dominator) = dominance.get(block.id) {
                tree[*dominator].push(block.id);
            }
        }

        tree
    }

    pub fn make_ssa(
        &mut self,
        dominance_frontier: IndexVec<usize, Vec<usize>>,
        dominance_tree: &IndexVec<usize, Vec<usize>>,
        predecessors: &IndexVec<usize, IndexVec<usize, usize>>,
    ) {
        let mut phis = {
            let mut visited =
                index_vec!(index_vec!(false; self.variables.len()); self.blocks.len());

            let mut stack = Vec::new();
            for block in &self.blocks {
                self.for_block(block.id, |instruction| {
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
                index_vec!(vec![]; self.blocks.len());

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
                        let phi = self.new_instruction(InstructionData::Phi(0));
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
                mut new_names: IndexVec<usize, Option<usize>>,
                phis: &mut IndexVec<usize, Vec<(usize, Vec<(usize, Option<usize>)>, usize)>>,
                // IndexVec<usize, Vec<(usize, IndexVec<usize, usize>, usize)>>,
                dominance_tree: &IndexVec<usize, Vec<usize>>,
                module: &mut Module,
            ) {
                let block = &mut module.blocks[block_id];
                for (variable_id, _map, instruction) in &phis[block.id] {
                    new_names[*variable_id] = Some(*instruction);
                }

                // println!("the length of instructions: {:?}", block.instructions.len());
                // println!("block id is {:?}", block.id);
                // let mut index = 0;
                module.for_block_mut(block_id, |instruction| {
                    match instruction.data {
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
                            new_names[id] = Some(instruction.id);
                            println!(
                                "ok its after set and the value for both is: old: {:?} | new: {:?}",
                                new_name, new_names[id]
                            );
                            instruction.data = InstructionData::Copy(new_name);
                        }

                        InstructionData::SetVariable(id, to_value) => {
                            println!("set var to id: {:?} and {:?}", id, to_value);
                            new_names[id] = Some(instruction.id);
                            instruction.data = InstructionData::Copy(to_value);
                        } // TODO: make it so that every use of variable will d this
                        // i think this might be enough? but idk
                        _ => {}
                    }
                    // index += 1;
                });
                // for (index, mut instruction) in block.instructions.iter_mut().enumerate() {
                // index += 1;
                // }

                // let mut map_size = 0;
                module.get_successors(block_id, |sucessor| {
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
                    visit(*dominated, new_names.clone(), phis, dominance_tree, module);
                }
            }

            let mut new_names = index_vec!(None; self.variables.len());
            visit(0, new_names, &mut phis, dominance_tree, self);
        }
        println!("IN THE SSA THINGY");
        for block in &self.blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            self.for_block(block.id, |instruction| {
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
                    self.instructions[*instruction_id].data =
                        InstructionData::Phi(self.phi_instructions.len());
                    self.insert_instruction(block_index, at, *instruction_id);
                    // self.phi_instructions.len()
                    self.phi_instructions.push(map);

                    at = Some(*instruction_id);
                }
            }
        }
    }

    pub fn copy_propogation(&mut self, dominance_tree: &IndexVec<usize, Vec<usize>>) {
        fn visit(
            block_id: usize,
            module: &mut Module,
            dominance_tree: &IndexVec<usize, Vec<usize>>,
        ) {
            module.block_replace_args(block_id, |new_module, instruction_id| {
                if let InstructionData::Copy(source) = new_module.instructions[*instruction_id].data
                {
                    println!(
                        "i see source {:?} for instruction id {:?}",
                        source, instruction_id
                    );
                    let mut new_arguments = source;
                    while let InstructionData::Copy(source) =
                        new_module.instructions[new_arguments].data
                    {
                        new_arguments = source;
                    }
                    println!("      new instruction id {:?}", new_arguments);

                    *instruction_id = new_arguments;
                }
            });

            for dominated in dominance_tree[block_id].iter() {
                visit(*dominated, module, dominance_tree);
            }
            // module.for_block_mut(block_id, |instruction| {});
        }

        visit(0, self, dominance_tree);

        println!("ok within this thing lets see ||||||| ");

        for block in &self.blocks {
            println!("im in block: {:?}, it has len: {:?}", block.id, block.size);
            self.for_block(block.id, |instruction| {
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

        for phi in &self.phi_instructions {
            println!("theres a phi instruction! {:?}", phi);
        }
    }

    pub fn dead_copy_elimination(&mut self) {
        let mut visited = index_vec![false; self.instructions.len()];
        self.all_args(|argument| {
            println!("visited {:?}", argument);
            visited[argument] = true;
        });

        for block_id in 0..self.blocks.len() {
            let mut current_block = self.blocks[block_id].start;
            // let mut at = current_block.start;

            while let Some(instruction_id) = current_block {
                let instruction = &self.instructions[instruction_id];
                let next = instruction.next;
                println!(
                    "checking {:?} and {:?} the data {:?}",
                    instruction_id, visited[instruction_id], instruction.data
                );
                if !visited[instruction_id] {
                    if let InstructionData::Copy(source) = instruction.data {
                        // let source_value = core::mem::take(&mut source.)
                        println!("removing copy {:?}", instruction_id);
                        self.remove_instruction(instruction_id);
                    }
                }

                current_block = next;
            }
        }
    }

    // convert to conventional ssa form
    pub fn convert_to_cssa(&mut self, predecessors: &IndexVec<usize, IndexVec<usize, usize>>) {
        let mut predecessor_copy_ids = index_vec![None; self.blocks.len()];

        // for block_id in 0..self.bl {
        //
        // }
        let mut last_parrallel_id = 0;
        for block in &self.blocks {
            if let InstructionData::Phi(first_phi) = self.instructions[block.start.unwrap()].data {
                let phi_parralel_copy = last_parrallel_id;
                last_parrallel_id += 1;

                for predecessor in &predecessors[block.id] {
                    predecessor_copy_ids[*predecessor] = Some(last_parrallel_id);
                    last_parrallel_id += 1;
                }

                let mut old_phi = Some(first_phi);
                while let Some(at) = old_phi {
                    let map = &mut self.phi_instructions[at];

                    // parallel copies for each predecessor
                    // let mut phi_map = phima
                    for (predecessor, source) in map {
                        let copy_id = predecessor_copy_ids[*predecessor].unwrap();
                        let copy =
                            self.new_instruction(InstructionData::ParallelCopy(*source, copy_id));
                    }
                }
                // let mut new_phi_cursor
            }
        }
    }
}
