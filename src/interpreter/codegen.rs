// use crate::bytecode::main::Opcode;

use std::{collections::HashMap, rc::Rc};

use chumsky::container::{Container, Seq};

use crate::index_vec::{IndexVec, index_vec};

use super::parser::Expressions;

// pub fn to_bytecode(expression: Expressions) -> Opcode {}

#[derive(Clone, Debug)]
pub enum Instruction {
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
    EndOfFile,
}

pub enum Value {
    String(String),
    Number(isize),
    Bool(bool),
}

#[derive(Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub id: usize,
    // pub successors: Vec<usize>,
    // pub predecessors: Vec<usize>, // mgiht be useless?
}

pub struct Module {
    pub blocks: Vec<Block>,
    pub variables: IndexVec<usize, String>,
    pub variable_definitions: IndexVec<usize, Vec<usize>>, // variable index -  blocks that define it
    pub phi_instructions: IndexVec<usize, Vec<(usize, usize)>>, // Vec<block_id, instruction_id>
                                                           // pub phi_instructions: IndexVec<usize, Vec<usize>>, // IndexVec<block_id, // Vec<isntruction_id>
}

impl Module {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            variables: IndexVec::new(),
            variable_definitions: IndexVec::new(),
            phi_instructions: IndexVec::new(),
        }
    }
    pub fn new_block(&mut self) -> usize {
        let id = self.blocks.len();
        let block = Block {
            instructions: Vec::new(),
            // successors: Vec::new(),
            // predecessors: Vec::new(),
            id,
        };
        self.blocks.push(block);

        id
    }

    // pub fn new_block_with(
    //     &mut self,
    //     // predecessors: Vec<usize>,
    //     current_block: &mut Block,
    // ) -> usize {
    //     let id = self.blocks.len();
    //     // for block_id in &current_block.predecessors {
    //     //     println!("changing predeceessor: {:?} and adding: {:?}", block_id, id);
    //     //     self.blocks[*block_id].successors.push(id);
    //     // }
    //     println!("applying succeosr {:?} to block {:?}", id, current_block.id);
    //     // current_block.successors.push(id);
    //     // if let Some(write_to) = option_write {
    //     //     write_to.successors.push(id);
    //     // }
    //     let block = Block {
    //         instructions: Vec::new(),
    //         // successors: Vec::new(),
    //         id,
    //         // predecessors: vec![current_block.id],
    //     };
    //     self.blocks.push(block);
    //
    //     id
    // }

    pub fn get_successors<F: FnMut(usize)>(&self, block_id: usize, mut callback: F) {
        let block = &self.blocks[block_id];

        let last_instruction = block.instructions.last().unwrap();

        match last_instruction {
            Instruction::Jump(target) => callback(*target),
            Instruction::JumpConditional(_, target_1, target_2) => {
                callback(*target_1);
                callback(*target_2)
            }
            Instruction::EndOfFile => {}
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
        context: &mut Block,
        expressions: Expressions,
    ) -> Option<usize> {
        match expressions {
            Expressions::Int(num) => {
                context.instructions.push(Instruction::LoadNumber(num));
            }
            Expressions::Bool(bool) => {
                context.instructions.push(Instruction::LoadBool(bool));
            }
            Expressions::String(value) => {
                context.instructions.push(Instruction::LoadString(value));
            }
            Expressions::Variable(value) => {
                // self.variables.push(value);
                context
                    .instructions
                    .push(Instruction::GetVariable(self.variables.len() - 1));
            }
            Expressions::LocalDefine(name, value) => {
                let Expressions::Variable(var_name) = *name else {
                    panic!()
                };

                context
                    .instructions
                    .push(Instruction::MakeLocal(var_name.clone()));

                // IMPORTANT: make a way to add to the variable_defintions
                // and a way to track current block?
                if let Some(value) = value {
                    let _ = self.build_instruction(context, *value);
                    self.variables.push(var_name);
                    context.instructions.push(Instruction::SetVariable(
                        self.variables.len() - 1,
                        context.instructions.len() - 1,
                    ));
                };
            }
            Expressions::Assign(name, value) => {
                self.build_instruction(context, *value);
                let mut id = None;
                for (index, value) in self.variables.iter().enumerate() {
                    if name == *value {
                        id = Some(index);
                        break;
                    }
                }

                // self.variables.push(name);
                context.instructions.push(Instruction::SetVariable(
                    id.unwrap(),
                    context.instructions.len(),
                ));
            }
            Expressions::Add(left_side, right_side) => {
                self.build_instruction(context, *left_side);
                self.build_instruction(context, *right_side);
                let length = context.instructions.len() - 1;

                context
                    .instructions
                    .push(Instruction::Add(length - 1, length));
            }
            Expressions::Subtract(left_side, right_side) => {
                self.build_instruction(context, *left_side);
                self.build_instruction(context, *right_side);
                let length = context.instructions.len() - 1;

                context
                    .instructions
                    .push(Instruction::Subtract(length - 1, length));
            }
            Expressions::Multiply(left_side, right_side) => {
                self.build_instruction(context, *left_side);
                self.build_instruction(context, *right_side);
                let length = context.instructions.len() - 1;

                context
                    .instructions
                    .push(Instruction::Multiply(length - 1, length));
            }
            Expressions::Divide(left_side, right_side) => {
                self.build_instruction(context, *left_side);
                self.build_instruction(context, *right_side);
                let length = context.instructions.len() - 1;

                context
                    .instructions
                    .push(Instruction::Divide(length - 1, length));
            }
            Expressions::Remainder(left_side, right_side) => {
                self.build_instruction(context, *left_side);
                self.build_instruction(context, *right_side);
                let length = context.instructions.len() - 1;

                context
                    .instructions
                    .push(Instruction::Remainder(length - 1, length));
            }
            Expressions::Scope(new_scope) => {
                let mut new_block_id =
                // if let None = self.blocks.last().unwrap().instructions.last() {
                //     self.blocks.len() - 1
                // } else {
                    self.new_block();
                // };
                let mut new_block = self.blocks[new_block_id].clone();

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
                    if let Some(obj) = self.build_instruction(&mut new_block, expression) {
                        if self.blocks[self.blocks.len() - 1].instructions.is_empty() {
                            self.blocks.pop();
                        }
                        self.blocks[new_block_id] = new_block;
                        new_block_id = self.new_block();
                        new_block = self.blocks[new_block_id].clone();
                    }

                    // println!("wow the instruction was {:?}", wow);
                }

                // TODO: best case in performance would be not to have these blocks that are made by the if
                // function that are untracked in the first place
                if new_block.instructions.is_empty() {
                    self.blocks.swap_remove(new_block_id);
                } else {
                    self.blocks[new_block_id] = new_block;
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
                let first_scope = self.build_instruction(context, *conditional);
                let scope = self.build_instruction(context, *scope).unwrap();
                let mut new_block = self.new_block();

                // self.blocks[new_block]
                //     .instructions
                //     .push(Instruction::LoadString("marked!".to_string()));
                // let new_block = if let None = self.blocks.last().unwrap().instructions.last() {
                //     self.blocks.len() - 1
                // } else {
                //     self.new_block()
                // };

                // match self.blocks[scope].instructions.last() {
                //     Some(Instruction::Jump(_) | Instruction::JumpConditional(_, _, _)) => {}
                //     Some(_) => {
                //         // add jump instruction
                //         self.blocks[scope]
                //             .instructions
                //             .push(Instruction::Jump(new_block));
                //     }
                //     None => {
                //         self.blocks[scope]
                //             .instructions
                //             .push(Instruction::Jump(new_block));
                //         // remove block
                //     }
                // }
                context.instructions.push(Instruction::JumpConditional(
                    context.instructions.len() - 1,
                    scope,
                    self.blocks.len() - 1, // self.blocks.len() - 1,
                ));

                println!("RETURNING BLOCK: {:?}", new_block);
                // make it jump to the new block made thats after the if scope
                // if the condition was false

                return Some(new_block);
            }
            Expressions::CompareEqual(primary_variable, other_variable) => {
                // maybe make a new block for the condition though i dont think that is nesscessary
                // let condition
                self.build_instruction(context, *primary_variable);
                self.build_instruction(context, *other_variable);

                context.instructions.push(Instruction::CompareEqual(
                    context.instructions.len() - 2,
                    context.instructions.len() - 1,
                    // context.instructions.len() - 1,
                ));
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
                for instruction in &block.instructions {
                    let Instruction::SetVariable(id, _) = instruction else {
                        continue;
                    };

                    if !visited[block.id][*id] {
                        visited[block.id][*id] = true;
                        stack.push((block.id, id));
                    }
                }
            }

            let mut phis: IndexVec<usize, Vec<(usize, Vec<(usize, Option<usize>)>, usize)>> =
                index_vec!(vec![]; self.blocks.len());

            while let Some((from_block, variable_id)) = stack.pop() {
                for to_block in dominance_frontier[from_block].iter().copied() {
                    let mut to_phis = &mut phis[to_block];

                    // checks if this variable does not have a phi instruction already added within
                    // this block
                    let is_new = to_phis
                        .iter()
                        .find(|(num, _, _)| num == variable_id)
                        .is_none();
                    if is_new {
                        let predecessor = &predecessors[to_block];
                        // let predecessor = &block_predecessors;
                        // variable_id isntead??

                        let map = predecessor.iter().map(|pred| (*pred, None)).collect();
                        let phi = Instruction::Phi(self.phi_instructions.len());

                        // TODO: try not to clone
                        to_phis.push((
                            *variable_id,
                            map,
                            // predecessor.clone(),
                            self.phi_instructions.len(),
                        ));

                        // to_block now defines a variable
                        if !visited[from_block][*variable_id] {
                            visited[from_block][*variable_id] = true;
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
                for (index, mut instruction) in block.instructions.iter_mut().enumerate() {
                    match instruction {
                        // IMPORTANT: instructions might not be proprely applying due to
                        // cloning
                        Instruction::GetVariable(id) => {
                            println!("got it for variable {:?}", id);
                            for variable in &module.variables {
                                println!("      {:?}", variable);
                            }
                            for variable in &new_names {
                                println!("      new name: {:?}", variable);
                            }
                            println!("current index: {:?}", index);
                            let new_name = new_names[*id].unwrap();
                            println!("new name: {:?}", new_name);
                            // TOOD: make a copy instruction
                            new_names[*id] = Some(index);
                            println!(
                                "ok its after set and the value for both is: old: {:?} | new: {:?}",
                                new_name, new_names[*id]
                            );
                            instruction = &mut Instruction::Copy(new_name);
                        }

                        Instruction::SetVariable(id, to_value) => {
                            println!("set var to id: {:?} and {:?}", id, to_value);
                            new_names[*id] = Some(index);
                            instruction = &mut Instruction::Copy(*to_value);
                        } // TODO: make it so that every use of variable will d this
                        // i think this might be enough? but idk
                        _ => {}
                    }
                    // index += 1;
                }

                module.get_successors(block_id, |sucessor| {
                    for (variable_id, map, _) in &mut phis[sucessor] {
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

                    // self.phi_instructions[instruction_id]

                    println!("GOT BLOCK ID {:?}", block_id);
                    if let Some(reference_id) = at {
                        // append
                        self.blocks[block_index]
                            .instructions
                            .insert(reference_id, Instruction::Phi(*block_id));
                    } else {
                        // prepend
                        self.blocks[block_index]
                            .instructions
                            .insert(0, Instruction::Phi(*block_id));
                    };

                    at = Some(*instruction_id);
                    // self.blocks[*block_id]

                    // self.blocks[*block_id]
                    //     .instructions
                    //     .insert(*instruction_id, Instruction::Phi(block_index)); // might need to
                }
            }
        }
    }

    // pub fn make_ssa(&mut self, dominance_frontier: IndexVec<usize, Vec<usize>>) {
    //     // adding phi's
    //     let phis = {
    //         let mut visited = Vec::with_capacity(self.variable_definitions.len());
    //         // let mut visited =
    //         //     index_vec![index_vec![false; self.variables.len()]; self.blocks.len()];
    //
    //         // let mut stack = Vec::new();
    //         //
    //         // for block in self.blocks {
    //         //     for instruction in block.instructions {
    //         //         let Instruction::SetVariable(name, _) = instruction else {
    //         //             continue;
    //         //         };
    //         //
    //         //         if !visited[block.id][] {
    //         //             visited
    //         //         }
    //         //     }
    //         // }
    //         //
    //         for (variable_index, variable) in self.variables.iter().enumerate() {
    //             // TODO: optimally, remove the clone
    //             for blocks in self.variable_definitions[variable_index].clone() {
    //                 for block in &dominance_frontier[blocks] {
    //                     // maybe use a hashmap instead?
    //                     if !visited.contains(block) {
    //                         // add the phi instruction
    //                         self.blocks[*block]
    //                             .instructions
    //                             .push(Instruction::Phi(*block));
    //
    //                         // self.blocks[*block].push(Instruction::Phi(*block));
    //
    //                         visited.push(*block);
    //                         if self.variable_definitions.get(*block).is_some() {
    //                             self.variable_definitions[variable_index].push(*block);
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //
    //         visited
    //     };
    //
    //     // renaming variables
    //
    //     {
    //         fn parse_variable(mut stack: HashMap<String, usize>, mut instruction: Instruction) {
    //             let Instruction::GetVariable(mut name) = instruction else {
    //                 return;
    //             };
    //             // Instruction::GetVariable(mut name) => {
    //             name.push_str(&stack.get(&name).unwrap_or(&0).to_string());
    //             instruction = Instruction::GetVariable(name);
    //             // }
    //             // Instruction::SetVariable(mut name, mut value) => {
    //             //     visit_instruction(block, stack, block.instructions[value]);
    //             //     // name.push_str(&stack.get(&name).unwrap_or(&0).to_string());
    //             //     instruction = Instruction::GetVariable(name.to_string());
    //             // }
    //         }
    //
    //         // just do this...
    //         fn parse_value(
    //             stack: &mut HashMap<String, usize>,
    //             block: &mut Block,
    //             phis: &Vec<usize>,
    //             instruction: usize,
    //         ) {
    //             // need to traverse through every single itme that continues a statment!
    //             // try to remove the clone in the future
    //             match block.instructions[instruction].clone() {
    //                 Instruction::Add(value_1, value_2) => {
    //                     parse_value(stack, block, phis, value_1);
    //                     parse_value(stack, block, phis, value_2);
    //                 }
    //                 Instruction::Subtract(value_1, value_2) => {
    //                     parse_value(stack, block, phis, value_1);
    //                     parse_value(stack, block, phis, value_2);
    //                 }
    //                 Instruction::GetVariable(mut name) => {
    //                     // Instruction::GetVariable(mut name) => {
    //                     name.push_str(&stack.get(&name).unwrap_or(&0).to_string());
    //                     block.instructions[instruction] =
    //                         Instruction::GetVariable(name.to_string());
    //                 }
    //                 Instruction::SetVariable(name, value) => {
    //                     parse_value(stack, block, phis, value);
    //                     if let Some(var) = stack.get_mut(&name) {
    //                         *var += 1;
    //                     } else {
    //                         // mayeb dont clone?
    //                         stack.insert(name.clone(), 1);
    //                     }
    //                     // TODO: make it so that it increments the name rthingy
    //                     block.instructions[instruction] =
    //                         Instruction::GetVariable(name + &value.to_string());
    //                 }
    //                 _ => {}
    //             }
    //
    //             if instruction < block.instructions.len() {
    //                 parse_value(stack, block, phis, instruction + 1);
    //             }
    //
    //             // TODO: rewrite tihs so maybe it does it in the reverse order with predecessors?
    //             // then all you need to track are predecesors
    //             for successor in &block.successors {
    //                 let test = 2 + 2;
    //             }
    //         }
    //         let mut stack = HashMap::with_capacity(self.variables.len());
    //         for block in &mut self.blocks {
    //             parse_value(&mut stack, block, &phis, 0);
    //         }
    //
    //         // fn visit(mut module: &mut Module, mut block: Block, mut stack: HashMap<String, usize>) {
    //         // for instruction in block.instructions {
    //         // parse_value(&mut stack, &mut block.instructions, 0);
    //         //     match instruction {
    //         //         Instruction::GetVariable(_) => parse_variable(stack, instruction),
    //         //         Instruction::SetVariable(mut name, mut value) => {
    //         //             // visit_instruction(block, stack, block.instructions[value]);
    //         //             // name.push_str(&stack.get(&name).unwrap_or(&0).to_string());
    //         //             instruction = Instruction::GetVariable(name.to_string());
    //         //         }
    //         //         _ => continue,
    //         //     }
    //         //     // visit_instruction(block, stack.clone(), instruction);
    //         // }
    //         // }
    //     }
    // }
}

// pub fn make_ssa(module: Module) {
//     // insert phi's
//     let mut phis = {};
// }
