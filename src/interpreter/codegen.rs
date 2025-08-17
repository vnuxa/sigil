use chumsky::prelude::skip_then_retry_until;

use crate::{bytecode::main::Opcode, index_vec::{index_vec, IndexVec}, interpreter::parser::Expressions};

#[derive(Clone, Debug)]
pub enum InstructionData {
    // Value(Value),
    LoadNumber(isize),
    LoadBool(bool),
    LoadFloat(f32), // might have to use f64?
    LoadString(String),

    MakeLocal(String), // name

    Upindex(usize, usize), // function id, instruction id
    SetUpindex(usize, usize), // to_value, upindex instruction id
    // SetUpindex(usize, usize, usize), // function id, origin

    // upvalues refer to variable ids
    Upvalue(usize, usize), // function id, varaible id
    SetUpvalue(usize, usize, usize), // function id, variable id, value
    GetVariable(usize),        // variable id
    SetVariable(usize, usize), // variable id, value
    Jump(usize),               // Jumps to a general instruction id
    Unimplemented,             // IMPORTANT: delete this once done

    MathOperands(MathVariant, usize, usize),
    ComparisonOperands(ComparisonVariant, usize, usize),
    Phi(usize), // id to a phi map, which contains a list of bloock id and instruction or variable id
    Copy(usize),
    JumpConditional(usize, usize, usize), // instruction id, jump if true, jump if false
    ParallelCopy(usize, usize),           // instruction_id, copy id
    Closure(usize, usize), // function id, variable_id
    Print(usize), // instruction id
    Return(Option<usize>), // returns a register?
    CallFunction(usize, Vec<usize>), // function id, vector of instruction ids as arguments!
    Param(usize), // a temporary value, with a variable i d, used for function arguments
    EndOfFile,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MathVariant {
    Add = 0,
    Subtract = 1,
    Multiply = 2,
    Divide = 3,
    Remainder = 4
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ComparisonVariant {
    Equal = 0,
    NotEqual = 1,
    LessEqual = 2,
    MoreEqual = 3,
    LessThan = 4,
    MoreThan = 5,
}
pub enum Value {
    String(String),
    Number(isize),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    Relative(usize),
    Absolute(usize),
    AbsoluteToFunction(usize, usize)
}

#[derive(Clone, Debug)]
pub struct Block {
    pub id: usize,
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub size: usize,
    // pub function: usize,
    pub previous_block: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub data: InstructionData,
    pub block: Option<usize>,
    pub next: Option<usize>,
    pub previous: Option<usize>,
    // pub function: usize,
    pub id: usize,
}

pub struct Module {
    pub variables: IndexVec<usize, IndexVec<usize, String>>,
    pub highest_variable: usize,
    pub functions: IndexVec<usize, Functions>,
    pub phi_functions: IndexVec<usize, Vec<(usize, usize, usize)>>
    // ^ Vec<function_id, block_id, instruction_id>

}

pub struct Functions {
    pub name: Option<String>,
    pub frame: BlockFrame,
    pub predecessor_block: Option<usize>,
    pub predecessor_function: Option<usize>,
    pub definition_id: usize,
    pub upindex_amount: usize,
    pub id: usize,
}

enum VariableVariant {
    Local(usize),
    Upvalue(usize, usize), // function id, variable id
    None,
}

#[derive(Clone)]
pub struct BlockFrame {
    pub instructions: IndexVec<usize, Instruction>,
    pub blocks: Vec<Block>,
    pub phi_instructions: Option<IndexVec<usize, Vec<(usize, usize)>>>,
    pub id: usize,
}

// functions
impl Module {
    pub fn new() -> Self {
        Self {
            variables: IndexVec::new(),
            highest_variable: 0,
            functions: IndexVec::with_capacity(1),
            phi_functions: IndexVec::with_capacity(1),
        }
    }
    fn compile(&self, syntax: Vec<Expressions>) {
        for expression in &syntax {

        }
    }

    pub fn build_expression(
        &mut self,
        expression: Expressions,
        block_frame: &mut BlockFrame,
        current_block: usize
    ) -> Option<usize> {
        match expression {
            Expressions::Int(num) => { Self::add_instruction(block_frame, current_block, InstructionData::LoadNumber(num)); },
            Expressions::Bool(val) => { Self::add_instruction(block_frame, current_block, InstructionData::LoadBool(val)); },
            Expressions::String(string) => { Self::add_instruction(block_frame, current_block, InstructionData::LoadString(string)); },
            Expressions::Variable(value) => {
                let mut new_value = VariableVariant::None;

                'outer: for (outer_index, var_function) in self.variables.iter().rev().enumerate() {
                    for (index, variable) in var_function.iter().enumerate().rev() {
                        if *variable == value {
                            if outer_index == 0 {
                                new_value = VariableVariant::Local(index);
                                break 'outer;
                            } else {
                                new_value = VariableVariant::Upvalue(self.variables.len() - 1 - outer_index, index);
                                break 'outer;
                            }
                            // new_value = Some(index);
                        }
                    }
                }
                match new_value {
                    VariableVariant::Local(val) => {
                        Self::add_instruction(block_frame, current_block, InstructionData::GetVariable(val));
                    },
                    VariableVariant::Upvalue(function, index) => {
                        Self::add_instruction(block_frame, current_block, InstructionData::Upvalue(function, index));
                    }
                    VariableVariant::None => panic!("Could not find variable: {:?}", value),
                }
                // panic!("Could not find variable: {:?}", value);
            }
            Expressions::LocalDefine(name, value) => {
                let Expressions::Variable(var_name) = *name else {
                    panic!()
                };

                if let Some(value) = value {
                    let _ = self.build_expression(*value, block_frame, current_block);
                    let context = &block_frame.blocks[current_block];
                    let obj = self.variables.len() - 1;
                    let vars = &mut self.variables[obj];
                    vars.push(var_name);
                    self.highest_variable += 1;

                    Self::add_instruction(
                        block_frame,
                        current_block,
                        InstructionData::SetVariable(
                            self.variables[obj].len() - 1,
                            context.end.unwrap()
                        ),
                    );
                }
            }
            Expressions::Assign(name, value) => {
                self.build_expression(*value, block_frame, current_block);

                let mut new_value = VariableVariant::None;

                'outer: for (outer_index, var_function) in self.variables.iter().rev().enumerate() {
                    for (index, variable) in var_function.iter().enumerate().rev() {
                        if *variable == name {
                            if outer_index == 0 {
                                new_value = VariableVariant::Local(index);
                                break 'outer;
                            } else {
                                new_value = VariableVariant::Upvalue(self.variables.len() - 1 - outer_index, index);
                                break 'outer;
                            }
                            // new_value = Some(index);
                        }
                    }
                }
                let context = &block_frame.blocks[current_block];
                match new_value {
                    VariableVariant::Local(id) => {
                        Self::add_instruction(
                            block_frame,
                            current_block,
                            InstructionData::SetVariable(
                                id,
                                context.end.unwrap()
                            )
                        );
                    }
                    VariableVariant::Upvalue(function, id) => {
                        Self::add_instruction(
                            block_frame,
                            current_block,
                            InstructionData::SetUpvalue(
                                function,
                                id,
                                context.end.unwrap()
                            )
                        );
                    }
                    VariableVariant::None => panic!("Could not assign variable '{:?}'. Variable not found", name),
                }
                // variables.push(name);
            }
            Expressions::MathOperand(variant, left, right) => {
                self.build_expression(*left, block_frame, current_block);
                self.build_expression(*right, block_frame, current_block);
                let context = &block_frame.blocks[current_block];

                Self::add_instruction(
                    block_frame,
                    current_block,
                    InstructionData::MathOperands(
                        variant,
                        block_frame.instructions[context.end.unwrap()].previous.unwrap(),
                        context.end.unwrap(),
                    )
                );
            }
            Expressions::Scope(new_scope) => {
                let mut new_block_id = if let Some(last) = block_frame.blocks.last() {
                    if last.end.is_none() {
                        block_frame.blocks.len() - 1
                    } else {
                        self.new_block(&mut block_frame.blocks, Some(current_block))
                    }
                } else {
                    self.new_block(&mut block_frame.blocks, Some(current_block))
                };
                // if block_frame.blocks.last().unwrap().end.is_none() {
                //     block_frame.blocks.len() - 1
                // } else {
                //     self.new_block(&mut block_frame.blocks, Some(current_block))
                // };
                println!("AND NEW BLOCK ID IS {:?}", new_block_id);
                // };
                // self.new_block(&mut block_frame.blocks, Some(current_block));

                let first_id = new_block_id;
                let old_len = self.variables.len();
                for expressions in new_scope {
                    // this needs to build the latest block
                    if let Some(new_id) = self.build_expression(expressions, block_frame, new_block_id) {
                        println!("inner ----- changing block id {:?} to {:?}", new_block_id, new_id);
                        // if block_frame.blocks[block_frame.blocks.len() - 1].size == 0 {
                        //     block_frame.blocks.pop();
                        // }
                        let last_new_block_instruction = block_frame.blocks[new_id - 1].end.unwrap();
                        match block_frame.instructions[last_new_block_instruction].data {
                            InstructionData::JumpConditional(_ ,_ ,_) | InstructionData::Jump(_) => {},
                            _ => {
                                Self::add_instruction(block_frame, new_id - 1, InstructionData::Jump(new_id));
                            }
                        }
                        new_block_id = new_id;
                    }
                    // might need to do this check once done building expressions for a block
                    // rather than after everything
                    // println!("+++++ BLOCK ID {:?} HAS SIZE OF {:?}", block_frame.blocks.len() - 1,block_frame.blocks[block_frame.blocks.len() - 1].size  );
                    println!("BLOCK FRAME SIZE OF {:?}", block_frame.blocks[block_frame.blocks.len() - 1].size);
                }
                // if block_frame.instructions[block_frame.blocks[new_block_id].end.unwrap()] {
                //
                // }
                if block_frame.blocks[new_block_id].size == 0 {
                    block_frame.blocks.swap_remove(new_block_id);

                    let start_id = block_frame.blocks[new_block_id - 1].end.unwrap();
                    // println!(")))))))))))))))))))))))) wel you see the previouis daata was: {:?}", block_frame.instructions[start_id].data );
                    match block_frame.instructions[start_id].data {
                        InstructionData::JumpConditional(condition, true_block, false_block) => {
                            // false_block = None;
                            // block_frame.instructions[start_id].data = InstructionData::JumpConditional(condition, true_block, None);
                        }
                        InstructionData::Jump(new_block) => {
                            Self::remove_instruction(block_frame, new_block - 1,  start_id);
                        }
                        _ => {}
                    }
                }

                // make it so that variables are scope local
                for var in old_len..self.variables.len() {
                    self.variables.pop();
                }

                return Some(first_id);
            }
            Expressions::If(conditional, scope) => {
                println!("CURRENT BLOCK BEFORE: {:?}", current_block);
                let first_scope = self.build_expression(*conditional, block_frame, current_block);
                let new_scope_id = block_frame.blocks.len();
                let scope = self.build_expression(*scope, block_frame, current_block);

                let last_scope_instruction = block_frame.blocks[scope.unwrap()].end.unwrap();
                match block_frame.instructions[last_scope_instruction].data {
                    InstructionData::JumpConditional(_ ,_ ,_) | InstructionData::Jump(_) => {},
                    _ => {
                        // NOTE: if i were to add an esle block to the ifs make sure to eheck if
                        // there will be an else block and then just jump past the else block
                        Self::add_instruction(block_frame, scope.unwrap(), InstructionData::Jump(scope.unwrap() + 1));
                    }
                }
                println!("|||||||||||||||| LATEST SCOPE: {:?}", scope);

                let mut new_block = self.new_block(&mut block_frame.blocks, Some(current_block));
                println!("CURRENT BLOCK: {:?}", current_block);
                println!("new b lock is {:?}", new_block);
                Self::add_instruction(
                    block_frame,
                    current_block,
                    InstructionData::JumpConditional(
                        block_frame.blocks[current_block].end.unwrap(),
                        new_scope_id,
                        // block_frame.blocks.len() - 1,
                        new_block
                        // block_frame.blocks.len()
                    )
                );

                return Some(new_block)
            }
            Expressions::ComparisonOperand(variant, left, right) => {
                self.build_expression(*left, block_frame, current_block);
                self.build_expression(*right, block_frame, current_block);

                Self::add_instruction(
                    block_frame,
                    current_block,
                    InstructionData::ComparisonOperands(
                        variant,
                        block_frame.instructions[block_frame.blocks[current_block].end.unwrap()].previous.unwrap(),
                        block_frame.blocks[current_block].end.unwrap(),
                    )
                );
            }
            Expressions::FunctionBlock(name, arguments, scope) => {

                // let old_function_id = self.functions.len() - 1;
                let mut new_variables = IndexVec::with_capacity(arguments.len());
                for argument in arguments.iter().rev() {
                    let Expressions::Variable(name) = argument else { panic!("argument is not a variable") };

                    new_variables.push(name.to_string());
                    self.highest_variable += 1;
                }
                self.variables.push(new_variables);

                let mut new_frame = BlockFrame::new(block_frame.id + 1, None);
                let latest_function = self.functions.len();
                let Expressions::Variable(name) = *name else { panic!() };
                self.functions.push(Functions {
                    name: Some(name.clone()), // TODO: anonymous functions
                    frame: new_frame.clone(),
                    predecessor_block: Some(current_block),
                    predecessor_function: Some(block_frame.id),
                    definition_id: block_frame.instructions.len(),
                    upindex_amount: 0,
                    id: block_frame.id + 1
                });
                // let new_frame = &mut self.functions[latest_function].frame;
                // panic!("new frame is {:?}, current id is {:?}", new_frame.id, block_frame.id);
                println!("_________________ ok so current id {:?}, nwo making new block __________________", block_frame.id);
                self.build_expression(*scope, &mut new_frame, 0);
                // println!("!!!!!!!!!!!!!!!!!!!!!!!!!!!! block is of id {:?}")
                // panic!("frame: {:?} instr {:?}", block_frame.id, block_frame.instructions);
                for (arg_index, argument) in arguments.iter().enumerate() {
                    Self::add_instruction(&mut new_frame, 0, InstructionData::Param(arg_index));
                }
                self.variables.pop();

                // panic!("old frame id {:?}", old_frame_id);


                // let start_id = block_frame.blocks[new_block_id - 1].end.unwrap();
                // match block_frame.instructions[start_id].data {
                //     InstructionData::JumpConditional(condition, true_block, false_block) => {
                //         // false_block = None;
                //         block_frame.instructions[start_id].data = InstructionData::JumpConditional(condition, true_block, None);
                //     }
                //     InstructionData::Jump(new_block) => {
                //         Self::remove_instruction(&mut block_frame.instructions, &mut block_frame.blocks[new_block - 1],  start_id);
                //     }
                //     _ => {}
                // }
                //
                // println!("!!!! id is {:?}, while the block frame id is {:?}", id, block_frame.id);
                // panic!("ok so in block frame old: {:?} new {:?}", old_frame_id, id);

                let len = self.variables.len() - 1;
                self.functions[latest_function].frame = new_frame;
                println!("VVVV -> self variables {:?}", self.variables);
                Self::add_instruction(block_frame, current_block, InstructionData::Closure(block_frame.id + 1, self.variables[len].len()));
                self.variables[len].push(name);
                self.highest_variable += 1;
                Self::add_instruction(block_frame, current_block, InstructionData::Jump(block_frame.blocks.len()));
                // panic!("frame: {:?}", block_frame.id);
                return Some(self.new_block(&mut block_frame.blocks, Some(current_block)));

            }
            Expressions::CallFunction(function_variable, variadic) => {
                if *function_variable == Expressions::Variable("print".to_string()) {
                    self.build_expression(variadic[0].clone(), block_frame, current_block);
                    // panic!("the value of variadic: {:?}", variadic);
                    println!("within print!!");
                    Self::add_instruction(block_frame,
                        current_block,
                        InstructionData::Print(block_frame.blocks[current_block].end.unwrap()),
                    );

                    return None;
                }

                let mut variadic_vec = Vec::with_capacity(variadic.len());
                for variable in variadic {
                    self.build_expression(variable, block_frame, current_block);
                    variadic_vec.push(block_frame.instructions.len() - 1);
                    // IMPORTANT: could just use a variadic start position and len as the count instead of
                    // iterating over this
                }
                self.build_expression(*function_variable, block_frame, current_block);

                Self::add_instruction(block_frame, current_block, InstructionData::CallFunction(block_frame.instructions.len() - 1, variadic_vec));
            }
            e => {
                panic!("instruction {:?}, not yet implemented", e);
            }
        }

        None
    }


    fn get_outer_function(functions: &IndexVec<usize, Functions>, function_id: usize, requested_id: usize) -> usize {
        let previous_function = &functions[function_id];
        if let Some(prev) = previous_function.predecessor_function {
            if prev == requested_id {
                return function_id;
            }
            return Self::get_outer_function(functions, prev, requested_id);
        }
        function_id
    }
    // fn get_function_offset(&self, function_id: usize, requested_id: usize, amount: &mut usize) -> usize {
    //     let previous_function = &self.functions[function_id];
    //     if let Some(prev) = previous_function.predecessor_function {
    //         if prev == requested_id {
    //             return function_id;
    //         }
    //         *amount += 1;
    //         return self.get_function_offset(prev, requested_id, amount);
    //     }
    //     function_id
    // }

    pub fn make_ssa(
        &mut self,
        frame: &mut BlockFrame,
        dominance_frontier: IndexVec<usize, IndexVec<usize, Option<(usize, usize)>>>,
    ) -> IndexVec<usize, Vec<(usize, usize)>> {
        let mut temporary_phi: Vec<(usize, Option<(usize, usize)>, usize, (usize, usize))> = Vec::new();
        // let mut new_names: IndexVec<usize, Option<(usize, usize)>> = index_vec![None; self.highest_variable];
        let mut new_names: IndexVec<usize, IndexVec<usize, Option<usize>>> = index_vec![ IndexVec::new(); self.functions.len()];
        // let mut phi_indexes = IndexVec::new();
        // might be allocating too much?
        // let mut phi_indices = index_vec![None; self.];

        let mut removed_instructions = Vec::new();
        let mut phi_map: IndexVec<usize, Vec<(usize, usize)>> = IndexVec::new();
        let mut inserted_phis = Vec::new();
        // let mut function_phis = Vec::new();

        // which functions have upindexes
        let mut has_upindex: IndexVec<usize, Vec<(usize, usize)>> = index_vec![ Vec::new(); self.functions.len() ];
        let mut previous_function = 0;
        // IMPORTANT: make functions swork alter right now the functions in conditionals dont
        // proprely get put out
        for block in &mut frame.blocks {
            let mut removed_anything = false;
            self.for_block_mut(0, block.id, &mut |functions, function_id, instruction_id| {
                // if function_id != 0 {
                //     panic!("not 0 id {:?}", function_id);
                // }
                let instr = &functions[function_id].frame.instructions[instruction_id];
                let block_id = instr.block.unwrap();
                if instr.previous.is_none() && !removed_anything {
                    for (id, frontier, _, replacement_id) in temporary_phi.iter() {
                        if let Some((frontier_block, frontier_function)) = frontier {
                            if function_id == *frontier_function && block_id == *frontier_block {
                                if let Some(name_id) = new_names[*frontier_function].get_mut(*id) {
                                    *name_id = Some(replacement_id.1);
                                    // name_id = Some((*frontier_function, replacement_id.1));
                                } else {
                                    new_names[*frontier_function].resize(*id, None);
                                    new_names[*frontier_function][*id] = Some(replacement_id.1);
                                }
                                inserted_phis.push((replacement_id.1, *id));
                            }
                        } else {
                            // new_names[*id] = Some(*replacement_id);
                            if let Some(name_id) = new_names[replacement_id.0].get_mut(*id) {
                                *name_id = Some(replacement_id.1);
                                // name_id = Some((*frontier_function, replacement_id.1));
                            } else {
                                new_names[replacement_id.0].resize(*id, None);
                                new_names[replacement_id.0][*id] = Some(replacement_id.1);
                            }
                            // inserted_phis.push((replacement_id.1, *id));
                            // println!(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; once {:?} {:?}", instruction_id, inserted_phis.len());
                            // panic!("once");
                            // panic!("adding replacement id {:?} to {:?}", replacement_id, id);
                            // inserted_phis.push((replacement_id.1, *id));
                        }
                    }

                    // dont know how else to do it other than 2 loops
                    for (id, frontier, map_id, replacement) in temporary_phi.iter() {
                        if let Some((frontier_block, frontier_function)) = frontier {
                            if let Some((current_block, current_function)) = dominance_frontier[0][block.id] {
                                if current_function == function_id && current_block == *frontier_block {
                                    for (instruction_id, var_id) in &inserted_phis {
                                        if var_id == id {
                                            phi_map[*map_id].push((0, *instruction_id));
                                        }
                                    }
                                }
                            } else {

                            }
                        } else if dominance_frontier[function_id][block.id].is_none() && replacement.0 == function_id {
                            for (instruction_id, var_id) in &inserted_phis {
                                if var_id == id {
                                    phi_map[*map_id].push((0, *instruction_id));
                                }

                            }
                        }
                    }
                    if !inserted_phis.is_empty()  {
                        inserted_phis.clear();
                    }
                }
                match functions[function_id].frame.instructions[instruction_id].data {
                    InstructionData::SetVariable(id, to_value) => {
                        let block_id = functions[function_id].frame.instructions[instruction_id].block.unwrap();
                        for (var_id, frontier, map_id, _) in temporary_phi.iter().rev() {
                            if *var_id == id {
                                // NOTE: might not even need this iflet checking
                                // since it wi.ll only be none in the tarrt and end blocks wherer
                                // convergence is impossible
                                if let Some((front_block, front_function)) = frontier {
                                    if *front_function == function_id {
                                        if dominance_frontier[function_id][block_id].unwrap().0 == *front_block {
                                            phi_map[*map_id].push((function_id, to_value));
                                            Self::remove_instruction(&mut functions[function_id].frame, block_id, instruction_id);
                                            removed_anything = true;
                                            if let Some(name_id) = new_names[function_id].get_mut(id) {
                                                *name_id = Some(to_value);
                                                // name_id = Some((*frontier_function, replacement_id.1));
                                            } else {
                                                new_names[function_id].resize(id, None);
                                                new_names[function_id][id] = Some(to_value);
                                            }
                                            // old
                                            // new_names[id] = Some((function_id, to_value));
                                            return;
                                        } else {
                                            panic!(" for phi {:?} frontier was none.. ", var_id)
                                        }
                                    } else {
                                        // NOTE: think later if functions are correct
                                        panic!("yueah");
                                        // let function_definition = Self::get_outer_function(functions, *front_function, function_id);
                                        // phi_map[*map_id]
                                    }
                                }
                            }
                        }

                        if let Some(name_value) = new_names[function_id].get(id) {
                            if let Some(name_value) = name_value {
                                temporary_phi.push((id, dominance_frontier[function_id][block_id], phi_map.len(), (function_id, instruction_id)));
                                functions[function_id].frame.instructions[instruction_id].data = InstructionData::Phi(phi_map.len());
                                phi_map.push(vec![(function_id, *name_value), (function_id, to_value)]);
                            }
                        } else {
                            Self::remove_instruction(&mut functions[function_id].frame, block_id, instruction_id);
                            removed_anything = true;
                            new_names[function_id].resize(id + 1, None);
                            // panic!("this doesnt owrk id {:?} function {:?}", instruction_id, function_id)
                        }
                        new_names[function_id][id] = Some(to_value);
                        // if let Some((original_function, original)) = new_names[id] {
                        //     temporary_phi.push((id, dominance_frontier[function_id][block_id], phi_map.len(), (function_id, instruction_id)));
                        //     functions[function_id].frame.instructions[instruction_id].data = InstructionData::Phi(phi_map.len());
                        //     phi_map.push(vec![(original_function, original), (function_id, to_value)]);
                        // } else {
                        //     Self::remove_instruction(&mut functions[function_id].frame.instructions, block, instruction_id);
                        //     removed_anything = true;
                        // }
                        // new_names[id] = Some((function_id, to_value));
                    }
                    InstructionData::SetUpvalue(func_id, id, to_value) => {
                        // removed_anything = true;
                        // // new_names[function_id][id] = Some(functions[func_id].frame.instructions.len() - 1);
                        // if let Some(name_id) = new_names[function_id].get_mut(id) {
                        //     *name_id = Some(functions[function_id].frame.instructions.len());
                        //     // name_id = Some((*frontier_function, replacement_id.1));
                        // } else {
                        //     new_names[function_id].resize(id + 1, None);
                        //     new_names[function_id][id] = Some(functions[function_id].frame.instructions.len());
                        // }
                        // panic!("new names is {:?} with id of {:?} in function {:?}", new_names[function_id][id], id, function_id);
                        // panic!("ok so len: {:?}", functions[func_id].frame.instructions.len() - 1);
                        let mut first_upindex = None;
                        println!("hello there");
                        Self::function_tree(functions, function_id, func_id, &mut |current_id, functions, previous_function| {
                            println!("------- function {:?}", current_id);
                            for var_id in has_upindex[*current_id].iter().rev() {
                                if var_id.0 == id {
                                    if first_upindex.is_none() {
                                        first_upindex = Some(var_id.1)
                                    }
                                    return;
                                }
                            }
                            println!("+++++ other ok so previosu function is {:?} current is {:?}", previous_function, current_id);
                            let upindex = if previous_function == Some(func_id) {
                                Self::new_instruction(&mut functions[*current_id].frame.instructions, InstructionData::Upindex(func_id, new_names[func_id][id].unwrap()))
                            } else {
                                let new_instruction = functions[previous_function.unwrap()].frame.instructions.len();
                                Self::new_instruction(&mut functions[*current_id].frame.instructions, InstructionData::Upindex(previous_function.unwrap(), new_instruction))
                                // panic!("ok so new instruction is {:?}", new);
                                // panic!("ok so current id {:?}, current fucntion id {:?}, requested: {:?}", new, function_id, current_id);
                            };
                            let new_frame = &mut functions[*current_id].frame;
                            let first_instr = &new_frame.blocks[0].start;
                            Self::insert_instruction(new_frame, 0, new_frame.instructions.len() - 1, None, *first_instr);
                            has_upindex[*current_id].push((id, upindex));
                            if first_upindex.is_none() {
                                first_upindex = Some(upindex);
                            }
                        });
                        println!("new names is {:?} funci d is {:?}", new_names, function_id);
                        functions[function_id].frame.instructions[instruction_id].data = InstructionData::SetUpindex(to_value, first_upindex.unwrap());
                        // instr.data =
                        // // println!(".............. ok so {:?} == {:?}", block_id, functions[instruction_id].frame.instructions[instruction_id].block);
                        // // Self::remove_instruction(&mut functions[function_id].frame.instructions, block, instruction_id);
                        // // functions[function_id].frame.blocks[block.id] = block.clone();
                        //
                        // let InstructionData::Upindex(original_function, original_id) = functions[function_id].frame.instructions[new_names[function_id][id].unwrap()].data else {
                        //     panic!("phi set upvalue doesnt refer to an upindex??")
                        // };
                        //
                        // // panic!("UPINDEX REFERS TO FUNCTION {:?} ID {:?}", original_function, original_id);
                        //
                        //
                        // // panic!("dominance fronteir is {:?}",dominance_frontier[function_id][block_id]);
                        // // if dominance_frontier[function_id][block_id].is_none() {
                        //     // println!("temporaries: {:?}", temporary_phi);
                        // let mut has_found = false;
                        // // for (var_id, frontier, map_id, _) in temporary_phi.iter().rev() {
                        // //     for (object_function, object_id) in &phi_map[*map_id] {
                        // //         if *object_id == original_id {
                        // //             has_found = true;
                        // //             break;
                        // //         }
                        // //     }
                        // //     // panic!("has found is {:?}", has_found);
                        // //     if has_found {
                        // //         phi_map[*map_id].push((function_id, to_value));
                        // //         removed_anything = true;
                        // //         break;
                        // //     // } else {
                        // //         // phi_map[*map_id].push((function_id, to_value));
                        // //     }
                        // //     // BUG: i have no clue if this shoiuld or shouldnt break
                        // // }
                        //     // panic!("has found is {:?}", has_found);
                        //
                        // if !has_found {
                        //     let new_instr = Self::new_instruction(&mut functions[original_function].frame.instructions, InstructionData::Phi(phi_map.len()));
                        //     temporary_phi.push((id, dominance_frontier[function_id][block_id], phi_map.len(), (original_function, new_instr)));
                        //     // functions[function_id].frame.instructions[instruction_id].data = InstructionData::Phi(phi_map.len());
                        //     phi_map.push(vec![(original_function, original_id), (function_id, to_value)]);
                        // }
                        // Self::remove_instruction(&mut functions[function_id].frame.instructions, block, instruction_id);
                        // // }
                        //
                        // // IMPORTANT: DO NOT FORTGET THAT `NONE` DOMINANCE FRONTIERS MEAN TAHT
                        // // THEY PASS THROUGH THE WHOLE PROGRAM
                        // new_names[function_id][id] = Some(to_value);

                        // new_names[id] = Some((function_id, to_value));
                    }
                    InstructionData::GetVariable(id) => {
                        println!("MMMMM current function id {:?}, adding {:?} and {:?} to removed", function_id, instruction_id, new_names[id]);
                        println!("instr data: next: {:?} prev: {:?}, block start: {:?} block end {:?}", instr.next, instr.previous, block.start, block.end);
                        removed_instructions.push((function_id, instruction_id, new_names[function_id][id]));
                        println!("its here with block id {:?}", block_id);
                        let frame = &mut functions[function_id].frame;
                        Self::remove_instruction(frame, block_id, instruction_id);
                        // functions[function_id].frame.blocks[block.id] = block.clone();
                        removed_anything = true;
                        println!("after -> instr data: next: {:?} prev: {:?}, block start: {:?} block end {:?}",
                            functions[function_id].frame.instructions[instruction_id].next,
                            functions[function_id].frame.instructions[instruction_id].previous,
                            block.start,
                            block.end
                        );
                        // NOTE: might not need this
                    }
                    InstructionData::Closure(new_function, id) => {
                        if let Some(new_name) = new_names[function_id].get_mut(id) {
                            *new_name = Some(instruction_id)
                        } else {
                            new_names[function_id].resize(id + 1, None);
                            new_names[function_id][id] = Some(instruction_id);
                        }
                        // new_names[id] = Some((function_id, instruction_id));
                    }
                    InstructionData::Upvalue(func_id, id) => {
                        let mut first_upindex = None;
                            // let current_instr
                        // if let Some(next) = instr.next {
                        //     // functions[function_id].frame.instructions[].next = functions[function_id].frame.instructions[instruction_id].next;
                        //     if let Some(prev) = functions[function_id].frame.instructions[next].previous {
                        //         functions[function_id].frame.instructions[next].previous = first_upindex;
                        //     }
                        //
                        // }
                        Self::remove_instruction(&mut functions[function_id].frame, block_id, instruction_id);
                        // panic!("OK SO DATA NOW {:?}", functions[function_id].frame.instructions[instruction_id]);
                        // panic!("OK SO BLOCK START NOW {:?}, current id {:?}", functions[function_id].frame.blocks[block_id].start, instruction_id);
                        Self::function_tree(functions, function_id, func_id, &mut |current_id, functions, previous_function| {
                            for var_id in has_upindex[*current_id].iter().rev() {
                                if var_id.0 == id {
                                    if first_upindex.is_none() {
                                        first_upindex = Some(var_id.1);
                                    }
                                    return;
                                }
                            }
                            println!("+++++ ok so previosu function is {:?} current is {:?}", previous_function, current_id);
                            let upindex = if previous_function == Some(func_id) {
                                Self::new_instruction(&mut functions[*current_id].frame.instructions, InstructionData::Upindex(func_id, new_names[func_id][id].unwrap()))
                            } else {
                                let new_instruction = functions[previous_function.unwrap()].frame.instructions.len();
                                Self::new_instruction(&mut functions[*current_id].frame.instructions, InstructionData::Upindex(previous_function.unwrap(), new_instruction))
                                // panic!("ok so new instruction is {:?}", new);
                                // panic!("ok so current id {:?}, current fucntion id {:?}, requested: {:?}", new, function_id, current_id);
                            };
                            let new_frame = &mut functions[*current_id].frame;
                            let first_instr = &new_frame.blocks[0].start;
                            Self::insert_instruction(new_frame, 0, new_frame.instructions.len() - 1, None, *first_instr);
                            has_upindex[*current_id].push((id, upindex));
                            if first_upindex.is_none() {
                                first_upindex = Some(upindex);
                            }
                        });

                        // functions[function_id].frame.instructions[instruction_id].data = InstructionData::Upindex(func_id, ())
                        // functions[function_id].frame.blocks[block.id] = block.clone();
                        // panic!("OK SO UPVALUE DATA: {:?} NEXT: {:?} IN BLOCK {:?}", instr.previous, instr.next)
                        // println!("before upvalue");
                        // new_names[function_id][id] = first_upindex;
                        // if let Some(new_name) = new_names[function_id].get_mut(id) {
                        //     *new_name = first_upindex
                        // } else {
                        //     new_names[function_id].resize(id + 1, None);
                        //     new_names[function_id][id] = first_upindex;
                        // }
                    }
                    _ => {}
                }
            });
        }
        while let Some((_, frontier, _, replacement_id)) = temporary_phi.pop() {
            if let Some((frontier_block, frontier_function)) = frontier {
                let frame = &mut self.functions[frontier_function].frame;
                // THE THING IS THE START!!!
                // BUG: also check if the block id matches not just instruction id
                if let Some(start) = frame.blocks[frontier_block].start {
                    if replacement_id.1 != start {
                        Self::insert_instruction(frame, frontier_block, replacement_id.1, None, frame.blocks[frontier_block].start);
                    } else {
                        panic!("oh wow 2");
                    }
                } else {
                    // panic!("not ineserting");
                }
            } else {
                let frame = &mut self.functions[replacement_id.0].frame;
                if let Some(start) = frame.blocks[frame.blocks.len() - 1].start {
                    if replacement_id.1 != start {
                        // Self::insert_instruction(frame, frontier_block, replacement_id.1, None, frame.blocks[frontier_block].start);
                        Self::insert_instruction(frame, frame.blocks.len() - 1, replacement_id.1, None, frame.blocks[frame.blocks.len() - 1].start);
                    } else {
                        // panic!("oh wow");
                    }
                } else {
                    // panic!("not ineserting 2");
                }
                // Self::insert_instruction(frame, frame.blocks.len() - 1, replacement_id.1, None, frame.blocks[frame.blocks.len() - 1].start);
            }
        }

        // make it so that this here will insert a phi function arguments maybe?
        self.replace_all_args(|instruction, function, instructions, original_instruction| {
            for (removed_function, removed, new_destination) in &removed_instructions {
                if removed_function == function && instruction == removed{
                    println!("removed: {:?}, new_destination {:?}", removed, new_destination);
                    *instruction = new_destination.unwrap();
                    break;
                }
            }

            // let current_instruction = &self.functions[*function].frame.instructions[*instruction];
            for (_, frontier, map, replacement_id) in temporary_phi.iter() {
                if let Some((frontier_block, frontier_function)) = frontier {
                    if frontier_function == function && instructions[original_instruction].block.unwrap() >= *frontier_block {
                        for (_, phi_instruction) in &phi_map[*map] {
                            if phi_instruction == instruction {
                                *instruction = replacement_id.1;
                                return;
                            }
                        }
                    }
                } else {
                    panic!("forgot")
                    // insert it at the end?
                }
            }
        });

        phi_map
    }


    pub fn get_liveness(
        &self,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        post_order: &Vec<(usize, usize)>,
        order_indices: &IndexVec<usize, IndexVec<usize, usize>>
    ) -> IndexVec<usize, Option<usize>>{
        let mut live: IndexVec<usize, Option<usize>> = index_vec![None; post_order.len()];
        let mut function_ends: IndexVec<usize, usize> = index_vec![0; self.functions.len()];
        for (index, (function_id, instruction_id)) in post_order.iter().enumerate().rev() {
            let function = &self.functions[*function_id];
            let instruction = &function.frame.instructions[*instruction_id];

            if function_ends[*function_id] == 0 {
                function_ends[*function_id] = index;
            }

            if instruction.has_value() {
                match &instruction.data {
                    InstructionData::Phi(map_id) => {
                        let phi_map = &phi_instructions[*map_id];
                        for (new_function, arg) in phi_map {
                            let indice = order_indices[*new_function][*arg];
                            live[indice] = live[index];
                        }
                    }
                    InstructionData::Upindex(func_id, ref_id) => {
                        live[index] = Some(function_ends[*function_id]);
                    }
                    _ => {

                        Self::instruction_args(*function_id, &instruction.data, &mut |arg, arg_function| {
                            let indice = order_indices[*arg_function][*arg];
                            if live[indice].is_none() {
                                live[indice] = Some(index);
                            }
                        });
                    }
                }
            }
        }
        for (index, value) in live.iter().enumerate() {
            println!("L: hey i have liveness {:?} : {:?}", index, value);
        }

        live
    }

    pub fn allocate_registers(
        &self,
        phi_map: &IndexVec<usize, Vec<(usize, usize)>>,
        post_order: &Vec<(usize, usize)>,
        order_indices: &IndexVec<usize, IndexVec<usize, usize>>,
        liveness: IndexVec<usize, Option<usize>>,
    ) -> IndexVec<usize, Option<usize>> {
        // register, function
        let mut post_order_registers: IndexVec<usize, Option<usize>> = index_vec![None; post_order.len()];
        let mut all_registers: IndexVec<usize, IndexVec<usize, usize>> = index_vec![IndexVec::new(); self.functions.len()];

        let mut previous_function = 0;
        for (order_index, liveness_end) in liveness.iter().enumerate() {
            let (order_function, order_id) = post_order[order_index];
            let frame = &self.functions[order_function].frame;
            let instruction = &frame.instructions[order_id];


            match instruction.data {
                InstructionData::SetUpindex(to_value, upindex) => {
                    post_order_registers[order_indices[order_function][to_value]] = post_order_registers[order_indices[order_function][upindex]];
                    println!("register upindex set to {:?}", post_order);
                    continue;
                }
                InstructionData::Phi(map_id) => {
                    let mut first_instruction = None;
                    let mut first_register = None;
                    self.for_phi(phi_map, &map_id, &mut |arg, function_id| {
                        if first_instruction.is_none() {
                            println!("function id is {:?}", function_id);
                            first_instruction = Some((arg, function_id));

                            first_register = post_order_registers[order_indices[function_id][arg]];
                        } else {
                            post_order_registers[order_indices[function_id][arg]] = first_register;
                        }
                    });
                    if let Some((first_instruction, first_function)) = first_instruction {
                        println!("all registers is {:?} and first function {:?} | first instr {:?}", all_registers, first_function, first_instruction);
                        let order_register = order_indices[first_function][first_instruction];
                        // if let Some(register) = all_registers[first_function].get_mut(order_register) {
                        //     *register = liveness_end.unwrap();
                        // } else {
                        //     // all_registers[first_function].resize(order_register + 1, 0);
                        //     // all_registers[first_function][order_register] = liveness_end.unwrap();
                        // }
                        // all_registers[first_function][order_register] = liveness_end.unwrap();
                    }
                    post_order_registers[order_index] = first_register;
                    continue

                }
                _ => {}
            }
            if post_order_registers[order_index].is_some() {
                panic!("YES");
            }
            if liveness_end.is_none() {
                // panic!("ok");
                println!("skipping {:?}", instruction.data);
                continue;
            }

            println!("instruction is {:?}", instruction.data);
            // for function in all_registers.iter_mut() {
            // NOTE: i might have to make is ot htat it would also add a register end to all outer
            // functions too
            // panic!("what");
            let mut new_index = None;
            for (index, register_end) in all_registers[order_function].iter_mut().enumerate() {
                println!("ok so comparing {:?} < {:?}", register_end, order_index);
                if *register_end < order_index  {
                    *register_end = liveness_end.unwrap();
                    new_index = Some(index);
                    println!("---- index should be {:?}", index);
                    break;
                }
            }
            if let Some(new_index) = new_index {
                println!("psot order registers is {:?}", post_order_registers);
                post_order_registers[order_index] = Some(new_index);
            } else {
                let length = all_registers[order_function].len();
                all_registers[order_function].resize(length + 1, 0);
                all_registers[order_function][length] = liveness_end.unwrap();
                println!("!! length is {:?} and im setting ordeer index {:?}", length, order_index);
                post_order_registers[order_index] = Some(length);
            }
        }
        println!("aftert all regidster {:?}", all_registers);

        post_order_registers
    }
    pub fn compile_bytecode(
        &self,
        post_order: &Vec<(usize, usize)>,
        order_indices: &IndexVec<usize, IndexVec<usize, usize>>,
        // INFO: so as a solution to the prob lem of hwo to make it so that the virtual machine
        // would understand which register is relative and which is non relative
        //
        //make order_registers instead of an option usize it would be a custom enum type that can
        //be casted to a u8 type
        //which then to detect ifi ts relative
        //relative - if function id matches
        // absolute - if function id doesnt match
        order_registers: &IndexVec<usize, Option<usize>>
    ) -> IndexVec<usize, Vec<u8>> {
        let mut all_output = index_vec![Vec::new(); self.functions.len()];

        let mut replace_to_pc = Vec::new();
        let mut inserted_ids = 0;
        for (index, (order_function, order_id)) in post_order.iter().enumerate() {
            let frame = &self.functions[*order_function].frame;
            let instruction = &frame.instructions[*order_id];
            let output = &mut all_output[*order_function];

            for (instr_id, function_id, location) in &replace_to_pc {
                if instr_id == order_id && *function_id == order_function {
                    println!("instr data: {:?} replacing with {:?}", instruction.data, output.len());
                    output[*location] = output.len() as u8;
                    if output[*location - 1] == Opcode::Jump as u8 {
                        output.pop();
                        output.pop();
                        // panic!("it leads to a jump that would go to {:?} || order id {:?}", instruction.id, index);
                    }
                }
            }
            match &instruction.data {
                InstructionData::LoadNumber(num) => {
                    if let Some(register_id) = order_registers[index] {
                        output.push(Opcode::Load as u8);
                        output.push(*num as u8);
                        output.push(register_id as u8);
                    }
                }
                InstructionData::MathOperands(variant, num_1, num_2) => {
                    if let Some(register_id) = order_registers[index] {
                        match variant {
                            MathVariant::Add => output.push(Opcode::Add as u8),
                            MathVariant::Subtract => output.push(Opcode::Subtract as u8),
                            MathVariant::Multiply => output.push(Opcode::Multiply as u8),
                            MathVariant::Divide => output.push(Opcode::Divide as u8),
                            MathVariant::Remainder => output.push(Opcode::Remainder as u8),
                        }
                        output.push(order_registers[order_indices[*order_function][*num_1]].unwrap() as u8);
                        output.push(order_registers[order_indices[*order_function][*num_2]].unwrap() as u8);
                        output.push(register_id as u8);
                    }
                }
                InstructionData::ComparisonOperands(variant, num_1, num_2) => {
                    if let Some(register_id) = order_registers[index] {
                        match variant {
                            ComparisonVariant::Equal => output.push(Opcode::Equal as u8),
                            ComparisonVariant::NotEqual => output.push(Opcode::NotEqual as u8),
                            ComparisonVariant::LessEqual => output.push(Opcode::LessThanOrEqual as u8),
                            ComparisonVariant::MoreEqual => output.push(Opcode::GreaterThanOrEqual as u8),
                            ComparisonVariant::LessThan => output.push(Opcode::LessThan as  u8),
                            ComparisonVariant::MoreThan => output.push(Opcode::GreaterThan  as u8),
                        }
                        output.push(order_registers[order_indices[*order_function][*num_1]].unwrap() as u8);
                        output.push(order_registers[order_indices[*order_function][*num_2]].unwrap() as u8);
                        output.push(register_id as u8);
                    }
                }
                InstructionData::Jump(block) => {
                    // let mut start = bl
                    if let Some(start) = frame.blocks[*block].start {
                        let mut start = start;
                        loop {
                            let instruction = &frame.instructions[start];
                            match instruction.data {
                                InstructionData::Phi(_) => {
                                    start= instruction.next.unwrap();
                                },
                                _ => break,
                            }
                        }
                        output.push(Opcode::Jump as u8);
                        replace_to_pc.push((start, order_function, output.len()));
                        output.push(98);
                        println!("|||||||||||||||| ADDED A JUMP INSTRUCTION {:?} IN FUCNTION {:?}, order id {:?}", instruction.id, order_function, index);
                    }
                }
                InstructionData::JumpConditional(comparison, target_true, target_false) => {
                    let mut true_start = frame.blocks[*target_true].start.unwrap();
                    loop {
                        let instruction = &frame.instructions[true_start];
                        match instruction.data {
                            InstructionData::Phi(_) => {
                                true_start = instruction.next.unwrap();
                            },
                            _ => break,
                        }
                    }
                    if let Some(false_target) = frame.blocks.get(*target_false) {
                        if let Some(false_start) = frame.blocks[*target_false].start {
                            let mut false_start = false_start;
                            loop {
                                let instruction = &frame.instructions[false_start];
                                match instruction.data {
                                    InstructionData::Phi(_) => {
                                        false_start = instruction.next.unwrap();
                                    },
                                    _ => break,
                                }
                            }

                            output.push(Opcode::JumpConditional as u8);
                            replace_to_pc.push((true_start, order_function, output.len()));
                            replace_to_pc.push((false_start, order_function,  output.len() + 1));
                            output.push(99);
                            output.push(98);
                            continue;
                        }
                    }
                    output.push(Opcode::JumpIfEqual as u8);
                    replace_to_pc.push((true_start, order_function, output.len()));
                    output.push(99);
                }
                InstructionData::Print(target) => {
                    // if let Some(register) = order_registers[index] {
                    output.push(Opcode::Print as u8);
                    output.push(order_registers[order_indices[*order_function][*target]].unwrap() as u8);
                    // }
                }
                InstructionData::Closure(function_id, variable_id) => {
                    if let Some(register) = order_registers[index] {
                        output.push(Opcode::Closure as u8);
                        output.push(*function_id as u8);
                        output.push(register as u8);
                    }
                }
                InstructionData::CallFunction(target, arguments) => {
                    output.push(Opcode::CallFunction as u8);
                    output.push(order_registers[order_indices[*order_function][*target]].unwrap() as u8);
                    // NOTE: might want to push argument count as well
                    for argument in arguments {
                        output.push(order_registers[order_indices[*order_function][*target]].unwrap() as u8);
                    }
                }
                InstructionData::Upindex(target_func, target_id) => {
                    if let Some(register) = order_registers[index] {
                        if let Some(target_register) = order_registers[order_indices[*target_func][*target_id]] {
                            output.push(Opcode::Upindex as u8);
                            output.push(target_register as u8);
                            output.push(register as u8);
                        }
                    }

                }
                e => {
                    println!("Unimplemented {:?} with id of {:?}", e, order_id);
                    continue;
                }
            }

        }
        println!("compiled bytecode: total len {:?}", all_output.len());
        let mut skip_amount = 0;
        let mut index = 0;
        for (function_id, output) in all_output.iter().enumerate() {
            println!("================= function id {:?}", function_id);
            for (pc, text) in output.iter().enumerate() {
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
                    Opcode::Closure => skip_amount = 2,
                    Opcode::CallFunction => skip_amount = 1, // this one is tricky
                    Opcode::Upindex => skip_amount = 2,
                    _ => {}
                }
                index += 1;
                println!("{:?}, program counter: {:?} ||    {:?}", index, pc, Opcode::from(*text));
                println!("(");
            }

        }

        all_output
    }

}


// ultility functions
impl Module {
    pub fn for_phi<F: FnMut(usize, usize)>(
        &self,
        phi_instructions: &IndexVec<usize, Vec<(usize, usize)>>,
        map_id: &usize,
        callback: &mut F
    ) {
        for (function_id, phi_arg) in &phi_instructions[*map_id] {
            let frame = &self.functions[*function_id].frame;
            if let InstructionData::Phi(new_map) = frame.instructions[*phi_arg].data {
                if new_map == *map_id {
                    panic!("same??");
                }
                // println!("|| met a phi! {:?} with map {:?}", phi_arg, map_id);
                self.for_phi(phi_instructions, &new_map, callback);
                callback(*phi_arg, *function_id);
            } else {
                callback(*phi_arg, *function_id)
            }
        }
    }
    pub fn get_successors<F: FnMut(
        &usize,
        &usize,
        &IndexVec<usize, Instruction>,
    )>(
        &self,
        instructions: &IndexVec<usize, Instruction>,
        block: &Block,
        function_id: &usize,
        callback: &mut F
    ) {
        println!("block info: {:?}", block);
        if block.end.is_none() {
            println!("no thing found");
            return
        }
        let last_instruction = &instructions[block.end.unwrap()];
        match last_instruction.data {
            InstructionData::Jump(target) => {
                if let Some(prev) = last_instruction.previous {
                    if let InstructionData::Closure(func, var_id) = instructions[prev].data {
                        let function = &self.functions[func];
                        self.get_successors(&function.frame.instructions, &function.frame.blocks[0], &func, callback);
                    }
                }
                callback(function_id, &target, instructions)
            },
            InstructionData::JumpConditional(_, target_1, target_2) => {
                callback(function_id, &target_1, instructions);
                callback(function_id, &target_2, instructions);
            }
            InstructionData::EndOfFile => {}
            // _ => unimplemented!("tried calling successors on an unterminated block"),
            _ => println!("unterminated block right here {:?}", block.id),
        }
    }

    // pub fn get_predecessors(
    //     &self,
    // ) -> IndexVec<usize, IndexVec<usize, IndexVec<usize, usize>>> {
    //     let blocks = &self.functions[0].frame.blocks;
    //     let instructions = &self.functions[0].frame.instructions;
    //     let mut predecessors = index_vec![index_vec!(IndexVec::new(); blocks.len()); self.functions.len()];
    //
    //     for block in blocks {
    //         self.get_successors(instructions, block, 0, |function, successor, _| predecessors[function][successor].push((function, block.id)));
    //         // self.get_successors(instructions, block, |successor, function, _| predecessors[successor].push(block.id));
    //     }
    //
    //     predecessors
    // }

    pub fn post_order(
        &self,
        frame: &BlockFrame,
        function_id: &usize,
        block_id: usize,
        current_post_order: &mut Vec<(usize, usize)>
    ) {
        // println!("function: {:?}, block: {:?}", function_id, block_id);
        if frame.blocks.get(block_id).is_none()  {
            return;
        }
        let block = &frame.blocks[block_id];
        let mut at = block.start;
        while let Some(id) = at {
            let instruction = &frame.instructions[id];
            at = instruction.next;
            println!("-- CURRENT INSTRUCTION: {:?} NEXT: {:?}", instruction.data, at);
            match instruction.data {
                InstructionData::Closure(func, _) => {
                    let function = &self.functions[func];
                    current_post_order.push((*function_id, instruction.id));
                    // println!("jumping to {:?}: {:?}", func, instruction.id);
                    self.post_order(&function.frame, &func, 0, current_post_order);
                }
                InstructionData::Jump(target) => {
                    current_post_order.push((*function_id, instruction.id));
                    self.post_order(frame, function_id, target, current_post_order);
                },
                InstructionData::JumpConditional(_, target_1, target_2) => {
                    current_post_order.push((*function_id, instruction.id));
                    self.post_order(frame, function_id, target_1, current_post_order);
                    self.post_order(frame, function_id, target_2, current_post_order);
                }
                _ => {
                    current_post_order.push((*function_id, instruction.id));
                }
            }
            // println!("      the {:?} instruction is {:?} in block {:?}", instruction.id, instruction.data, instruction.block);
        }
    }


    // gets the convergence point of a block
    pub fn dominance_frontier(
        &self,
        frame: &BlockFrame,
        origin_function: usize,
        convergences: &mut IndexVec<usize, IndexVec<usize, Option<(usize, usize)>, >>
    ) {
        for block in &frame.blocks {
            self.get_successors(&frame.instructions, block, &origin_function, &mut |function_id, target, instructions| {
                if *function_id == origin_function {
                    match instructions[block.end.unwrap()].data {
                        InstructionData::JumpConditional(conditional, target_1, target_2) => {
                            // TODO: if i add an  else, this needs to check if there is an else
                            // if yes - set the convergence of this to the else target
                            // if no - set the convergence to target_2
                            // if let Some(target) = target_2 {
                            convergences[*function_id][target_1] = Some((target_2, *function_id));
                            // }
                        }
                        _ => {
                            // let previous_block = frame.blocks[frame.blocks]
                            let function_frame = &self.functions[*function_id].frame;
                            println!("ok so the current function id {:?}, target: {:?}", function_id, target);
                            let previous_block = function_frame.blocks[*target].previous_block;
                            if let Some(previous) = previous_block {
                                convergences[*function_id][*target] = convergences[*function_id][previous];
                            }
                        }
                    }
                } else {
                    // panic!("wow");
                    // this condition never happens because previous function is function frame local!
                    // if let Some(prev) = previous_function {
                    //     if prev == origin_function {
                    //         // set this functions 0th block convergence point to the
                    //         // predecessors functions next block
                    //         convergences[function_id][0] =
                    //         return;
                    //     }
                    // }
                    self.dominance_frontier(&self.functions[*function_id].frame, *function_id,  convergences);
                    // NOTE: in order ofr this to wor ki need to make ti so that functins will make
                    // a new block beacuse it wont converge proprely atm after the function
                    // (or technically i ovuld also amke it so that there is a precise instruction
                    // location also included in the dominance thing so its not only just with the
                    // block when it is related to a function)
                    convergences[*function_id][0] = Some((block.id, *function_id));
                }
            });
        }
    }

    // fn build_dominance(
    //     &self,
    //     frame: &mut BlockFrame,
    //     post_order: &mut Vec<(usize, usize)>,
    // ) {
    //     let mut changed = true;
    //
    //     let post_indices = {
    //         let mut indices: IndexVec<usize, IndexVec<usize, Option<usize>>> = index_vec![index_vec![None; frame.blocks.len()]; self.functions.len()];
    //         for (index, block) in post_order.iter().enumerate() {
    //             indices[block.1] = Some(index); // NOTE: make it so that it puts it ot the proper
    //             // function
    //         }
    //
    //         indices
    //     };
    //
    //     let mut domination = index_vec![index_vec![None; frame.blocks.len()]; self.functions.len()];
    //     for func in self.functions {
    //         domination[func.id][0] = Some(0);
    //     }
    // }
    pub fn new_instruction(
        instructions: &mut IndexVec<usize, Instruction>,
        instruction: InstructionData,
        // function: usize
    ) -> usize {
        let id = instructions.len();
        println!("instr len: {:?}", id);

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

    pub fn insert_instruction(
        frame: &mut BlockFrame,
        block_id: usize,
        instruction_id: usize,
        new_previous: Option<usize>,
        new_next: Option<usize>,
    ) {
        let instruction = frame.instructions[instruction_id].clone();
        if let Some(prev) = instruction.previous {
            frame.instructions[prev].next = instruction.next;
        }
        if let Some(next) = instruction.next {
            frame.instructions[next].previous = instruction.previous; // might not need this?
        }
        if let Some(instruction_block) = instruction.block {
            frame.blocks[instruction_block].size -= 1;
        }
        // frame.blocks[instruction.block.unwrap()].size -= 1;
        // frame.instructions

        let block = &mut frame.blocks[block_id];
        let instruction = &mut frame.instructions[instruction_id];

        instruction.block = Some(block_id);
        println!("previosu prev: {:?} new: {:?}", instruction.previous, new_previous);
        instruction.previous = new_previous;
        println!("previosu next: {:?} new: {:?}", instruction.next, new_next);
        instruction.next = new_next;
        block.size += 1;

        if new_previous.is_none() {
            block.start = Some(instruction_id);
        }
        if new_next.is_none() {
            block.end = Some(instruction_id);
        }

    }
    // pub fn insert_instruction(
    //     frame: &mut BlockFrame,
    //     // instructions: &mut IndexVec<usize, Instruction>,
    //     // blocks: &mut Vec<Block>,
    //     block_id: usize,
    //     reference: Option<usize>,
    //     instruction_id: usize,
    // ) {
    //     let block = &mut frame.blocks[block_id];
    //
    //     let (previous, next) = {
    //         if let Some(reference_id) = reference {
    //             (Some(reference_id), frame.instructions[reference_id].next)
    //         } else {
    //             (None, block.start)
    //         }
    //     };
    //     println!("WHEN INSERTING {:?} PREVIOUS: {:?}, NEXT: {:?}", instruction_id, previous, next);
    //
    //     let instruction = &mut frame.instructions[instruction_id];
    //
    //     instruction.block = Some(block_id);
    //     instruction.previous = previous;
    //     instruction.next = next;
    //     block.size += 1;
    //
    //     if let Some(prev) = previous {
    //         frame.instructions[prev].next = Some(instruction_id);
    //     } else {
    //         block.start = Some(instruction_id);
    //     }
    //
    //     if let Some(next) = next {
    //         frame.instructions[next].previous = Some(instruction_id);
    //     } else {
    //         block.end = Some(instruction_id);
    //     }
    // }

    pub fn add_instruction(frame: &mut BlockFrame, block_id: usize, instruction: InstructionData)  -> usize {
        let block = &mut frame.blocks[block_id];
        let old_last = block.end;

        let id = frame.instructions.len();
        if let Some(end_id) = old_last {
            frame.instructions[end_id].next = Some(id);
            block.end = Some(id);
            block.size += 1;
        } else {
            block.start = Some(id);
            block.end = Some(id);
            block.size = 1;
        }

        frame.instructions.push(Instruction {
            data: instruction,
            previous: old_last,
            next: None,
            block: Some(block_id),
            id: frame.instructions.len(),
            // function
        });

        id
    }
    pub fn new_block(&self, blocks: &mut Vec<Block>, current_block: Option<usize>) -> usize {
        let id = blocks.len();
        let block = Block {
            previous_block: current_block,
            start: None,
            end: None,
            size: 0,
            id,
        };
        blocks.push(block);

        id
    }

    pub fn remove_instruction(
        // instructions: &mut IndexVec<usize, Instruction>,
        frame: &mut BlockFrame,
        block_id: usize,
        // blocks: &mut Vec<Block>,
        instruction_id: usize
    ) -> Option<usize> {
        let block = &mut frame.blocks[block_id];
        let instruction = &mut frame.instructions[instruction_id];
        // let block = &mut blocks[instruction.block?];

        println!("instr: {:?}", instruction.data);
        let old_previous = instruction.previous;
        let old_next = instruction.next;

        println!("instruction id {:?} next: {:?} prev: {:?}", instruction.id, instruction.previous, instruction.next);
        if block.id != instruction.block.unwrap() {
            panic!("it doesnt match? {:?} != {:?}", block.id, instruction.block);
        }
        println!("previous: {:?}, next: {:?}", old_previous, old_next);
        instruction.previous = None;
        instruction.next = None;
        instruction.block = None;

        if let Some(previous) = old_previous {
            frame.instructions[previous].next = old_next;
        } else {
            block.start = old_next;
            // if instruction_id != 0 {
            //     panic!("failed to set start for the instruction {:?} whih is in bloick id {:?}", instruction_id, block.id);
            //
            // }
            println!("setting block {:?} start to {:?}", block.id, old_next);
        }

        if let Some(next) = old_next {
            frame.instructions[next].previous = old_previous;
        } else {
            block.end = old_previous;
            println!("setting block end to {:?}", old_previous);
        }

        block.size -= 1;
        println!("block start is: {:?}", block.start);

        old_next
    }

    pub fn for_block_mut
    <F: FnMut(
        &mut IndexVec<usize, Functions>,
        usize,
        usize,
    )>(
        &mut self,
        // instructions: &mut IndexVec<usize, Instruction>,
        // blocks: &mut Vec<Block>,
        // frame: &mut BlockFrame,
        function_id: usize,
        block_id: usize,
        callback: &mut F
    ) {
        let mut at = self.functions[function_id].frame.blocks[block_id].start;
        while let Some(id) = at {
            let instruction = &mut self.functions[function_id].frame.instructions[id];
            if let InstructionData::Closure(func_id, _) = instruction.data {
                at = instruction.next;
                callback(&mut self.functions, function_id, id);
                for new_block in 0..self.functions[func_id].frame.blocks.len() {
                    self.for_block_mut(func_id, new_block, callback);
                }
                continue;
            }
            at = instruction.next;

            callback(&mut self.functions, function_id, id);
            println!("OK SO DATA2: {:?} ID {:?} FUNC: {:?}", self.functions[function_id].frame.instructions[id].data, id, function_id);
        }
    }
    // loops over every instruction and runs a function for every single argument they have
    pub fn block_replace_args<F: FnMut(&mut usize, &usize)>(
        &mut self,
        block: &Block,
        function_id: usize,
        mut callback: F,
    ) {
        let mut at = block.start;
        while let Some(id) = at {
            let mut instr = self.functions[function_id].frame.instructions[id].clone();
            Self::replace_instruction(0,  &mut instr.data, &mut callback);

            at = instr.next;

            self.functions[function_id].frame.instructions[id] = instr;
        }
    }
    // loops over every instruction and runs a function for every single argument they have
    pub fn replace_all_args<F: FnMut(&mut usize, &usize, &IndexVec<usize, Instruction>, usize)>(
        &mut self,
        // block: &Block,
        // function_id: usize,
        mut callback: F,
    ) {
        for function in &mut self.functions {
            for block in &mut function.frame.blocks {
                let mut at = block.start;
                while let Some(id) = at {
                    let mut instr = function.frame.instructions[id].clone();
                    println!("current: {:?} next is {:?} function {:?} instr data: {:?}", at, instr.next, function.id, instr.data);
                    // println!("instr before is {:?}", instr.data);
                    Self::replace_instruction(function.id,  &mut instr.data, &mut |instr, function_id| {
                        callback(instr, function_id, &function.frame.instructions, id);
                    });

                    at = instr.next;

                    // println!("instri s {:?}", instr.data);
                    function.frame.instructions[id] = instr;
                }
            }
        }
    }
    pub fn function_tree<F: FnMut(&usize, &mut IndexVec<usize, Functions>, Option<usize>)>(
        functions: &mut IndexVec<usize, Functions>,
        current_function: usize,
        target_previous: usize,
        callback: &mut F
    ) {
        // panic!("current function {:?} == {:?}, the predecessor {:?}", current_function, target_previous, functions[current_function].predecessor_function);
        let function_id = functions[current_function].id;
        if function_id == target_previous {
            return;
        }
        // let function = &self.functions[current_function];
        if let Some(previous_id) = functions[current_function].predecessor_function {
            callback(&current_function, functions, Some(previous_id));
            Self::function_tree(functions, previous_id, target_previous, callback);
        } else {
            callback(&current_function, functions, None);
        }
    }

    pub fn instruction_args<F: FnMut(&usize, &usize)>(
        function_id: usize,
        instruction: &InstructionData,
        callback: &mut F,
    )  {
        match instruction {
            InstructionData::Print(data) => callback(data, &function_id),
            InstructionData::MathOperands(variant, num_1, num_2) => {
                callback(num_1, &function_id);
                callback(num_2, &function_id);
            }
            InstructionData::ComparisonOperands(variant, num_1, num_2) => {
                callback(num_1, &function_id);
                callback(num_2, &function_id);
            }
            InstructionData::CallFunction(num_1, args) => {
                // TODO: might need to do a iterator with callback on every single argument of call
                // function
                callback(num_1, &function_id);
                for arg in args {
                    callback(arg, &function_id);
                }
            }
            // InstructionData::Upindex(func, num_1) => {
            //     callback(func, num_1);
            // }
            InstructionData::JumpConditional(instruction, v1, v2) => callback(instruction, &function_id),

            // TODO: add more comparisons, return, calls, tuples, lists here too
            e => {
                // unimplemented!("{:?}", e)
            }
        }
    }
    pub fn replace_instruction<F: FnMut(&mut usize, &usize)>(
        function_id: usize,
        instruction: &mut InstructionData,
        callback: &mut F,
    ) -> Option<InstructionData> {
        match instruction {
            InstructionData::Copy(data) =>  {  println!("copy before {:?}", data); callback(data, &function_id); println!("copy should be {:?}", data); Some(InstructionData::Copy(*data))},
            InstructionData::Print(data) => { callback(data, &function_id); Some(InstructionData::Print(*data)) },
            InstructionData::MathOperands(variant, num_1, num_2) => {
                callback(num_1, &function_id);
                callback(num_2, &function_id);

                Some(InstructionData::MathOperands(variant.clone(), *num_1, *num_2))
            }
            InstructionData::ComparisonOperands(variant, num_1, num_2) => {
                callback(num_1, &function_id);
                callback(num_2, &function_id);

                Some(InstructionData::ComparisonOperands(variant.clone(), *num_1, *num_2))
            }
            InstructionData::CallFunction(num_1, args) => {
                // TODO: might need to do a iterator with callback on every single argument of call
                // function
                callback(num_1, &function_id);
                for arg in &mut *args {
                    callback(arg, &function_id);
                }

                Some(InstructionData::CallFunction(*num_1, args.to_vec()))
            }

            InstructionData::JumpConditional(instruction, v1, v2) => { callback(instruction, &function_id); Some(InstructionData::JumpConditional(*instruction, *v1, *v2))},

            // TODO: add more comparisons, return, calls, tuples, lists here too
            e => {
                // unimplemented!("{:?}", e)
                None
            }
        }
    }
}

impl BlockFrame {
    pub fn new(function_id: usize, phi_instructions: Option<IndexVec<usize, Vec<(usize, usize)>>>) -> Self {
        Self {
            instructions: IndexVec::new(),
            blocks: Vec::new(),
            id: function_id,
            phi_instructions
        }
    }
}
impl Instruction {
    fn has_value(&self) -> bool {
        match self.data {
            InstructionData::SetVariable(_, _) |
            InstructionData::SetUpvalue(_, _, _) |
            InstructionData::Jump(_) |
            InstructionData::MakeLocal(_) => false,
            _ => true
        }
    }
}
