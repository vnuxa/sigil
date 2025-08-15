use std::clone;




#[derive(Debug, PartialEq)]
pub enum Opcode {
    // Halt,
    Load = 1,
    Add = 2,
    Subtract = 3,
    Multiply = 4,
    Divide = 5,
    Remainder = 20,
    Jump = 6,
    RelativeJumpForward = 7,
    RelativeJumpBackward = 8,
    Equal = 9,
    NotEqual = 10,
    GreaterThan = 11,
    LessThan = 12,
    GreaterThanOrEqual = 13,
    LessThanOrEqual = 14,
    JumpIfEqual = 15,
    JumpConditional = 16,
    Allocate = 17,
    Free = 18,
    Print = 19,
    CallFunction = 21,
    Return = 22,
    Closure = 23,
    Upindex = 24,
    Illegal = 99,
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            // 0 => Opcode::Halt,
            1 => Opcode::Load,
            2 => Opcode::Add,
            3 => Opcode::Subtract,
            4 => Opcode::Multiply,
            5 => Opcode::Divide,
            20 => Opcode::Remainder,
            6 => Opcode::Jump,
            7 => Opcode::RelativeJumpForward,
            8 => Opcode::RelativeJumpBackward,
            9 => Opcode::Equal,
            10 => Opcode::NotEqual,
            11 => Opcode::GreaterThan,
            12 => Opcode::LessThan,
            13 => Opcode::GreaterThanOrEqual,
            14 => Opcode::LessThanOrEqual,
            15 => Opcode::JumpIfEqual,
            16 => Opcode::JumpConditional,
            17 => Opcode::Allocate,
            18 => Opcode::Free,
            19 => Opcode::Print,
            21 => Opcode::CallFunction,
            22 => Opcode::Return,
            23 => Opcode::Closure,
            24 => Opcode::Upindex,
            e => { println!("Illegal {:?}", e); Opcode::Illegal },
        }
    }
}

pub struct Thread {

}

pub struct CallFrame {
    pub function: usize,
    // used for returns
    pub origin_pc: usize,
    // where allocating registers begin for this callfame
    pub offset: usize,
    pub internal_counter: usize,
    pub previous_offset: usize, // previous callframe register offset
}

#[derive(Clone)]
pub struct FunctionPrototype {
    pub instructions: Vec<u8>,
    // pub internal_counter: usize,
    // the minimum register where the function lives at
    // pub base: usize,
    pub number_params: usize,

    // will be initialized with params
    pub locals: Vec<u8>,

}

#[derive(Clone, Debug)]
pub enum SigilTypes {
    Number(i32),
    Function(i32), // function id
    Reference(i32), // register id, used in upvalues
    Empty
}

pub struct Upvalue {
    pub register_refrence: usize,
}

pub struct VM {
    // array that simulates hardware registers
    // TODO: maybe replace this with a vec in the future, and maybe change the i32 to a i64
    pub registers: Vec<SigilTypes>,
    // a list of active call frames
    pub functions: Vec<FunctionPrototype>,
    // upvalues, which are refrences to values
    upvalues: Vec<Upvalue>,
    // to store global variables
    global_registers: Vec<i32>,
    // program counter that keeps track of which byte is being executed
    program_counter: usize,
    // all of the bytes to execute
    pub program: Vec<u8>,
    // used to calculate the remainder for the divisional operator
    remainder: u32,
    // contains the result of the last comparison operator
    equal_flag: bool,
    highest_register: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            // registers: [0; 32],
            registers: Vec::new(),
            functions: Vec::new(),
            global_registers: Vec::new(),
            program_counter: 0,
            program: Vec::new(),
            remainder: 0,
            equal_flag: false,
            upvalues: Vec::new(),
            highest_register: 0,
        }
    }

    // pub fn run(&mut self) {
    //     while !self.execute_instruction() {}
    // }

    pub fn execute_function(&mut self, call_frame: &mut CallFrame ) {
        let mut function = self.functions[call_frame.function].clone();

        let old_highest = self.highest_register;
        println!("!! highest rn is {:?}", old_highest);
        while !self.execute_instruction(call_frame) {}

        // TODO: make it so that this will work with function return values
        self.highest_register = old_highest;
        // later, use an for with an if, you loop over every single register and see if its in use
        // (use defined by reference counting)
        // then if its used, increment a variable like current_height and if the height of the
        // registers matches the required sum that would be needed to execute said function
        // then you use that offset
        // if its not used, you reset the current_height counter

        // NOTE: find a better solution than cloning!
        self.functions[call_frame.function] = function;
    }

    pub fn execute_instruction(&mut self, call_frame: &mut CallFrame) -> bool {
        let instructions = &mut self.functions[call_frame.function].instructions;
        println!("{:?} > {:?}",call_frame.internal_counter, instructions.len());
        println!("HIGHEST REGISTER: {:?}", self.highest_register);
        println!("{:?}", self.registers);
        println!("function id is {:?}", call_frame.function);
        if call_frame.internal_counter > instructions.len() - 1 {
            return true;
        }

        let decoded_opcode = Self::decode_opcode(instructions, &mut call_frame.internal_counter);
        println!("handling {:?}", decoded_opcode);
        match decoded_opcode {
            Opcode::Load => {
                let number = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as i32;
                let register = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                // self.registers[register + call_frame.offset] = SigilTypes::Number(number);
                let register_location = Self::get_register_id(&self.registers, register, call_frame.offset);
                println!("in the load there is a number {:?}", number);
                self.registers[register_location] = SigilTypes::Number(number);

                return false;
            }
            Opcode::Add => {
                let register_id_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_3 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                println!("offset is {:?}, id {:?} id 2 {:?} id 3 {:?}", call_frame.offset, register_id_1, register_id_2, register_id_3);
                // IMPORTANT: IMPROVE THE ERROR SYSTEM TO SHOW EXACTLY WHERE THE ERROR IS!!
                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_id_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_id_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_2) };

                if register_id_3 + call_frame.offset > self.highest_register {
                    self.highest_register = register_id_3 + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register_id_3, call_frame.offset);
                self.registers[register_location] = SigilTypes::Number(first_value + second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = SigilTypes::Number(first_value + second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = Self::get_register(register_id_1, call_frame.offset) + Self::get_register(register_id_2, call_frame.offset);
            }
            Opcode::Subtract => {
                let register_id_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_3 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_id_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_id_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_2) };

                if register_id_3 > self.highest_register {
                    self.highest_register = register_id_3 + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register_id_3, call_frame.offset);
               self.registers[register_location] = SigilTypes::Number(first_value - second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = Self::get_register(register_id_1, call_frame.offset) - Self::get_register(register_id_2, call_frame.offset);
            }
            Opcode::Multiply => {
                let register_id_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_3 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;


                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_id_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_id_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_2) };

                if register_id_3 + call_frame.offset > self.highest_register {
                    self.highest_register = register_id_3 + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register_id_3, call_frame.offset);
                self.registers[register_location] = SigilTypes::Number(first_value * second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = Self::get_register(register_id_1, call_frame.offset) * Self::get_register(register_id_2, call_frame.offset);
            }
            Opcode::Divide => {
                let register_id_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_3 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_id_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_id_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_2) };

                if register_id_3 + call_frame.offset > self.highest_register {
                    self.highest_register = register_id_3 + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register_id_3, call_frame.offset);
                self.registers[register_location] = SigilTypes::Number(first_value / second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = Self::get_register(register_id_1, call_frame.offset) / Self::get_register(register_id_2, call_frame.offset);
            }
            Opcode::Remainder => {
                let register_id_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_id_3 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_id_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_id_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_id_2) };

                if register_id_3 + call_frame.offset > self.highest_register {
                    self.highest_register = register_id_3 + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register_id_3, call_frame.offset);
                self.registers[register_location] = SigilTypes::Number(first_value % second_value);
                // Self::get_register(&self.registers, register_id_3, call_frame.offset) = Self::get_register(register_id_1, call_frame.offset) % Self::get_register(register_id_2, call_frame.offset);
            }
            Opcode::Jump => {
                let target = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                // IMPORTANT: this should probably just be directly target, no need for getting it
                // in register
                call_frame.internal_counter = target;
            }
            Opcode::Equal => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                println!("!!!!!!!!! OK SO FIRST VALUE  {:?} == {:?}", first_value, second_value);
                self.equal_flag = first_value == second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::NotEqual => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value != second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::GreaterThan => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value > second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::LessThan => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value < second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::GreaterThanOrEqual => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value >= second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::LessThanOrEqual => {
                let register_1 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let register_2 = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;

                let SigilTypes::Number(first_value) = Self::get_register(&self.registers, register_1, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = Self::get_register(&self.registers, register_2, call_frame.offset) else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value <= second_value;

                Self::next_8_bits(instructions, &mut call_frame.internal_counter);
            }
            Opcode::JumpIfEqual => {
                println!("ok so the equal flag is {:?}", self.equal_flag);
                if self.equal_flag {
                    println!("GOIGN TO THE EQUAL FLAG CONDITION");
                    let target = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                    call_frame.internal_counter = target;
                }
            }
            Opcode::JumpConditional => {
                let target_true = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                let target_false = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                println!("WITHIN JUMPCONDITIONAL {:?} {:?}", target_true, target_false);
                println!("EQUAL FLAG IS {:?}", self.equal_flag);
                if self.equal_flag {
                    // let target = Self::next_8_bits() as usize;
                    call_frame.internal_counter = target_true;
                } else {
                    call_frame.internal_counter = target_false;
                }

            }
            // Opcode::Allocate => {
            //     let bytes = self.registers[Self::next_8_bits() as usize] as usize;
            //     self.heap.resize(self.heap.len() + bytes, 0);
            // }
            // Opcode::Free => {}
            Opcode::Print => {
                let register_id = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                // let register = self.registers[Self::next_8_bits() as usize];
                println!("registers: {:?}", self.registers);
                println!(
                    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OUTPUT OF A PRINT: {:?} IT HAS REGISTER ID {:?}",
                    Self::get_register(&self.registers, register_id, call_frame.offset),
                    register_id
                );
            }
            Opcode::CallFunction => {
                let function_register = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                println!("|||||||||||||||||||||||||||||||| FUNCTION REGISTER {:?} and highest is {:?}", function_register, self.highest_register);
                // IMPORTANT: MAKE PROPER ERROR SYSTEM!
                // it cant handle recursion yet because it cannot get its outer enviroment
                //
                // BUG: previously this used to use function_register without the offset
                // now it will have an offset
                // dont know if it will cause a bug or not
                let SigilTypes::Function(function_id) = Self::get_register(&self.registers, function_register, call_frame.offset) else { panic!("not a function in register: {:?} || {:?}", function_register, self.registers) };

                // println!(" the amount of arguments for this functon is {:?}", self.functions[function_id as usize].number_params );
                // NOTE: i have no clue if this works
                let (functions, new_functions) = self.functions.split_at_mut(call_frame.function);
                let mut new_callframe = CallFrame::new(*function_id as usize, self.highest_register + 1, call_frame.offset);
                for index in 0..new_functions[*function_id as usize - call_frame.function].number_params {
                    let register = Self::next_8_bits(&mut functions[call_frame.function].instructions, &mut call_frame.internal_counter) as usize;
                    // NOTE: this should only clone if it mutates
                    let register_location = Self::get_register_id(&self.registers, self.highest_register + index, call_frame.offset);
                    self.registers[register_location] = self.registers[register + call_frame.offset].clone();
                    // self.registers[self.highest_register + index + call_frame.offset] = self.registers[register + call_frame.offset].clone();
                }

                println!("ok so setting highest to offset: {:?}", self.highest_register);
                self.execute_function(&mut new_callframe);
                // self.execute_function(function_id as usize);
            }
            Opcode::Closure => {
                let number = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as i32;
                let register = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                if register + call_frame.offset > self.highest_register {
                    println!("_______ seting new highest register {:?}", register + call_frame.offset);
                    self.highest_register = register + call_frame.offset;
                }
                let register_location = Self::get_register_id(&self.registers, register, call_frame.offset);
                self.registers[register_location] = SigilTypes::Function(number);
            }
            Opcode::Upindex => {
                let original_register = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as i32;
                let register = Self::next_8_bits(instructions, &mut call_frame.internal_counter) as usize;
                if register + call_frame.offset > self.highest_register {
                    self.highest_register = register + call_frame.offset;
                }

                println!("|| original register {:?}, within function {:?}", original_register, call_frame.function);
                self.registers[register + call_frame.offset] = SigilTypes::Reference(original_register + call_frame.previous_offset as i32);
            }
            e => {
                // println!("Unkown opcode {:?}", e as u8);
            }
        }

        false
    }

    // primarily used in tests
    // pub fn run_once(&mut self) {
    //     self.execute_instruction();
    // }

    fn decode_opcode(program: &mut Vec<u8>, program_counter: &mut usize) -> Opcode {
        println!("on program counter {:?}", program_counter);
        println!("program: {:?}", program[*program_counter]);
        let opcode = Opcode::from(program[*program_counter]);
        *program_counter += 1;

        opcode
    }
    fn get_register(registers: &Vec<SigilTypes>, register_id: usize, offset: usize) -> &SigilTypes {
        let output = &registers[register_id + offset];
        if let SigilTypes::Reference(new_id) = output {
            // println!("got reference with id {:?}", new_id);
            return Self::get_register(registers, *new_id as usize, 0);
        }

        output
    }
    fn get_register_id(registers: &Vec<SigilTypes>, register_id: usize, offset: usize) -> usize {
        let output = &register_id + offset;
        if let SigilTypes::Reference(new_id) = &registers[output] {
            return Self::get_register_id(registers, *new_id as usize, 0);
        }

        output
    }

    fn next_8_bits(program: &mut Vec<u8>, program_counter: &mut usize) -> u8 {
        let result = program[*program_counter];
        *program_counter += 1;
        result
    }

    // NOTE: i need this for larger thna 8 bytes integers

    // fn next_16_bits(&mut self) -> u16 {
    //     let result = ((self.program[self.program_counter] as u16) << 8)
    //         | self.program[self.program_counter + 1] as u16;
    //
    //     self.program_counter += 2;
    //     result
    // }
}

impl CallFrame {
    pub fn new(function: usize, new_offset: usize, old_offset: usize) -> Self {
        CallFrame {
            internal_counter: 0,
            offset: new_offset,
            previous_offset: old_offset,
            origin_pc: 0, // IMPORTANT: change this later
            function
        }
    }
}
