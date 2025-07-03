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
    pub base: usize,
    pub internal_counter: usize,
    pub instructions: Vec<Opcode>
}

#[derive(Clone)]
pub struct FunctionPrototype {
    pub instructions: Vec<u8>,
    pub internal_counter: usize,
    // the minimum register where the function lives at
    pub base: usize,

    pub number_params: usize,

    // will be initialized with params
    pub locals: Vec<u8>,


}

#[derive(Clone, Debug)]
pub enum SigilTypes {
    Number(i32),
    Function(i32), // function id
}

pub struct Upvalue {
    pub register_refrence: usize,
}

pub struct VM {
    // array that simulates hardware registers
    // TODO: maybe replace this with a vec in the future, and maybe change the i32 to a i64
    pub registers: Vec<SigilTypes>,
    // a list of active call frames
    pub call_frames: Vec<CallFrame>,
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
            call_frames: Vec::new(),
        }
    }

    // pub fn run(&mut self) {
    //     while !self.execute_instruction() {}
    // }

    pub fn execute_function(&mut self, function_id: usize) {
        let mut function = self.functions[function_id].clone();

        while !self.execute_instruction(&mut function.internal_counter, &mut function.instructions) {}

        // NOTE: find a better solution than cloning!
        self.functions[function_id] = function;
    }

    pub fn execute_instruction(&mut self, program_counter: &mut usize, program: &mut Vec<u8>) -> bool {
        println!("{:?} > {:?}",program_counter, program.len());
        if *program_counter > program.len() - 1 {
            return true;
        }

        let decoded_opcode = self.decode_opcode(program, program_counter);
        println!("handling {:?}", decoded_opcode);
        match decoded_opcode {
            Opcode::Load => {
                let number = self.next_8_bits(program, program_counter) as i32;
                let register = self.next_8_bits(program, program_counter) as usize;
                self.registers[register] = SigilTypes::Number(number);

                return false;
            }
            Opcode::Add => {
                let register_id_1 = self.next_8_bits(program, program_counter) as usize;
                let register_id_2 = self.next_8_bits(program, program_counter) as usize;
                let register_id_3 = self.next_8_bits(program, program_counter) as usize;

                // IMPORTANT: IMPROVE THE ERROR SYSTEM TO SHOW EXACTLY WHERE THE ERROR IS!!
                let SigilTypes::Number(first_value) = self.registers[register_id_1] else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = self.registers[register_id_2] else { panic!("Expected a number value within register {:?}", register_id_2) };
                self.registers[register_id_3] = SigilTypes::Number(first_value + second_value);
                // self.registers[register_id_3] = self.registers[register_id_1] + self.registers[register_id_2];
            }
            Opcode::Subtract => {
                let register_id_1 = self.next_8_bits(program, program_counter) as usize;
                let register_id_2 = self.next_8_bits(program, program_counter) as usize;
                let register_id_3 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_id_1] else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = self.registers[register_id_2] else { panic!("Expected a number value within register {:?}", register_id_2) };
                self.registers[register_id_3] = SigilTypes::Number(first_value - second_value);
                // self.registers[register_id_3] = self.registers[register_id_1] - self.registers[register_id_2];
            }
            Opcode::Multiply => {
                let register_id_1 = self.next_8_bits(program, program_counter) as usize;
                let register_id_2 = self.next_8_bits(program, program_counter) as usize;
                let register_id_3 = self.next_8_bits(program, program_counter) as usize;


                let SigilTypes::Number(first_value) = self.registers[register_id_1] else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = self.registers[register_id_2] else { panic!("Expected a number value within register {:?}", register_id_2) };
                self.registers[register_id_3] = SigilTypes::Number(first_value * second_value);
                // self.registers[register_id_3] = self.registers[register_id_1] * self.registers[register_id_2];
            }
            Opcode::Divide => {
                let register_id_1 = self.next_8_bits(program, program_counter) as usize;
                let register_id_2 = self.next_8_bits(program, program_counter) as usize;
                let register_id_3 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_id_1] else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = self.registers[register_id_2] else { panic!("Expected a number value within register {:?}", register_id_2) };
                self.registers[register_id_3] = SigilTypes::Number(first_value / second_value);
                // self.registers[register_id_3] = self.registers[register_id_1] / self.registers[register_id_2];
            }
            Opcode::Remainder => {
                let register_id_1 = self.next_8_bits(program, program_counter) as usize;
                let register_id_2 = self.next_8_bits(program, program_counter) as usize;
                let register_id_3 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_id_1] else { panic!("Expected a number value within register {:?}", register_id_1) };
                let SigilTypes::Number(second_value) = self.registers[register_id_2] else { panic!("Expected a number value within register {:?}", register_id_2) };
                self.registers[register_id_3] = SigilTypes::Number(first_value % second_value);
                // self.registers[register_id_3] = self.registers[register_id_1] % self.registers[register_id_2];
            }
            Opcode::Jump => {
                let target = self.next_8_bits(program, program_counter) as usize;

                // IMPORTANT: this should probably just be directly target, no need for getting it
                // in register
                *program_counter = target;
            }
            Opcode::Equal => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value == second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::NotEqual => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value != second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::GreaterThan => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value > second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::LessThan => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value < second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::GreaterThanOrEqual => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value >= second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::LessThanOrEqual => {
                let register_1 = self.next_8_bits(program, program_counter) as usize;
                let register_2 = self.next_8_bits(program, program_counter) as usize;

                let SigilTypes::Number(first_value) = self.registers[register_1] else { panic!("Expected a number value within register {:?}", register_1) };
                let SigilTypes::Number(second_value) = self.registers[register_2] else { panic!("Expected a number value within register {:?}", register_2) };
                self.equal_flag = first_value <= second_value;

                self.next_8_bits(program, program_counter);
            }
            Opcode::JumpIfEqual => {
                if self.equal_flag {
                    let target = self.next_8_bits(program, program_counter) as usize;
                    *program_counter = target;
                }
            }
            Opcode::JumpConditional => {
                let target_true = self.next_8_bits(program, program_counter) as usize;
                let target_false = self.next_8_bits(program, program_counter) as usize;
                println!("WITHIN JUMPCONDITIONAL {:?} {:?}", target_true, target_false);
                println!("EQUAL FLAG IS {:?}", self.equal_flag);
                if self.equal_flag {
                    // let target = self.next_8_bits() as usize;
                    *program_counter = target_true;
                } else {
                    *program_counter = target_false;
                }

            }
            // Opcode::Allocate => {
            //     let bytes = self.registers[self.next_8_bits() as usize] as usize;
            //     self.heap.resize(self.heap.len() + bytes, 0);
            // }
            // Opcode::Free => {}
            Opcode::Print => {
                let register_id = self.next_8_bits(program, program_counter) as usize;
                // let register = self.registers[self.next_8_bits() as usize];
                println!(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OUTPUT OF A PRINT: {:?} IT HAS REGISTER ID {:?}", self.registers[register_id], register_id);
            }
            Opcode::CallFunction => {
                let function_register = self.next_8_bits(program, program_counter) as usize;
                // IMPORTANT: MAKE PROPER ERROR SYSTEM!
                let SigilTypes::Function(function_id) = self.registers[function_register] else { panic!("not a function in register: {:?}", function_register) };

                self.execute_function(function_id as usize);
            }
            Opcode::Closure => {
                let number = self.next_8_bits(program, program_counter) as i32;
                let register = self.next_8_bits(program, program_counter) as usize;
                self.registers[register] = SigilTypes::Function(number);
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

    fn decode_opcode(&mut self, program: &mut Vec<u8>, program_counter: &mut usize) -> Opcode {
        println!("on program counter {:?}", program_counter);
        println!("program: {:?}", program[*program_counter]);
        let opcode = Opcode::from(program[*program_counter]);
        *program_counter += 1;

        opcode
    }

    fn next_8_bits(&mut self,  program: &mut Vec<u8>, program_counter: &mut usize) -> u8 {
        let result = program[*program_counter];
        *program_counter += 1;
        result
    }

    fn next_16_bits(&mut self) -> u16 {
        let result = ((self.program[self.program_counter] as u16) << 8)
            | self.program[self.program_counter + 1] as u16;

        self.program_counter += 2;
        result
    }
}
