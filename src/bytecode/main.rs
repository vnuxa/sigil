#[derive(Debug, PartialEq)]
pub enum Opcode {
    // Halt,
    Load,
    Add,
    Subtract,
    Multiply,
    Divide,
    Jump,
    RelativeJumpForward,
    RelativeJumpBackward,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    JumpIfEqual,
    JumpIfNotEqual,
    Allocate,
    Free,
    Print,
    Illegal,
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
            16 => Opcode::JumpIfNotEqual,
            17 => Opcode::Allocate,
            18 => Opcode::Free,
            19 => Opcode::Print,
            _ => Opcode::Illegal,
        }
    }
}

pub struct VM {
    // array that simulates hardware registers
    // TODO: maybe replace this with a vec in the future, and maybe change the i32 to a i64
    registers: Vec<i32>,
    // to store global variables
    global_registers: Vec<i32>,
    // program counter that keeps track of which byte is being executed
    program_counter: usize,
    // all of the bytes to execute
    program: Vec<u8>,
    // used to calculate the remainder for the divisional operator
    remainder: u32,
    // contains the result of the last comparison operator
    equal_flag: bool,
    // heap
    heap: Vec<u8>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            // registers: [0; 32],
            registers: Vec::new(),
            global_registers: Vec::new(),
            program_counter: 0,
            program: Vec::new(),
            remainder: 0,
            equal_flag: false,
            heap: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        while !self.execute_instruction() {}
    }

    pub fn execute_instruction(&mut self) -> bool {
        if self.program_counter > self.program.len() {
            return true;
        }

        match self.decode_opcode() {
            // Opcode::Halt => {
            //     println!("Halt encountered!");
            //     return false;
            // }
            Opcode::Load => {
                let register = self.next_8_bits() as usize;
                let number = self.next_16_bits() as i32;
                self.registers[register] = number;

                return false;
            }
            Opcode::Add => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.registers[self.next_8_bits() as usize] = register_1 + register_2;
            }
            Opcode::Subtract => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.registers[self.next_8_bits() as usize] = register_1 - register_2;
            }
            Opcode::Multiply => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.registers[self.next_8_bits() as usize] = register_1 * register_2;
            }
            Opcode::Divide => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.registers[self.next_8_bits() as usize] = register_1 / register_2;
                self.remainder = (register_1 % register_2) as u32;
            }
            Opcode::Jump => {
                let target = self.registers[self.next_8_bits() as usize];

                self.program_counter = target as usize;
            }
            Opcode::RelativeJumpForward => {
                let value = self.registers[self.next_8_bits() as usize];
                self.program_counter += value as usize;
            }
            Opcode::RelativeJumpBackward => {
                let value = self.registers[self.next_8_bits() as usize];
                self.program_counter -= value as usize;
            }

            Opcode::Equal => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 == register_2;

                self.next_8_bits();
            }
            Opcode::NotEqual => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 != register_2;

                self.next_8_bits();
            }
            Opcode::GreaterThan => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 > register_2;

                self.next_8_bits();
            }
            Opcode::LessThan => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 < register_2;

                self.next_8_bits();
            }
            Opcode::GreaterThanOrEqual => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 >= register_2;

                self.next_8_bits();
            }
            Opcode::LessThanOrEqual => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                let register_2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = register_1 <= register_2;

                self.next_8_bits();
            }
            Opcode::JumpIfEqual => {
                if self.equal_flag {
                    self.program_counter = self.registers[self.next_8_bits() as usize] as usize;
                }
            }
            Opcode::JumpIfNotEqual => {
                if !self.equal_flag {
                    self.program_counter = self.registers[self.next_8_bits() as usize] as usize;
                }
            }
            Opcode::Allocate => {
                let bytes = self.registers[self.next_8_bits() as usize] as usize;
                self.heap.resize(self.heap.len() + bytes, 0);
            }
            Opcode::Free => {}
            Opcode::Print => {
                let register_1 = self.registers[self.next_8_bits() as usize];
                println!("{:?}", register_1);
            }

            _ => {
                println!("Unkown opcode");
            }
        }

        false
    }

    // primarily used in tests
    pub fn run_once(&mut self) {
        self.execute_instruction();
    }

    fn decode_opcode(&mut self) -> Opcode {
        let opcode = Opcode::from(self.program[self.program_counter]);
        self.program_counter += 1;

        opcode
    }

    fn next_8_bits(&mut self) -> u8 {
        let result = self.program[self.program_counter];
        self.program_counter += 1;
        result
    }

    fn next_16_bits(&mut self) -> u16 {
        let result = ((self.program[self.program_counter] as u16) << 8)
            | self.program[self.program_counter + 1] as u16;

        self.program_counter += 2;
        result
    }
}
