// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

// use bytecode::main::VM;
use interpreter::parser::generate;
//
// mod bytecode;
mod interpreter;
// mod vm;

// NOTE: this runs a repl for quick test
// maybe in the future i could support this feature
fn main() {
    // let mut language = VM::new();
    //
    // language.run();
    let input = "

global test = 2 + 4

"
    .to_string();
    generate(input);
}
