// IMPORTANT: rewrite everything once i got a fundemental understanding of what to do

use vm::VM;

mod vm;

// NOTE: this runs a repl for quick test
// maybe in the future i could support this feature
fn main() {
    let mut language = VM::new();

    language.run();
}
