use std::{rc::Rc, sync::Arc};

use logos::{Lexer, Logos};

use chumsky::prelude::*;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Expressions {
    Bool(bool),
    Int(isize),
    Variable(String),
    String(String),

    Negative(Box<Expressions>), // makes a number negative

    Add(Box<Expressions>, Box<Expressions>),
    Subtract(Box<Expressions>, Box<Expressions>),
    Multiply(Box<Expressions>, Box<Expressions>),
    Divide(Box<Expressions>, Box<Expressions>),
    Remainder(Box<Expressions>, Box<Expressions>),
    ConcatenateString(Box<Expressions>, Box<Expressions>),

    CompareEqual(Box<Expressions>, Box<Expressions>),
    CompareNotEqual(Box<Expressions>, Box<Expressions>),
    CompareLessThan(Box<Expressions>, Box<Expressions>),
    CompareMoreThan(Box<Expressions>, Box<Expressions>),
    CompareLessEquals(Box<Expressions>, Box<Expressions>),
    CompareMoreEquals(Box<Expressions>, Box<Expressions>),

    GlobalDefine(Box<Expressions>, Option<Box<Expressions>>), // variable name, variable definition
    LocalDefine(Box<Expressions>, Option<Box<Expressions>>),  // variable name, variable definition
    Access(Box<Expressions>, Box<Expressions>),
    Index(Box<Expressions>, Box<Expressions>), // objeect, index
    CallFunction(Box<Expressions>, Vec<Expressions>), // function name, function params
    FunctionBlock(Box<Expressions>, Vec<Expressions>, Box<Expressions>), // name, variable arguments, scope

    Assign(String, Box<Expressions>), // Variable, value
    Scope(Vec<Expressions>),
    If(Box<Expressions>, Box<Expressions>),

    EndOfFile,
    ScopeEnd,
    ParanthesesEnd
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"\s+")]
#[logos(error = String)]
enum Tokens {
    #[token("+")]
    Add,

    #[token("-")]
    Subtract,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Remainder,

    #[token(".")]
    Access,

    #[token(":")]
    AccessImpl,

    #[token("or")]
    Or,

    #[token("and")]
    And,

    #[token("==")]
    CompareEqual,

    #[token("!=")]
    CompareNotEqual,

    #[token("<")]
    CompareLessThan,

    #[token(">")]
    CompareMoreThan,

    #[token("<=")]
    CompareLessEquals,

    #[token(">=")]
    CompareMoreEquals,

    #[token("=", priority = 10)]
    Assign,

    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap())]
    Integer(isize),

    // statement tokens
    //
    #[token("[")]
    SquaredBracketOpen,

    #[token("]")]
    SquaredBracketClose,

    #[token("(")]
    ParenthesesOpen,

    #[token(")")]
    ParenthesesClose,

    #[token("{")]
    ScopeOpen,

    #[token("}")]
    ScopeClose,

    #[token("..")]
    ConcatenateString,

    // #[regex("[A-Za-z0-9]+")]
    // IMPORTANT: make the variable definition function work
    // #[regex(r"[[:alpha:]][[:alnum:]]*", variable_definition)]
    #[regex(r"(\w+)",priority = 1, callback = |lex| {
        let value = lex.slice().to_string();
        println!("this was tried: {:?}", value);
        value
    })]
    // #[regex(r"(\w+)")]
    Variable(String),

    // #[regex(r"[[:alpha:]][[:alnum:]]*", string_definition)]
    // String(String),
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("global")]
    Global,

    #[token("local")]
    Local,

    #[token("if")]
    If,

    #[token("loop")]
    Loop,

    #[token("break")]
    Break,

    #[token("for")]
    For,

    // For((OperatorTokens, ))
    #[token("return")]
    Return,

    #[token("match")]
    Match,

    #[token("in")]
    In,

    #[token(",")]
    Comma,

    #[token("\"", string_definition)]
    String(String),

    #[token("fn")]
    FunctionDefinition,

    EndOfFile,
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum StringContext {
    #[token("\"")]
    DoubleQuote,

    #[regex(r#"[^"\\]+"#)]
    Content,

    #[token("\\")]
    EscapeSequence,
}

pub fn string_definition(lex: &mut Lexer<Tokens>) -> Option<String> {
    let mut str = String::new();
    let mut lex_2 = lex.clone().morph::<StringContext>();
    let mut has_escape = false;
    while let Some(Ok(token)) = lex_2.next() {
        match token {
            StringContext::Content => str.push_str(lex_2.slice()),
            StringContext::EscapeSequence => has_escape = true,
            StringContext::DoubleQuote => {
                // println!("has escape sequence: {:?}", has_escape);
                if has_escape {
                    has_escape = false;
                    str.push('"');
                } else {
                    break;
                }
            }
        }
        // println!("str is {:?}", str)
    }
    // println!("ok after all of that the next value is {:?}", lex_2.next());

    *lex = lex_2.morph();
    Some(str)
    // }
    // println!(
    //     "no quotation amrk..: {:?} and {:?}",
    //     lex.slice(),
    //     lex.remainder()
    // );
    //
    // None
}

impl Expressions {
    // will have no string implementation
    fn from_tokens(
        value: Tokens,
        left_side: Option<Expressions>,
        right_side: Option<Expressions>,
        many: Option<Vec<Expressions>>,
    ) -> Self {
        match value {
            Tokens::Add => {
                Expressions::Add(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::Subtract => {
                Expressions::Subtract(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::Multiply => {
                Expressions::Multiply(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::Divide => {
                Expressions::Divide(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::Remainder => {
                Expressions::Remainder(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::Access => {
                Expressions::Access(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::CompareEqual => Expressions::CompareEqual(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::SquaredBracketOpen => {
                println!("left side: {:?}, right side: {:?}", left_side, right_side);
                Expressions::Index(Box::new(left_side.unwrap()), Box::new(right_side.unwrap()))
            }
            Tokens::CompareNotEqual => Expressions::CompareNotEqual(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::CompareLessThan => Expressions::CompareLessThan(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::CompareMoreThan => Expressions::CompareMoreThan(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::CompareLessEquals => Expressions::CompareLessEquals(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::CompareMoreEquals => Expressions::CompareMoreEquals(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            Tokens::ParenthesesOpen => {
                Expressions::CallFunction(Box::new(left_side.unwrap()), many.unwrap())
            }
            Tokens::Assign => Expressions::Assign(
                if let Some(Expressions::Variable(var)) = left_side {
                    var
                } else {
                    panic!("Undeifned variable name")
                },
                Box::new(right_side.unwrap()),
            ),
            Tokens::ConcatenateString => Expressions::ConcatenateString(
                Box::new(left_side.unwrap()),
                Box::new(right_side.unwrap()),
            ),
            // TODO: implement more
            _ => unimplemented!(),
        }
    }

    // wikth string implementatio
}

// if this token can continue a provided statement
fn can_continue(token: Tokens) -> bool {
    match token {
        Tokens::Add => true,
        Tokens::Subtract => true,
        Tokens::Multiply => true,
        Tokens::Divide => true,
        Tokens::Remainder => true,

        Tokens::Access => true,
        Tokens::AccessImpl => true,

        Tokens::Or => true,
        Tokens::And => true,

        Tokens::CompareEqual => true,
        Tokens::CompareNotEqual => true,
        Tokens::CompareLessThan => true,
        Tokens::CompareMoreThan => true,
        Tokens::CompareLessEquals => true,
        Tokens::CompareMoreEquals => true,

        Tokens::Assign => true, // dont know if it should continue?
        Tokens::SquaredBracketOpen => true,
        Tokens::SquaredBracketClose => true,
        Tokens::ParenthesesOpen => true,
        Tokens::ParenthesesClose => true,

        Tokens::ConcatenateString => true,
        _ => false,
    }
}

// NOTE: figuere out hwo to amke it so that string concatination can work with all of this
fn parser(tokens: &mut Vec<Tokens>, min_power: u8) -> Expressions {
    // might want end of file handling here?
    let test = tokens.pop();
    println!("parsing {:?}", test);
    // let mut left_hand_side = match tokens.pop() {
    let mut left_hand_side = match test {
        Some(Tokens::Integer(number)) => Expressions::Int(number),
        Some(Tokens::Variable(var)) => Expressions::Variable(var),
        Some(Tokens::Bool(value)) => Expressions::Bool(value),
        // Tokens::Add => {
        //     // substract and add have the same binding power anyways
        //     let right_power = prefix_binding_power(&Tokens::Add);
        //
        //     Expressions::from_tokens(Tokens::Add, left_side, right_side)
        // }
        Some(Tokens::Subtract) => {
            // substract and add have the same binding power anyways
            let right_power = prefix_binding_power(&Tokens::Subtract);

            Expressions::Negative(Box::new(parser(tokens, right_power)))
            // Expressions::from_tokens(Tokens::Subtract, None, Some(parser(tokens, right_power)))
        }
        Some(Tokens::ParenthesesOpen) => parser(tokens, 0),

        // keywords
        Some(Tokens::Global) => {
            // no clue if assign add cna work on a value that has no value
            // if let Tokens::Assign = tokens.pop().unwrap() {}
            let name = if let Some(Tokens::Variable(name)) = tokens.pop() {
                name
            } else {
                // TODO: better error handling, maybe specifiying which lines and all of that
                panic!("Global variable has no name!")
            };

            let assignment = if let Some(Tokens::Assign) = tokens.last() {
                tokens.pop().unwrap();
                Some(Box::new(parser(tokens, 0)))
            } else {
                None
            };

            Expressions::GlobalDefine(
                Box::new(Expressions::Variable(name.to_string())),
                assignment,
            )
        }
        Some(Tokens::Local) => {
            // no clue if assign add cna work on a value that has no value
            // if let Tokens::Assign = tokens.pop().unwrap() {}
            let name = if let Some(Tokens::Variable(name)) = tokens.pop() {
                name
            } else {
                // TODO: better error handling, maybe specifiying which lines and all of that
                panic!("Local variable has no name!")
            };

            let assignment = if let Some(Tokens::Assign) = tokens.last() {
                tokens.pop().unwrap();
                Some(Box::new(parser(tokens, 0)))
            } else {
                None
            };

            Expressions::LocalDefine(
                Box::new(Expressions::Variable(name.to_string())),
                assignment,
            )
        }
        Some(Tokens::ScopeOpen) => {
            let mut expressions = Vec::new();
            loop {
                let output = parser(tokens, 0);
                println!("parser outputu: {:?}", output);
                if let Expressions::ScopeEnd = output {
                    break;
                }
                expressions.push(output);
            }

            Expressions::Scope(expressions)
        }
        Some(Tokens::ScopeClose) => Expressions::ScopeEnd,
        Some(Tokens::ParenthesesClose) => Expressions::ParanthesesEnd,
        Some(Tokens::If) => {
            let value = parser(tokens, 0);
            println!("the value for if is {:?}", value);

            let scope = parser(tokens, 0);
            println!("Scope is: {:?}", scope);

            Expressions::If(Box::new(value), Box::new(scope))
        }
        Some(Tokens::FunctionDefinition) => {
            let name = if let Some(Tokens::Variable(name)) = tokens.pop() {
                name
            } else {
                // NOTE: make it so that there can be anonymous functions
                panic!("Function has no name!")
            };


            let mut arguments = Vec::new();
            // TODO: error checking here!, this next instruction ahs to be prantheses open
            tokens.pop();
            // let expression = parser(tokens, 0);
            loop {
                let expression = parser(tokens, 0);

                if let Expressions::ParanthesesEnd = expression {
                    break;
                }

                arguments.push(expression);
            }

            let scope = parser(tokens, 0);

            Expressions::FunctionBlock(Box::new(Expressions::Variable(name)), arguments, Box::new(scope))
        }
        Some(Tokens::String(value)) => {
            // tokens.pop().unwrap();
            // let value = parser(tokens, 0);
            // let value = match tokens.pop() {
            //     Some(Tokens::String(val)) => val.to_string(),
            //     Some((e)) => panic!("unknown val: {:?}", e),
            //     None => panic!("waht"),
            // };
            // if let Some(Tokens::String(val)) = tokens.pop() {
            //     val.to_string()
            // } else {
            //     panic!("String has no value?")
            // };
            // tokens.pop().unwrap();

            Expressions::String(value)
        }
        None => Expressions::EndOfFile,
        // Some(Tokens::ParenthesesClose) => Expressions::EndOfFile,
        e => panic!("not handled: {:?}", e),
    };

    // println!("as i see it tokens is");
    // for token in tokens {
    //     println!("      {:?}", token);
    // }
    while let Some(operator) = tokens.last().cloned() {
        // let operator = match tokens.last().copied() {
        //     Some(output) => output,
        //     None => break,
        // };

        if !can_continue(operator.clone()) {
            break;
        }
        // checks if the statement can continue

        // gets the binding power

        if let Some(left_power) = postfix_binding_power(&operator) {
            if left_power < min_power {
                break;
            }
            tokens.pop().unwrap();

            // let right_side = parser(tokens, right_power);

            left_hand_side = if operator == Tokens::SquaredBracketOpen {
                let right_side = parser(tokens, 0);

                tokens.pop().unwrap();

                Expressions::from_tokens(operator, Some(left_hand_side), Some(right_side), None)
            } else if operator == Tokens::ParenthesesOpen {
                let mut right_side = Vec::new();

                // should check if there is a variable
                let token = parser(tokens, 0);
                if let Expressions::ParanthesesEnd = token  {
                    return Expressions::from_tokens(operator, Some(left_hand_side), None, Some(right_side));
                }
                right_side.push(token);


                while let Some(Tokens::Comma) = tokens.pop() {
                    let token = parser(tokens, 0);
                    if let Expressions::ParanthesesEnd = token  {
                        return Expressions::from_tokens(operator, Some(left_hand_side), None, Some(right_side) );
                    }
                    right_side.push(token);
                }
                println!("left hand side is {:?} and right side is {:?}", left_hand_side, right_side );

                // TODO: make it so that once it recognizes a , it will continue and add more
                Expressions::from_tokens(operator, Some(left_hand_side), None, Some(right_side))
            } else {
                Expressions::from_tokens(operator, Some(left_hand_side), None, None)
            };
            // left_hand_side = Expressions::from_tokens(operator, Some(left_hand_side), None);
            continue;
        }

        if let Some((left_power, right_power)) = infix_binding_power(&operator) {
            // println!(
            //     "binding power is smaller? than right power of token: {:?}, binding power: {:?}, min power; {:?}",
            //     operator,
            //     (left_power, right_power),
            //     min_power
            // );
            if left_power < min_power {
                break;
            }
            tokens.pop().unwrap();

            let right_side = parser(tokens, right_power);

            left_hand_side = Expressions::from_tokens(
                operator.clone(),
                Some(left_hand_side),
                Some(right_side),
                None,
            );
            continue;
        }

        break;
    }

    // println!("left hand side is: {:?}", left_hand_side);
    left_hand_side
}

fn prefix_binding_power(token: &Tokens) -> u8 {
    match token {
        Tokens::Subtract => 5,
        _ => panic!("wow"),
    }
}

pub fn infix_binding_power(token: &Tokens) -> Option<(u8, u8)> {
    match token {
        Tokens::Add | Tokens::Subtract => Some((1, 2)),
        Tokens::Multiply | Tokens::Divide | Tokens::Remainder => Some((3, 4)),
        Tokens::CompareEqual
        | Tokens::CompareNotEqual
        | Tokens::CompareLessThan
        | Tokens::CompareMoreThan
        | Tokens::CompareLessEquals
        | Tokens::CompareMoreEquals => Some((5, 6)),
        Tokens::Access | Tokens::AccessImpl => Some((7, 6)),
        Tokens::Assign => Some((10, 1)),
        Tokens::ConcatenateString => Some((8, 9)),
        _ => None,
    }
}

pub fn postfix_binding_power(token: &Tokens) -> Option<u8> {
    match token {
        Tokens::SquaredBracketOpen => Some(7),
        Tokens::ParenthesesOpen => Some(1),
        _ => None,
    }
}

pub fn generate(input: String) -> Vec<Expressions> {
    let mut lexer = Tokens::lexer(&input);

    let mut tokens = Vec::new();

    for (token, span) in lexer.spanned() {
        println!("token is {:?}", token);
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                panic!("lexer error {:?}", e);
            }
        }
    }
    tokens.reverse();

    let mut output_vec = Vec::new();
    loop {
        let output = parser(&mut tokens, 0);
        println!("parser outputu: {:?}", output);

        if let Expressions::EndOfFile = output {
            break;
        } else {
            output_vec.push(output);
        }
    }

    output_vec
    // println!("parser output: {:?}", parser(&mut tokens, 0));
}
