// use chumsky::{
//     Parser,
//     error::Simple,
//     prelude::{just, recursive},
//     primitive::end,
//     recursive, select,
// };
use chumsky::prelude::*;
use logos::{Lexer, Logos};

// use crate::bytecode::main::Opcode;

// instead of enum, use a struct perhaps
// with values known as numbers

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum Expressions {
    Bool(bool),
    Int(isize),

    Add(Box<Expressions>, Box<Expressions>),
    Subtract(Box<Expressions>, Box<Expressions>),
    Multiply(Box<Expressions>, Box<Expressions>),
    Divide(Box<Expressions>, Box<Expressions>),

    GlobalDefine((String, Option<OperatorTokens>)), // string due to the name of the global
    ConcatenateString((String, String)),            // Negative(Box<Expres)
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\n]+")]
#[logos(error = String)]
enum OperatorTokens {
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

    #[token(".")]
    Access,

    #[token(":")]
    FunctionAccess,

    #[token("=>")]
    FatArrow,

    #[token("->")]
    Arrow,

    #[token("..")]
    ConcatenateString,

    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap())]
    Integer(isize),

    // #[regex("[A-Za-z0-9]+")]
    // IMPORTANT: make the variable definition function work
    // #[regex(r"[[:alpha:]][[:alnum:]]*", variable_definition)]
    #[regex(r"(\w+)",priority = 1, callback = |lex| lex.slice().to_string() )]
    // #[regex(r"(\w+)")]
    Variable(String),

    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),
}

impl OperatorTokens {
    fn can_continue(&self) -> bool {
        match self {
            OperatorTokens::Variable(_) => false,
            OperatorTokens::Bool(_) => false,
            OperatorTokens::Integer(_) => false,
            OperatorTokens::Assign => false, // NOTE: maybe it could continue? no clue
            _ => true,
        }
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\n]+")]
enum StatementTokens {
    #[token("global", global_definition)]
    Global((String, Option<OperatorTokens>)),

    #[token("local")]
    Local,

    #[token("if")]
    If,

    #[token("loop")]
    Loop,

    #[token("break")]
    Break,

    // For((OperatorTokens, ))
    #[token("return")]
    Return,

    // seperate tokens becuase everything has to be in 1 due to parser
    //its not a big deal i think because wit hthese statements we can know when t oexpect what kind
    //of tokens
    Operator(OperatorTokens),
}

// fn get_operator_definition(lex: &mut Lexer<OperatorTokens>) -> OperatorTokens {}

fn global_definition(lex: &mut Lexer<StatementTokens>) -> Option<(String, Option<OperatorTokens>)> {
    let mut lex_2 = lex.clone().morph::<OperatorTokens>();
    // let test = lex_2.next();
    lex_2.next();
    let id = lex_2.slice().to_string();
    println!("i got: {:?}", lex_2.slice());

    // let test = lex_2.next();
    if let Some(Ok(OperatorTokens::Assign)) = lex_2.next() {
        println!("next was assign");
        let value = lex_2.next().unwrap().unwrap();
        // checks if the next value can continue statement
        let next_token = lex_2.next();
        if let Some(Ok(token)) = next_token {
            if token.can_continue() {
                println!("do some recursion here");
            }
        }
        println!("value is: {:?}", value);
        *lex = lex_2.morph();
        return Some((id, Some(value)));
    }
    println!("no assign");
    // println!("test: {:?}", test);

    // IMPORTANT: dont do unwrapping like this

    *lex = lex_2.morph();

    Some((id, None))
}

fn new_parser<'src>() -> impl Parser<
    'src,
    &'src [StatementTokens],
    Expressions,
    chumsky::extra::Err<chumsky::error::Simple<'src, StatementTokens>>,
> {
    recursive(|parsing| {
        // TODO: make it so that the abstract syntax tree will correctly parse operator tokens when
        // met with them
        let global = select! {
            StatementTokens::Global(tuple) => Expressions::GlobalDefine(tuple)
        };

        global
    })
}

// fn parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
//     // .then_ignore(End::end)
// }

// pub fn starter_statements(input: String) -> Vec<StatementTokens> {
//     let lexer = StatementTokens::lexer(&input);
//     let mut tokens = Vec::new();
//
//     for (token, span) in lexer.spanned() {
//         let token = token.unwrap();
//
//         match token {
//             StatementTokens::Global(_) => {
//                 // TODO: maybe make it so that it chhecks for an equal sign and then expects the
//                 // whole operator otken thing right now because currently you cannot have just `global bob` and
//                 // then later define it
//                 // though might be a feature?
//                 let mut lexer_2 = lexer.morph::<OperatorTokens>();
//             }
//             _ => {}
//         }
//     }
//
//     tokens
// }

pub fn generate(input: String) {
    let lexer = StatementTokens::lexer(&input);

    let mut tokens = Vec::new();

    // splits input into tokens
    for (token, span) in lexer.spanned() {
        println!("token is {:?}", token);
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                panic!("lexer error");
            }
        }
    }

    // parses tokens to produce an abstarct syntax tree
    let abstract_syntax_tree = match new_parser().parse(&tokens).into_result() {
        Ok(expression) => {
            println!("[AST]\n{:#?}", expression);
            expression
        }
        Err(e) => {
            panic!("parse error: {:#?}", e);
        }
    };

    // println!("\n[result]:\n {:?}", abstract_syntax_tree.eval());

    // let abstract_syntax_tree = match parser
}
