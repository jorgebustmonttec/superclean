use std::fs;
use std::path::Path;

use superclean::lexer::lex;
use superclean::parser::parse;
use superclean::type_checker::{TypeEnv, type_check_program};

fn main() {
    // Load input from test.sclean
    let path = Path::new("test.sclean");
    let input = match fs::read_to_string(path) {
        Ok(content) => content.trim_end().to_string(),
        Err(e) => {
            eprintln!("Error reading test.sclean: {}", e);
            return;
        }
    };

    println!("== Raw Input ==");
    println!("{input}");

    // Step 1: Lexing
    println!("\n== Lexing ==");
    let tokens = match lex(&input) {
        Ok(toks) => {
            println!("{:#?}", toks);
            toks
        }
        Err(e) => {
            eprintln!("\nLexer Error:");
            eprintln!("{:?}", e);
            return;
        }
    };

    // Step 2: Parsing
    println!("\n== Parsing ==");
    let stmts = match parse(&tokens) {
        Ok(ast) => {
            println!("AST:\n{:#?}", ast);
            ast
        }
        Err(e) => {
            eprintln!("\nParser Error:");
            eprintln!("{:#?}", e);
            return;
        }
    };

    // Step 3: Type Checking
    println!("\n== Type Checking ==");
    match type_check_program(&stmts) {
        Ok(env) => {
            println!("Type Checking Successful!");
            println!("Environment:\n{:#?}", env);
        }
        Err(e) => {
            eprintln!("\nType Checking Error:");
            eprintln!("{:?}", e);
        }
    }
}
