use std::fs;
use std::path::Path;

use superclean::evaluator::{Env, eval_stmt};
use superclean::lexer::lex;
use superclean::parser::parse;
use superclean::type_checker::type_check_program;

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
    let type_check_env = match type_check_program(&stmts) {
        Ok(env) => {
            println!("Type Checking Successful!");
            println!("Environment:\n{:#?}", env);
            env
        }
        Err(e) => {
            eprintln!("\nType Checking Error:");
            eprintln!("{:?}", e);
            return;
        }
    };

    // Step 4: Evaluation
    println!("\n== Evaluation ==");
    let mut env = Env::new();
    for stmt in stmts {
        match eval_stmt(&stmt, &mut env) {
            Ok(Some(value)) => println!("Result: {:?}", value),
            Ok(None) => {} // No result to print
            Err(e) => {
                eprintln!("\nEvaluation Error:");
                eprintln!("{:?}", e);
                return;
            }
        }
    }
}
