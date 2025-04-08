use std::fs;
use std::path::Path;

use superclean::lexer::lex;
use superclean::parser::parse;

fn main() {
    // Load input from test.txt
    let path = Path::new("test.txt");
    let input = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading test.txt: {}", e);
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
    match parse(&tokens) {
        Ok((rest, ast)) => {
            println!("AST:\n{:#?}", ast);
            if !rest.is_empty() {
                println!("\nWarning: There are leftover tokens:\n{:#?}", rest);
            }
        }
        Err(e) => {
            eprintln!("\nParser Error:");
            eprintln!("{:#?}", e);
        }
    }
}
