DISCLAIMER I MADE A LOT OF THE DOCUMENTATION LAST MINUTE WITH AI SO IT MAY BE OFF. NOTES.TXT HAS SOME IMPORTANT STUFF. DO SH RUN.SH TO RUN OFF TEST.SCLEAN AND SH.TEST.SH TO RUN UNIT TESTS.

# Superclean

## 🧼 Introduction (1.0)

Superclean is a minimal, statically typed, interpreted scripting language designed for clarity and ease of implementation.

It uses familiar syntax inspired by C, JavaScript, and Rust, and is implemented in Rust using the `nom` and `rustyline` crates.

This project was developed as part of the course **CS-C2170 – Modern and Emerging Programming Languages** at **Aalto University**.

## ✨ Features

- C-like syntax with braces and semicolons
- Static type checking
- Integers, booleans, and strings
- Variable bindings, control flow, and functions
- Interactive REPL and file execution support
- Written in Rust with [`nom`](https://docs.rs/nom) and [`rustyline`](https://docs.rs/rustyline)

## 🚀 Getting Started

### Requirements

- [Rust](https://www.rust-lang.org/tools/install)

### Build & Run

To start the REPL:

```bash
cargo run
```

To run a Superclean program from file:

```bash
cargo run -- path/to/file.sclean
```

## 📄 Project Structure

The project is organized into the following directories and files:

```
superclean/
├── src/
│   ├── main.rs          # Entry point for the interpreter
│   ├── ast.rs           # Abstract syntax tree definitions
│   ├── token.rs         # Token definitions for the lexer
│   ├── lexer/           # Lexer implementation
│   │   ├── mod.rs       # Lexer module
│   ├── parser/          # Parser implementation
│   │   ├── mod.rs       # Parser module
│   │   ├── expr.rs      # Expression parsing
│   │   ├── stmt.rs      # Statement parsing
│   ├── evaluator/       # Evaluator implementation
│   │   ├── mod.rs       # Evaluator module
│   │   ├── expr.rs      # Expression evaluation
│   │   ├── stmt.rs      # Statement evaluation
│   ├── type_checker.rs  # Type checker implementation
├── docs/                # Documentation
│   ├── spec.md          # Language specification
│   ├── start.md         # Tutorial and exercises
│   ├── std.md           # Standard library reference
│   ├── checklist.md     # Project checklist
│   ├── notes.txt        # Development notes
├── examples/            # Example Superclean programs
│   ├── showcase.sclean  # Comprehensive feature showcase
├── test.sclean          # Test program for manual testing
├── README.md            # Project README
```

## 📚 Documentation

Full language specification, including syntax and typing rules, can be found in [`docs/spec.md`](docs/spec.md).

## 💡 License

This project is developed for educational purposes and has no license.
