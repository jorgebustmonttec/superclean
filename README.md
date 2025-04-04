# Superclean

Superclean is a minimalist, statically typed scripting language designed for clarity, simplicity, and educational purposes.

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

- `src/` – language implementation
- `docs/` – language specification and documentation
- `examples/` – sample Superclean programs
- `tests/` – automated tests

## 📚 Documentation

Full language specification, including syntax and typing rules, can be found in [`docs/spec.md`](docs/spec.md).

## 💡 License

This project is developed for educational purposes and has no license.
