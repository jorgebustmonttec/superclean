# Superclean Language Specification

# 2. Language Specification (600 pts)

---

### 2.1 Syntax (250 pts)

#### 2.1.1 Language Tokens (50 pts)

Below is a summary of all token types in Superclean:

| Token          | Example                      | Description                         |
| -------------- | ---------------------------- | ----------------------------------- |
| Identifier     | `foo123`                     | Names of variables or functions     |
| Integer        | `42`                         | Integer literals                    |
| StringLiteral  | `"hello\nworld"`             | String literals with escape support |
| Let            | `let`                        | Variable binding                    |
| Fun            | `fun`                        | Function definition                 |
| Return         | `return`                     | Return statement                    |
| If / Else      | `if`, `else`                 | Conditional expressions             |
| While          | `while`                      | Looping construct                   |
| Print          | `print`                      | Print statement                     |
| Types          | `Int`, `Bool`                | Type annotations                    |
| Boolean        | `true`, `false`              | Boolean literals                    |
| Plus / Minus   | `+`, `-`                     | Arithmetic operators                |
| Star / Slash   | `*`, `/`                     | Multiplication / division           |
| Equal          | `=`                          | Assignment or comparison            |
| EqualEqual     | `==`                         | Equality test                       |
| NotEqual       | `!=`                         | Inequality test                     |
| Less / Greater | `<`, `>`                     | Comparison operators                |
| LessEqual      | `<=`                         | Less than or equal                  |
| GreaterEqual   | `>=`                         | Greater than or equal               |
| Delimiters     | `(`, `)`, `{`, `}`, `;`, `:` | Grouping, code blocks, etc.         |
| Comments       | `//...`, `/*...*/`           | Line and block comments             |

---

#### 2.1.2 Syntax in BNF Format (100 pts)

> **TODO**: Write the BNF grammar for Superclean. Here's a starting point:

```bnf
<program>       ::= { <statement> }

<statement>     ::= "let" <identifier> ":" <type> "=" <expression> ";"
                 | "return" <expression> ";"
                 | "print" "(" <expression> ")" ";"
                 | <expression> ";"
                 | <function_decl>

<function_decl> ::= "fun" <identifier> "(" [<param_list>] ")" ":" <type> <block>

<param_list>    ::= <identifier> ":" <type> { "," <identifier> ":" <type> }

<block>         ::= "{" { <statement> } "}"

<expression>    ::= <literal>
                 | <identifier>
                 | <expression> <binop> <expression>
                 | <function_call>
                 | "(" <expression> ")"
                 | "if" <expression> <block> "else" <block>
                 | "while" <expression> <block>

<function_call> ::= <identifier> "(" [<argument_list>] ")"

<argument_list> ::= <expression> { "," <expression> }

<binop>         ::= "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">="

<literal>       ::= <int> | <string> | "true" | "false"

<type>          ::= "Int" | "Bool" | "String" | "Unit"

<identifier>    ::= [a-zA-Z_][a-zA-Z0-9_]*
```

---

#### 2.1.3 Abstract Syntax & Formation Rules (100 pts)

> **TODO**: Write how AST nodes are structured. Example:

```rust
enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Var(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(String, Vec<Expr>),
    IfElse(Box<Expr>, Block, Block),
    While(Box<Expr>, Block),
}

enum Stmt {
    Let(String, Type, Expr),
    Return(Expr),
    Print(Expr),
    ExprStmt(Expr),
    Fun(String, Vec<(String, Type)>, Type, Block),
}

enum Type {
    Int,
    Bool,
    String,
    Unit,
}
```

---

### 2.2 Semantics (350 pts)

#### 2.2.1 Statics and Typing (100 pts)

> **TODO**: Describe type rules in plain English. Example:

- Every expression has a type.
- You cannot add an `Int` and a `Bool`.
- Function calls must match the number and types of parameters.
- The `Unit` type is returned from functions that do not return values.

#### 2.2.2 Typing Rules (50 pts)

> **TODO**: Use inference-style notation. Example:

```
Γ ⊢ 5 : Int
Γ ⊢ true : Bool
Γ ⊢ e1 : Int    Γ ⊢ e2 : Int
-------------------------------
        Γ ⊢ e1 + e2 : Int

Γ(x) = T
-------------
Γ ⊢ x : T
```

#### 2.2.3 Evaluation Semantics (100 pts)

> **TODO**: Describe how evaluation works. Example:

- `if` statements evaluate the condition first. Then either the true or false branch is executed.
- Function calls evaluate arguments first, then apply the function body.
- Expressions inside parentheses are evaluated first.

#### 2.2.4 Evaluation Rules (50 pts)

> **TODO**: Add evaluation rules like:

```
e1 → e1'
-------------------
e1 + e2 → e1' + e2

-------------------
5 + 3 → 8
```

#### 2.2.5 Type Safety and Expressiveness (50 pts)

> **TODO**: Short section stating:

- Superclean guarantees type safety: expressions do not change type during evaluation.
- It supports general-purpose programming through variables, functions, control flow, and recursion.

---

## 3. Standard Library Reference (100 pts)

> **TODO**: Even if you don’t implement it, write a reference for functions you’d _want_ to include.

Example:

### `print(x: String): Unit`

Prints a string to the console.

```superclean
print("Hello, world!");
```

---
