# Superclean Language Specification

---

### 2.1 Syntax (250 pts)

#### 2.1.1 Language Tokens (50 pts)

Below is a summary of all token types in Superclean:

| Token          | Example                         | Description                         |
| -------------- | ------------------------------- | ----------------------------------- |
| Identifier     | `foo123`                        | Names of variables or functions     |
| Integer        | `42`                            | Integer literals                    |
| Float          | `3.14`                          | Floating-point literals             |
| StringLiteral  | `"hello\nworld"`                | String literals with escape support |
| Boolean        | `true`, `false`                 | Boolean literals                    |
| Unit           | `()`                            | Unit value                          |
| Tuple          | `(1, 2)`                        | Tuple values                        |
| Let            | `let`                           | Variable binding                    |
| Fun            | `fun`                           | Function definition                 |
| Return         | `return`                        | Return statement                    |
| If / Else      | `if`, `else`                    | Conditional expressions             |
| While          | `while`                         | Looping construct                   |
| Print          | `print`                         | Print statement                     |
| Types          | `Int`, `Bool`, `String`, `Unit` | Type annotations                    |
| Plus / Minus   | `+`, `-`                        | Arithmetic operators                |
| Star / Slash   | `*`, `/`                        | Multiplication / division           |
| Modulo         | `%`                             | Remainder operator                  |
| Equal          | `=`                             | Assignment or comparison            |
| EqualEqual     | `==`                            | Equality test                       |
| NotEqual       | `!=`                            | Inequality test                     |
| Less / Greater | `<`, `>`                        | Comparison operators                |
| LessEqual      | `<=`                            | Less than or equal                  |
| GreaterEqual   | `>=`                            | Greater than or equal               |
| Logical AND    | `&&`                            | Logical AND operator                |
| Logical OR     | `                               |                                     |
| Not            | `!`                             | Logical NOT operator                |
| Dot            | `.`                             | Member or tuple access              |
| Delimiters     | `(`, `)`, `{`, `}`, `;`, `:`    | Grouping, code blocks, etc.         |
| Comments       | `//...`, `/*...*/`              | Line and block comments             |

---

#### 2.1.2 Syntax in BNF Format (100 pts)

```bnf
<program>       ::= { <statement> }

<statement>     ::= "let" <identifier> ":" <type> "=" <expression> ";"
                 | "return" <expression> ";"
                 | "print" "(" <expression> ")" ";"
                 | <expression> ";"
                 | <function_decl>
                 | "while" <expression> <block>
                 | "break" ";"

<function_decl> ::= "fun" <identifier> "(" [<param_list>] ")" ":" <type> <block>

<param_list>    ::= <identifier> ":" <type> { "," <identifier> ":" <type> }

<block>         ::= "{" { <statement> } "}"

<expression>    ::= <literal>
                 | <identifier>
                 | <expression> <binop> <expression>
                 | <function_call>
                 | <tuple>
                 | "(" <expression> ")"
                 | "if" <expression> <block> "else" <block>
                 | <expression> "." <identifier>
                 | <expression> "." <integer>

<function_call> ::= <identifier> "(" [<argument_list>] ")"

<argument_list> ::= <expression> { "," <expression> }

<tuple>         ::= "(" <expression> "," <expression> { "," <expression> } ")"

<binop>         ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

<literal>       ::= <int> | <float> | <string> | "true" | "false" | "()"

<type>          ::= "Int" | "Float" | "Bool" | "String" | "Unit" | <tuple_type>

tuple_type      ::= "(" <type> "," <type> { "," <type> } ")"

<identifier>    ::= [a-zA-Z_][a-zA-Z0-9_]*
```

---

#### 2.1.3 Abstract Syntax & Formation Rules (100 pts)

```rust
enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
    Tuple(Vec<Expr>),
    Var(String),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    Call(String, Vec<Expr>),
    IfElse(Box<Expr>, Block, Block),
    While(Box<Expr>, Block),
    MemberAccess(Box<Expr>, String),
    TupleAccess(Box<Expr>, usize),
}

enum Stmt {
    Let(String, Type, Expr),
    Return(Expr),
    Print(Expr),
    ExprStmt(Expr),
    Fun(String, Vec<(String, Type)>, Type, Block),
    While(Box<Expr>, Block),
    Break,
}

enum Type {
    Int,
    Float,
    Bool,
    String,
    Unit,
    Tuple(Vec<Type>),
}
```

---

### 2.2 Semantics (350 pts)

#### 2.2.1 Statics and Typing Spelled Out (100 pts)

- Every expression has a type.
- You cannot add an `Int` and a `Bool`.
- You cannot access a non-existent tuple element.
- Function calls must match the number and types of parameters.
- The `Unit` type is returned from functions that do not return values.
- Type annotations are required for all variable and function definitions.

#### 2.2.2 Typing Expressed Using Typing Rules (50 pts)

```
Γ ⊢ 5 : Int
Γ ⊢ 3.14 : Float
Γ ⊢ true : Bool
Γ ⊢ "hi" : String
Γ ⊢ () : Unit
Γ ⊢ (1, true) : (Int, Bool)

Γ(x) = T
-------------
Γ ⊢ x : T

Γ ⊢ e1 : Int    Γ ⊢ e2 : Int
----------------------------
Γ ⊢ e1 + e2 : Int
```

#### 2.2.3 Dynamics and Evaluation Spelled Out (100 pts)

- Arithmetic operations compute numeric results.
- Boolean expressions evaluate to `true` or `false`.
- `if` evaluates the condition, then executes the corresponding branch.
- `while` evaluates its condition and executes the block repeatedly.
- Function calls evaluate arguments and substitute them into the body.
- Tuple expressions evaluate their elements.

#### 2.2.4 Dynamics Expressed Using Evaluation Rules (50 pts)

```
e1 → e1'
-------------------
e1 + e2 → e1' + e2

-------------------
5 + 3 → 8

-------------------
while false { ... } → ()

-------------------
if true { e1 } else { e2 } → e1
```

#### 2.2.5 Type Safety and Computational Expressiveness (50 pts)

- Superclean guarantees type safety: values never change types during evaluation.
- All operations are type-checked at compile-time.
- The language supports general-purpose programming: functions, control flow, data structures.

---

### **TODOs for Future Iterations**

- Add support for lists (`[1, 2, 3]`) and list operations.
- Add `for` loops for iterating over collections.
