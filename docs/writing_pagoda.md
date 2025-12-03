# Writing Pagoda: A User Guide

Pagoda is a small, expression-oriented language that lowers to the Todaiji
assembly described in `docs/writing_todaiji.md`. This guide shows how to write
Pagoda code, from basics to structs, enums, and modules.

## Table of Contents

1. [Overview](#overview)
2. [Running Pagoda](#running-pagoda)
3. [Program Layout](#program-layout)
4. [Types and Literals](#types-and-literals)
5. [Expressions and Operators](#expressions-and-operators)
6. [Variables and Assignment](#variables-and-assignment)
7. [Control Flow](#control-flow)
8. [Functions](#functions)
9. [Data Structures](#data-structures)
10. [Pattern Matching](#pattern-matching)
11. [Modules and Visibility](#modules-and-visibility)
12. [Complete Examples](#complete-examples)
13. [Tips and Gotchas](#tips-and-gotchas)

## Overview

- Expression-first: blocks and loops evaluate to the value of their last
  expression.
- Strongly typed with simple inference: omit most type annotations and Pagoda
  defaults to `i64` unless a context or literal provides more detail.
- Designed to be compiler-friendly: all features lower directly to Todaiji
  assembly; `docs/todaiji_asm.md` plus this guide are enough to understand
  emitted code.

## Running Pagoda

Compile a `.pagoda` file to Todaiji assembly, then assemble/execute:

```sh
cargo run -- -p path/to/file.pagoda > file.asm   # lower to assembly
cargo run -- file.asm                            # assemble + run
```

Errors are span-aware with source snippets, so you can fix issues quickly.

## Program Layout

- A file can contain imports, struct/enum definitions, function definitions, and
  top-level blocks. Top-level blocks run after functions are defined.
- The entry point is `fn main() -> i64 { ... }`. It takes no parameters and its
  return value becomes the process exit code.
- Statements live inside blocks: `{ stmt1; stmt2; ... }`. A trailing semicolon
  is allowed.
- Whitespace is free-form; there is currently no comment syntax.

## Types and Literals

- Integers: `i64`/`int` (default), `i32`, `i16`, `i8`, and unsigned variants
  `u64`, `u32`, `u16`, `u8`.
- Booleans: `true`, `false`.
- Strings: `"hello"`.
- Arrays: `[1, 2, 3]` (elements must be integers; array length is part of the
  type).
- Tuples: `(1, 2, 3)`.
- Structs, tuple structs, and enums are user-defined (see below).
- Type names you do not recognize default to a struct type in type checking.

## Expressions and Operators

- Arithmetic: `+ - * / %` on integer types. Division/modulo follow the Todaiji
  backend policy (division by zero traps).
- Bitwise: `& | ^ ~` on integers; shifts `<< >>` (right shift is arithmetic).
- Comparisons: `== != < > <= >=` produce `bool`.
- Logical: `&& || !` accept `bool` or int-like values (non-zero is true).
- Precedence mirrors C-style languages: unary → multiplicative → additive →
  shifts → bitwise (`& ^ |`) → comparisons → logical `&&`/`||` → assignment.
- Field and index access: `expr.field`, `expr[index]`, `tuple_expr.0`.
- Calls: `fn_name(arg1, arg2)`, or qualified `module::fn_name(...)`.
- Evaluation is left-to-right; logical operators are eager (evaluate both
  sides).

## Variables and Assignment

- Bindings: `let name = expr;` with optional annotation `let x: i32 = 1;`.
- Reassignment: `name = expr` returns the new value.
- Compound assignment: `+= -= *= /= %= <<= >>= &= |= ^=` on integer variables.
- Names must be defined before use; shadowing is block-scoped.

## Control Flow

- Blocks: `{ ... }` can appear wherever an expression is expected; they evaluate
  to the last non-empty statement.
- If/else: `if (cond) { ... } else { ... }`. Conditions accept `bool` or
  int-like expressions.
- For loops: `for (init; cond; post) { body }`. Any part may be omitted; a
  missing condition means an infinite loop.
- Return: `return expr;` exits the current function early.

## Functions

- Declare with `fn name(params...) -> return_type { body }`.
- Parameter and return annotations are optional; omitted types default to `i64`.
- Up to 255 parameters; arguments and return values are integers/structs/enums
  lowered into registers/stack following the calling convention described in
  `docs/pagoda.md`.
- Recursion is allowed. All paths should return a value matching the signature.

## Data Structures

### Arrays

- Literal: `let xs = [1, 2, 3];` (integers only).
- Indexing: `xs[0]` reads; `xs[1] = 10;` writes. Indices must be integer.

### Tuples

- Literal: `let t = (10, 20, 30);`
- Access: `t.0`, `t.1`, etc. Nested tuples work the same.

### Structs

- Named fields:
  ```pagoda
  struct Rectangle { width: i64, height: i64 }
  let r = Rectangle { width: 3, height: 4 };
  let area = r.width * r.height;
  r.width = r.width + 1;
  ```
- Tuple structs:
  ```pagoda
  struct Point(i64, i64);
  let p = Point(10, 20);
  let sum = p.0 + p.1;
  ```
- Field names must be provided exactly once in literals; types must match the
  definition.

### Enums

- Define tagged unions with optional payloads:
  ```pagoda
  enum Result {
      Ok(i64),
      Err(string),
  }
  let r = Result::Ok(42);
  let e = Result::Err("fail");
  ```
- Enum construction uses `Enum::Variant(data?)`. Payload presence must match the
  definition.

### Strings and Booleans

- String literals produce the `string` type and can be stored in variables or
  struct fields.
- Booleans are first-class and interoperate with logical operators and `if`.

## Pattern Matching

- `match` requires exhaustive coverage of variants; missing a case is an error.
- Syntax:
  ```pagoda
  match r {
      Result::Ok(val) => val,
      Result::Err(msg) => 0,
  }
  ```
- You can omit the enum name inside the match when it is clear from context, but
  qualifying is supported: `module::Enum::Variant`.
- Bindings inside a pattern (`Variant(x)`) introduce a new variable scoped to
  the arm body.

## Modules and Visibility

- Import sibling modules by file name: `import math;` loads `math.pag` from the
  same directory (imports are resolved recursively).
- Public items are marked with `pub` on functions, structs, tuple structs, and
  enums. Non-`pub` items stay private to the module.
- Use qualified names to access imported items: `math::add(1, 2)`,
  `shapes::Rectangle { ... }`, `shapes::Shape::Circle(...)`.
- Circular imports are rejected with a descriptive error.

## Complete Examples

### Hello World (numeric)

```pagoda
fn main() -> i64 {
    return 42;
}
```

### Functions and Blocks

```pagoda
fn add(a, b) -> i64 { a + b }

fn main() -> i64 {
    { let x = add(2, 3); x * 4 }
}
```

### Structs, Arrays, and Loops

```pagoda
struct Accum { total: i64 }

fn main() -> i64 {
    let acc = Accum { total: 0 };
    let nums = [1, 2, 3, 4];
    for (let i = 0; i < 4; i += 1) {
        acc.total += nums[i];
    }
    return acc.total;
}
```

### Enums and Match

```pagoda
enum Option { Some(i64), None }

fn main() -> i64 {
    let v = Option::Some(10);
    match v {
        Option::Some(x) => x,
        Option::None => 0,
    }
}
```

## Tips and Gotchas

- Keep arithmetic operands the same integer width; mixing `i64` and `i32`
  triggers a type error.
- Arrays currently hold integers; other element types are not yet supported.
- Logical operators are eager; prefer `if`/`else` when guarding expensive work.
- `match` must cover every variant (including those from imported enums) or the
  checker will flag a non-exhaustive match.
- When designing modules, export with `pub` and use `module::item` to keep
  namespacing clear.
