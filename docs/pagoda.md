# Pagoda 

Pagoda is a minimal front end that lowers to Todaiji assembly. 

## CLI usage
Compile Pagoda source to assembly and print to stdout:
```sh
cargo r -- -p path/to/file.pagoda > file.asm
cargo r -- file.asm # assemble
```

## Error reporting
All stages (tokenize, parse, semantics, emit) surface span-aware errors with a caret snippet, e.g.:
```
1:4: trailing input Int(30) starting at bytes 3..5
1 | 42 30
  |    ^^
  | trailing input Int(30) starting at bytes 3..5
```

## Step 1: Numbers
The first language takes only 1 integer, and returns it as an exit code.

So this program:

```
99
```

Turns into this:

```asm
main:
  load.w %r0, $99 # span 0..2 "99"
  push.w %r0
  pop.w %r1
  movi %r0, $60
  trap
```


## Step 2: Arithmetic
Next milestone adds integer `+ - * /`.

- Grammar: `Expr -> Term (('+' | '-') Term)*`; `Term -> Factor (('*' | '/') Factor)*`; `Factor -> IntLiteral`.
- Evaluation order: left-to-right per grammar; result always lives in `%r0`.
- Type rule: operands must be `int`; result is `int`.
- Lowering sketch (stack helpers): evaluate left into `%r0`, `push.w %r0`; evaluate right into `%r1`; `pop.w %r0`; emit op:
  - `add.w %r0, %r1`
  - `sub.w %r0, %r1`
  - `mul.w %r0, %r1`
  - `divmod.w %r0, %r1` (quotient in `%r0`, remainder in `%r1`; remainder ignored for now).
- Runtime policy: decide and document division-by-zero behavior (trap vs. defined value) when wiring the backend.

## Step 3: Unary plus/minus
Add unary `+` and `-` with higher precedence than binary `+/-`.

- Grammar tweak: `Factor -> ('+' | '-')* IntLiteral` for now (can expand Atom later).
- Lowering: emit the literal into a register, then apply `neg.w` for each leading `-`. Leading `+` is a no-op.
  ```asm
  load.w %r1, $40  # literal
  neg.w %r1        # applies unary minus
  ```
- Precedence: unary binds tighter than multiplicative/additive binary ops.

## Step 4: Parentheses
Add grouping to override precedence without changing lowering or types.

- Grammar tweak: `Atom -> IntLiteral | '(' Expr ')'`; `Factor -> ('+' | '-')* Atom`.
- Parsing: when seeing `(`, parse an `Expr` then require `)`.
- Lowering/semantics: unchanged; spans wrap the whole parenthesized region.

## Step 5: Comparisons
Add relational/equality operators `== != < > <= >=` (left-to-right, lower precedence than `+/-`).

- Grammar: `Compare -> Sum (('==' | '!=' | '<' | '>' | '<=' | '>=') Sum)*`.
- Type: operands must be `int`; result is `bool` (0/1 in registers).
- Lowering:
  - `==`/`!=`: `cmpeq.w %r0, %r1` / `cmpne.w %r0, %r1`.
  - `<`: `cmplt.w %r0, %r1`.
  - `>`: evaluate as `cmplt.w %r1, %r0`, then move result from `%r1` to `%r0`.
  - `<=`: compute `cmplt.w %r1, %r0` then `not.w %r1`, move to `%r0`.
  - `>=`: compute `cmplt.w %r0, %r1` then `not.w %r0`.

## Step 6: Statements
Add multiple statements separated by semicolons. Each statement is still an expression; the program exits with the value of the last one. A trailing semicolon is allowed.

- Grammar: `Program -> Stmt (';' Stmt)* ';'?`; `Stmt -> Expr`.
- Lowering: emit each statement in order; each leaves its result in `%r0`, and the final exit code comes from the last statement.
- Example:
  ```
  1+2;3*4;5
  ```
  Lowers to (comments show spans for each expression):
  ```asm
  main:
    load.w %r0, $2  # span 2..3 "2"
    push.w %r0
    load.w %r0, $1  # span 0..1 "1"
    pop.w %r1
    add.w %r0, %r1  # span 0..3 "1+2"
    load.w %r0, $4  # span 6..7 "4"
    push.w %r0
    load.w %r0, $3  # span 4..5 "3"
    pop.w %r1
    mul.w %r0, %r1  # span 4..7 "3*4"
    load.w %r0, $5  # span 8..9 "5"
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```

## Step 7: Variables
Introduce immutable variables with `let name = expr;`. Bindings must be defined before use and cannot be redeclared. Each binding lives on the stack; references load from the saved slot.

- Grammar: `Stmt -> 'let' Ident '=' Expr | Expr`.
- Lowering:
  - Evaluate the initializer into `%r0`, then `push.w %r0` to allocate a slot (stack grows down).
  - Remember the slot depth; a later `name` becomes `load.w %r0, <disp>(%sp)` where `<disp>` is computed from the current stack depth so temporary pushes stay balanced.
  - Program result is still the value of the last statement (for a `let`, the initializer in `%r0`).
- Example:
  ```
  let x = 1+2;
  x*3
  ```
  Lowers to:
  ```asm
  main:
    load.w %r0, $2  # span 10..11 "2"
    push.w %r0
    load.w %r0, $1  # span 8..9 "1"
    pop.w %r1
    add.w %r0, %r1  # span 8..11 "1+2"
    push.w %r0      # saves x
    load.w %r0, $3  # span 15..16 "3"
    push.w %r0
    load.w %r0, 8(%sp)  # span 13..14 "x"
    pop.w %r1
    mul.w %r0, %r1  # span 13..16 "x*3"
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```
