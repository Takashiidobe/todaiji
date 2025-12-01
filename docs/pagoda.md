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

## Step 8: Return
Add early returns from a statement list.

- Grammar: `Stmt -> 'return' Expr | 'let' Ident '=' Expr | Expr`.
- Lowering: evaluate the return expression into `%r0`, then `jmp ret_exit`. A single `ret_exit:` label is emitted before the final exit sequence (`push.w %r0; pop.w %r1; movi %r0, $60; trap`). Code after a `return` may still be emitted but is unreachable.
- Example:
  ```
  1;
  return 2;
  3
  ```
  Lowers to:
  ```asm
  main:
    load.w %r0, $1  # span 0..1 "1"
    load.w %r0, $2  # span 10..11 "2"
    jmp ret_exit
    load.w %r0, $3  # span 13..14 "3"
  ret_exit:
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```

## Step 9: Blocks
Statements must now be written as blocks: `{ stmt1; stmt2; ... }`. Blocks can be nested and evaluate to the value of their last inner statement. Top-level programs are one or more blocks separated by semicolons.

- Grammar: `Program -> Block (';' Block)* ';'?`; `Block -> '{' Stmt (';' Stmt)* '}'`; `Stmt -> 'return' Expr | 'let' Ident '=' Expr | Expr | Block | /* empty */`.
- Scoping: `let` bindings are scoped to their block; nested blocks can shadow outer names. Leaving a block pops its locals off the stack while preserving `%r0`.
- Null statements: consecutive semicolons inside a block are allowed and simply skipped (`{ ;; return 5; }`). Semicolons between statements are optional when the next token clearly starts another statement.

## Step 10: If Statements
Add conditional execution with `if (expr) { block }`. The condition can be an `int` or `bool` (non-zero is true). No `else` yet.

- Grammar: `Stmt -> 'if' '(' Expr ')' Block ('else' (Block | If))* | ...` (other statement forms unchanged). This covers `if/else if/else`.
- Lowering: evaluate the condition into `%r0`, branch if zero to an `else` label (or end label if no else) using `brz.w %r0, else_label`, emit the `then` block; if there is an `else`, emit `jmp end_label`, then the `else` branch (which may itself be another `if`), then `end_label:`. Branches do not change the stack layout; blocks still pop their locals when exiting.
- Example:
  ```
  { let x = 1;
    if (x == 1) { return 5; };
    x }
  ```
  Lowers to:
  ```asm
  main:
    load.w %r0, $1
    push.w %r0          # x
    load.w %r0, $1
    push.w %r0
    load.w %r0, 8(%sp)
    pop.w %r1
    cmpeq.w %r0, %r1    # x == 1
    brz.w %r0, label_0  # skip if false
    load.w %r0, $5
    jmp ret_exit
  label_0:
    load.w %r0, 0(%sp)  # x
    pop.w %r15          # pop x
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```
- Example:
  ```
  { let x = 1+2; { let x = x+1; x }; x }
  ```
  Lowers (spans elided for brevity) to:
  ```asm
  main:
    load.w %r0, $2
    push.w %r0
    load.w %r0, $1
    pop.w %r1
    add.w %r0, %r1      # x = 3
    push.w %r0          # bind outer x
    load.w %r0, $1
    push.w %r0
    load.w %r0, 8(%sp)  # outer x
    pop.w %r1
    add.w %r0, %r1      # inner x = 4
    push.w %r0          # bind inner x
    load.w %r0, 0(%sp)  # use inner x
    pop.w %r15          # pop inner x
    pop.w %r15          # pop outer x (restored env)
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```
  The result is the last statement of the outer block (outer `x`, value 3).

## Step 11: For Loops
Add C-style `for` loops with initializer, condition, and post expressions.

- Grammar: `Stmt -> 'for' '(' ( 'let' Ident '=' Expr | Expr )? ';' Expr? ';' Expr? ')' Block | ...` (other statement forms unchanged). The initializer runs once before the loop, the condition runs at the top of each iteration (`0`/false exits), the post expression runs after each body execution. A missing condition means an infinite loop. The loop body is a block.
- Types: condition must be `int`/`bool`. Other expressions follow the existing arithmetic rules. Variables declared in the initializer are scoped to the loop.
- Lowering: emit the initializer, then a loop label. Evaluate the condition (if present) into `%r0`; `brz.w %r0, end_label` exits. Emit the body block (which manages its own locals), then the post expression, `jmp loop_label`, and finish with `end_label:`. After the loop, locals introduced in the initializer are popped so outer scopes are restored.
- Example:
  ```
  { for (let i = 0; i < 1; i = i + 1) { 2; }; 3 }
  ```
  Lowers (spans elided) to:
  ```asm
  main:
    load.w %r0, $0
    push.w %r0          # i
  label_0:
    load.w %r0, $1
    push.w %r0
    load.w %r0, 8(%sp)  # i
    pop.w %r1
    cmplt.w %r0, %r1
    brz.w %r0, label_1  # exit when condition is false
    load.w %r0, $2      # body
    load.w %r0, $1      # post expression (i+1)
    push.w %r0
    load.w %r0, 8(%sp)  # i
    pop.w %r1
    add.w %r0, %r1
    store.w %r0, 8(%sp) # write back i
    jmp label_0
  label_1:
    pop.w %r15          # pop i
    load.w %r0, $3      # trailing statement result
    push.w %r0
    pop.w %r1
    movi %r0, $60
    trap
  ```

## Step 12: Assignment
Variables can be reassigned with `name = expr` as an expression (returns the new value).

- Grammar: extend `Expr` with assignment of the form `Ident '=' Expr`, lowest precedence and right-associative (`a = b = 1` is allowed). Compound forms `+=`, `-=`, `*=`, `/=` are also accepted.
- Types: the variable must already exist in scope; the right-hand side must match the variable’s type (`int` or `bool`).
- Lowering: evaluate the right-hand side into `%r0`, then `store.w %r0, <disp>(%sp)` to overwrite the saved slot for `name`. The assignment leaves `%r0` holding the new value. Compound assignments load the current slot, apply the arithmetic op with the RHS, then write back with `store.w`.
- Example (using a loop post expression):
  ```
  { for (let i = 0; i < 3; i = i + 1) {  }; i }
  ```
  This increments `i` three times and the program exits with `3`.

## Step 13: Arithmetic Assignment
Add syntactic sugar for in-place arithmetic: `+=`, `-=`, `*=`, `/=`.

- Grammar: extend assignment to accept the compound operators (`Ident ('+=' | '-=' | '*=' | '/=') Expr`).
- Types: same as simple assignment — target must exist and RHS type must match.
- Lowering: load the current variable slot, evaluate the RHS, apply the corresponding ALU op (`add.w/sub.w/mul.w/divmod.w`), then `store.w` back to the slot. The result remains in `%r0`.
- Example:
  ```
  { let a = 1; let b = 2; a += b; a }
  ```
  Exits with `3`.

## Step 14: Bitwise Ops
Introduce bitwise operations and their compound assignments: `& | ^ ~` plus `&= |= ^=`.

- Grammar/precedence: unary `~` sits with other prefixes; binary `&` binds tighter than `^`, which binds tighter than `|`; all of these are higher precedence than comparisons and lower than `*`/`/`. Compound forms reuse assignment precedence.
- Types: operands must be `int`; results are `int`.
- Lowering:
  - Unary `~expr` -> evaluate into `%r0`, emit `not.w %r0`.
  - Binary ops evaluate RHS then LHS (existing pattern), then emit `and.w/or.w/xor.w %r0, %r1`.
  - Compound assignments load the current slot, combine with RHS using the matching op, and write back with `store.w`.
- Example:
  ```
  { let a = 1; a &= 3; ~a | (a ^ 4) }
  ```

## Step 15: Shifts
Add shift operators and their compound assignments. For now, `>>` uses arithmetic right shift (`sar`) since values are signed. Logical left uses `shl`.

- Grammar/precedence: binary `<<`/`>>` sit between multiplicative and bitwise-AND (tighter than `&`). Compound `<<=`/`>>=` follow assignment precedence.
- Types: operands must be `int`; results are `int`.
- Lowering:
  - `a << b` -> evaluate `b` then `a`, `shl.w %r0, %r1`.
  - `a >> b` -> evaluate `b` then `a`, `sar.w %r0, %r1` (arithmetic right shift for now).
  - Compound forms load the slot, apply `shl.w`/`sar.w` with the RHS, then `store.w` back.
- Example:
  ```
  { let a = 1; a <<= 2; a >> 1 }
  ```

## Step 16: Functions (up to 255 args)
Add functions and calls (0–255 integer arguments).

- Syntax: `fn name(a,b,...) { <block> }` with up to 255 comma-separated parameters. Top-level can mix function definitions and blocks; functions must be defined at the top level. Call with `name(expr, ...)`.
- Types: params and return are `int` (via `return expr;` or the last statement of the body). Arity must match.
- Scoping: functions have their own local scope; params are locals. No captures/closures yet.
- Calling convention: args 1–8 go in `%r0..%r7`, return in `%r0`. Caller-saved: `%r0..%r7`; callee-saved: `%r8..%r14`; `%r15` is `%sp`, `%r14` is `%pc`, `%r13` is `%fp`, so avoid using them as scratch. We use `%r7` as the discard/scratch register in lowering. Callee saves/restores any non-volatile it touches; `%sp` is the stack pointer. Params are pushed to the stack on entry to create variable slots. If there are more than 8 args, args 9+ are passed on the caller’s stack (left-to-right, with the last extra arg at the top); the caller cleans them up after the call. The callee copies stack args into its own locals on entry.
- Lowering: each function becomes a label (`fn_<name>`). Calls emit `call fn_<name>`. Returns pop locals then jump to a function-specific epilogue that emits `ret`. Main starts after a jump that skips function bodies.
- Example:
  ```
  fn add(a,b) { a + b };
  { add(2,3) }
  ```

## Step 17: Arrays
Adds arrays + array indexing
- Syntax: `[1,..n]`
- Access: `$array[$index]`
- Setting: `$array[$index] = $val`
- Example:
  ```
  { let x = [0]; x[0] = 1; return x[0]; } 
  ```
