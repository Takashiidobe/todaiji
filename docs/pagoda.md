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
- Lowering sketch (stack helpers): evaluate left into `%r0`, `push %r0`; evaluate right into `%r1`; `pop %r0`; emit op:
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
