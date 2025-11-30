# Pagoda 

Pagoda is a minimal front end that lowers to Todaiji assembly. 

The first language takes only 1 integer, and returns it as an exit code.

So this program:

```
99
```

Turns into this:

```asm
main:
  movi %r0, $60
  load.l %r1, $99 # span 0..2 "99"
  trap
```

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
