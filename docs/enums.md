# Enum Support in Pagoda

This document outlines the implementation of Rust-style enums (tagged unions) in the Pagoda language.

## Overview

Enums in Pagoda are algebraic data types that allow defining a type with multiple variants, where each variant can optionally carry associated data. This is similar to Rust's enum types or tagged unions in other languages.

### Example Syntax

```rust
// Enum definition
enum Option {
    Some(i64),
    None,
}

enum Result {
    Ok(i64),
    Err(string),
}

// Enum construction
let x = Option::None;
let y = Option::Some(42);

// Pattern matching (planned)
match x {
    Option::Some(val) => val,
    Option::None => 0,
}
```

## Implementation Status

### ✅ Completed

#### 1. Tokenizer (src/pagoda/tokenizer.rs)
- Added `Enum` keyword token
- Added `Match` keyword token
- Keywords are recognized during tokenization

#### 2. AST Nodes (src/pagoda/mod.rs)
- **EnumDef**: Represents enum definitions
  ```rust
  pub struct EnumDef {
      pub is_public: bool,
      pub name: String,
      pub variants: Vec<EnumVariant>,
      pub span: Span,
  }
  ```

- **EnumVariant**: Represents individual variants within an enum
  ```rust
  pub struct EnumVariant {
      pub name: String,
      pub data: Option<String>, // Optional associated data type
      pub span: Span,
  }
  ```

- **EnumLiteral**: Expression for constructing enum values
  ```rust
  Expr::EnumLiteral {
      enum_name: String,
      variant_name: String,
      data: Option<Box<Expr>>, // Optional data value
      span: Span,
  }
  ```

- **QualifiedEnumLiteral**: Module-qualified enum construction
  ```rust
  Expr::QualifiedEnumLiteral {
      module: String,
      enum_name: String,
      variant_name: String,
      data: Option<Box<Expr>>,
      span: Span,
  }
  ```

- **Match Expression**: Pattern matching (AST only, parsing not implemented)
  ```rust
  Expr::Match {
      expr: Box<Expr>,
      arms: Vec<MatchArm>,
      span: Span,
  }

  pub struct MatchArm {
      pub pattern: Pattern,
      pub body: Box<Expr>,
      pub span: Span,
  }

  pub enum Pattern {
      Variant {
          enum_name: Option<String>,
          variant_name: String,
          binding: Option<String>, // Variable to bind data to
          span: Span,
      },
  }
  ```

- Updated `Program` and `CheckedProgram` to include `enums` field

#### 3. Parser (src/pagoda/parser.rs)

##### Enum Definition Parsing
- Implemented `parse_enum()` method
- Parses enum definitions with multiple variants
- Supports variants with and without associated data types
- Handles public/private visibility
- Example: `pub enum Option { Some(i64), None }`

##### Enum Literal Parsing
- Parses local enum construction: `EnumName::Variant` or `EnumName::Variant(expr)`
- Parses qualified enum construction: `module::Enum::Variant(expr)`
- Integrated into expression parsing pipeline
- Distinguishes between:
  - Function calls: `module::function()`
  - Struct literals: `module::Struct { ... }`
  - Enum variants: `module::Enum::Variant(...)` or `Enum::Variant(...)`

##### Integration
- Updated `parse_program_impl()` to parse enum definitions alongside structs and functions
- Enum definitions can appear at module level
- Support for `pub enum` declarations

#### 4. Type System (src/pagoda/semantics.rs)
- Added `Type::Enum(String)` variant
- Updated `Type::as_str()` to display enum types
- Type Display implementation includes enums

#### 5. Stub Implementation
- Added stub implementations in bytecode generator (returns UnsupportedExpr)
- Added stub implementations in semantic analyzer (returns UnsupportedExpr)
- All code compiles successfully with enum support

#### 6. Tests
- `parses_enum_definition`: Basic enum with one variant
- `parses_enum_with_multiple_variants`: Enum with multiple variants
- `parses_enum_literal_without_data`: `Option::None`
- `parses_enum_literal_with_data`: `Option::Some(42)`
- All tests pass with snapshot testing

### 🚧 In Progress / Not Yet Implemented

#### 1. Match Expression Parsing
**Status**: AST nodes defined, parsing not implemented

**What needs to be done**:
- Implement `parse_match()` method in parser
- Parse `match expr { ... }` syntax
- Parse match arms with patterns
- Parse pattern bindings (e.g., `Some(x) => x`)
- Handle exhaustiveness checking (may be in semantics phase)

**Syntax to support**:
```rust
match value {
    Option::Some(x) => x,
    Option::None => 0,
}

match result {
    Result::Ok(val) => val,
    Result::Err(msg) => -1,
}
```

#### 2. Semantic Analysis
**Status**: Type infrastructure exists, checking not implemented

**What needs to be done**:
- Validate enum definitions:
  - Check for duplicate variant names
  - Validate associated data types exist
  - Track defined enums in symbol table

- Type check enum construction:
  - Verify enum exists
  - Verify variant exists in enum
  - Verify data type matches variant's associated type
  - Handle qualified vs unqualified enum references

- Type check match expressions:
  - Infer type from matched expression
  - Verify patterns match enum variants
  - Check pattern exhaustiveness (all variants covered)
  - Type check pattern bindings
  - Ensure all match arms return compatible types

- Module system integration:
  - Track public/private enums
  - Handle `import module` bringing enums into scope
  - Resolve qualified enum references

**Key semantic checks**:
```rust
// Should error: variant doesn't exist
let x = Option::Invalid(42);

// Should error: data type mismatch
let y = Option::Some("string");  // expects i64

// Should error: non-exhaustive match
match x {
    Option::Some(val) => val,
    // Missing Option::None case
}
```

#### 3. Bytecode Generation
**Status**: Not implemented (currently returns UnsupportedExpr error)

**What needs to be done**:
- Determine runtime representation:
  - Option 1: Tag + pointer (tag identifies variant, pointer to heap data)
  - Option 2: Inline small variants, heap allocate for larger ones
  - Option 3: Always heap allocate (tag + data on heap)

- Implement enum construction:
  - Allocate memory for enum value
  - Store variant tag
  - Store associated data (if present)
  - Return pointer to enum value

- Implement pattern matching:
  - Extract tag from enum value
  - Generate jump table or conditional branches
  - Bind data to variables in pattern
  - Execute corresponding match arm

**Suggested representation**:
```
Enum value in memory:
  [tag: i64][data: varies]

For Option::None:
  [0][unused]

For Option::Some(42):
  [1][42]

For variants with reference types:
  [tag][pointer to heap data]
```

#### 4. VM Support
**Status**: Not implemented

**What needs to be done**:
- May need new VM instructions for enum operations:
  - `load_variant_tag` - extract variant tag
  - `construct_enum` - create enum value with tag and data
  - Pattern matching can likely use existing conditional jump instructions

- Alternatively, compile enums to existing primitives:
  - Use structs with a tag field
  - Use integer tags + parallel data storage

#### 5. Additional Features (Future)

**Multi-field variants**:
```rust
enum Message {
    Move { x: i64, y: i64 },
    Write(string),
    Quit,
}
```

**Recursive enums**:
```rust
enum List {
    Cons(i64, List),  // Needs heap allocation
    Nil,
}
```

**Generic enums** (far future):
```rust
enum Option<T> {
    Some(T),
    None,
}
```

## Testing Strategy

### Unit Tests (Parser)
- ✅ Enum definition parsing
- ✅ Enum literal construction
- 🚧 Match expression parsing
- 🚧 Pattern parsing

### Integration Tests (Semantics)
- 🚧 Type checking enum definitions
- 🚧 Type checking enum construction
- 🚧 Type checking match expressions
- 🚧 Exhaustiveness checking
- 🚧 Module system integration

### End-to-End Tests
- 🚧 Simple enum example (Option type)
- 🚧 Result type with error handling
- 🚧 State machine with enums
- 🚧 Enum with imports/exports

### Example Test Programs

**test_option.pag**:
```rust
enum Option {
    Some(i64),
    None,
}

fn unwrap_or(opt: Option, default: i64) -> i64 {
    match opt {
        Option::Some(x) => x,
        Option::None => default,
    }
}

{
    let x = Option::Some(42);
    let y = Option::None;

    let result1 = unwrap_or(x, 0);  // Should be 42
    let result2 = unwrap_or(y, 99); // Should be 99
}
```

**test_result.pag**:
```rust
enum Result {
    Ok(i64),
    Err(string),
}

fn divide(a: i64, b: i64) -> Result {
    if b == 0 {
        Result::Err("division by zero")
    } else {
        Result::Ok(a / b)
    }
}

{
    let r1 = divide(10, 2);  // Ok(5)
    let r2 = divide(10, 0);  // Err("...")

    match r1 {
        Result::Ok(val) => val,
        Result::Err(msg) => -1,
    }
}
```

## Architecture Notes

### Parser Design Decisions

1. **Two-token lookahead for `:::`**: The parser distinguishes between:
   - `module::function` (qualified call)
   - `module::Struct` (qualified struct)
   - `Enum::Variant` (local enum)
   - `module::Enum::Variant` (qualified enum)

2. **Optional associated data**: Variants can have no data, requiring careful handling of the `(...)` syntax.

3. **Integration with existing expression parsing**: Enum literals are parsed as primary expressions alongside variables, literals, and function calls.

### Type System Design Decisions

1. **Simple enum representation**: `Type::Enum(String)` contains the enum name. Variant information is looked up from the enum definition.

2. **Qualified names**: Module-qualified enums are represented with `module::EnumName` in the type string.

3. **Future consideration**: May need to extend Type enum to include variant information for better error messages.

### Semantic Analysis Considerations

1. **Symbol table**: Need to track:
   - Defined enums (name → EnumDef)
   - Enum variants (enum_name + variant_name → associated type)
   - Imported enums from modules

2. **Pattern exhaustiveness**: Can be checked by:
   - Collecting all variants from enum definition
   - Collecting all patterns in match arms
   - Ensuring every variant is covered

3. **Type inference**: Match expressions need to infer their result type from all arms.

## Implementation Roadmap

### Phase 1: Core Parsing (✅ Complete)
1. ✅ Tokenizer support
2. ✅ AST nodes
3. ✅ Enum definition parsing
4. ✅ Enum literal parsing
5. ✅ Basic tests

### Phase 2: Pattern Matching Parsing (Next)
1. 🚧 Parse `match` keyword and expression
2. 🚧 Parse match arms
3. 🚧 Parse patterns with bindings
4. 🚧 Add match expression tests

### Phase 3: Semantic Analysis
1. 🚧 Enum definition validation
2. 🚧 Enum construction type checking
3. 🚧 Match expression type checking
4. 🚧 Exhaustiveness checking
5. 🚧 Module system integration

### Phase 4: Code Generation
1. 🚧 Design runtime representation
2. 🚧 Implement enum construction
3. 🚧 Implement pattern matching
4. 🚧 End-to-end tests

### Phase 5: Advanced Features (Future)
1. 🚧 Multi-field variants
2. 🚧 Recursive enums
3. 🚧 Generic enums (requires generics support)

## Known Issues / TODOs

1. **Parser**: Match expression parsing not implemented
2. **Semantics**: All enum-related semantic checks return UnsupportedExpr
3. **Bytecode**: All enum operations return UnsupportedExpr
4. **Testing**: Need integration tests for full enum pipeline
5. **Error Messages**: Need better error messages for enum-related errors
6. **Documentation**: Need user-facing documentation for enum syntax

## References

- Rust enums: https://doc.rust-lang.org/book/ch06-00-enums.html
- Pattern matching: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Algebraic data types: https://en.wikipedia.org/wiki/Algebraic_data_type
