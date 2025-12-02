# Module System Design

## Implementation Status

### ✅ Completed Features

- [x] **Tokenizer**: `pub`, `import`, and `::` tokens
- [x] **Parser**: Import statements, qualified names (module::name), pub keyword
- [x] **AST**: Import, QualifiedCall, QualifiedStructLiteral nodes
- [x] **Semantic Analysis**: Module loading, visibility checking, type checking
- [x] **Bytecode Generation**: Qualified function calls and struct literals
- [x] **Error Handling**: Module not found, private access, unknown exports

### 🚧 Limitations & TODO

- [ ] **Recursive Imports**: Imported modules cannot have their own imports yet
  - Structure for circular import detection is in place
  - Needs recursive module loading implementation
- [ ] **Advanced Features**: Selective imports, aliases, re-exports (planned for future)
- [ ] **Return Type Inference**: All functions assumed to return i64 for now
- [ ] **Name Mangling**: Uses simple `module_function` format (works but basic)

## Overview

This document outlines the design for a minimal module system in Pagoda. The system supports:
- Public and private functions and structs using the `pub` keyword
- Importing modules with `import module_name;`
- Accessing imported items with qualified names (`module::item`)

## Goals

- **Encapsulation**: ✅ Private functions/structs are not accessible outside their module
- **Clear visibility**: ✅ `pub` keyword explicitly marks public items
- **Type safety**: ✅ Type checking across module boundaries
- **Circular import detection**: ⚠️ Detection code exists but not fully tested with recursive imports
- **Name mangling**: ✅ Basic name mangling implemented (`module_function` format)

## Syntax Examples

### Basic Module with Public and Private Items

```pagoda
// ===== math.pag =====
pub fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

pub fn multiply(a: i64, b: i64) -> i64 {
    return a * b;
}

fn helper(x: i64) -> i64 {  // private - no pub keyword
    return x * 2;
}

pub struct Point {
    x: i64,
    y: i64,
}

struct InternalData {  // private
    value: i64,
}
```

### Importing and Using Modules

```pagoda
// ===== main.pag =====
import math;

fn main() {
    let sum = math::add(5, 3);
    let product = math::multiply(4, 7);

    let p = math::Point { x: 10, y: 20 };

    // ERROR: helper is private
    // let x = math::helper(5);

    return sum;
}
```

### Multi-Module Example

```pagoda
// ===== vec2.pag =====
pub struct Vec2 {
    x: i64,
    y: i64,
}

pub fn add(a: Vec2, b: Vec2) -> Vec2 {
    return Vec2 {
        x: a.x + b.x,
        y: a.y + b.y,
    };
}

// ===== physics.pag =====
import vec2;

pub struct Body {
    pos: vec2::Vec2,
    vel: vec2::Vec2,
}

pub fn update(body: Body, dt: i64) -> Body {
    let new_pos = vec2::add(body.pos, body.vel);
    return Body {
        pos: new_pos,
        vel: body.vel,
    };
}

// ===== main.pag =====
import vec2;
import physics;

fn main() {
    let pos = vec2::Vec2 { x: 0, y: 0 };
    let vel = vec2::Vec2 { x: 10, y: 5 };

    let body = physics::Body { pos: pos, vel: vel };
    let updated = physics::update(body, 1);

    return updated.pos.x;
}
```

## Working Examples

See `examples/` directory for complete working examples:
- `examples/vec2.pag` - Vector2D module with public structs and functions
- `examples/shapes.pag` - Shapes module demonstrating public/private items
- `examples/module_demo.pag` - Main program using multiple imports

```bash
# Test the module system
cargo test

# View the working example
cat examples/module_demo.pag
```

## Implementation Details

### Phase 1: Tokenizer Changes ✅ COMPLETED

Three new token types were added:

```rust
// src/pagoda/tokenizer.rs

pub enum TokenKind {
    // ... existing tokens ...
    Pub,         // new: pub keyword
    Import,      // new: import keyword
    ColonColon,  // new: :: operator for qualified names
    // ...
}
```

#### Keyword Recognition

Update the identifier matching to recognize new keywords:

```rust
let kind = match lexeme {
    "let" => TokenKind::Let,
    "return" => TokenKind::Return,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "for" => TokenKind::For,
    "fn" => TokenKind::Fn,
    "struct" => TokenKind::Struct,
    "true" => TokenKind::True,
    "false" => TokenKind::False,
    "pub" => TokenKind::Pub,      // new
    "import" => TokenKind::Import, // new
    _ => TokenKind::Ident(lexeme.to_string()),
};
```

#### ColonColon Operator

Add tokenization for `::` in the two-character operator section:

```rust
if idx + 1 < bytes.len() {
    let two = &bytes[idx..idx + 2];
    // ... existing two-char operators ...

    if two == b"::" {
        let span = Span {
            start: idx,
            end: idx + 2,
            literal: "::".to_string(),
        };
        tokens.push(Token {
            kind: TokenKind::ColonColon,
            span,
        });
        idx += 2;
        continue;
    }
}
```

### Phase 2: AST Changes ✅ COMPLETED

The AST was updated to support module system features:

```rust
// src/pagoda/mod.rs

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub imports: Vec<Import>,    // new: list of imports at top of file
    pub structs: Vec<StructDef>,
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub module_name: String,  // e.g., "math" for "import math;"
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub is_public: bool,  // new: true if has pub keyword
    pub name: String,
    pub params: Vec<String>,
    pub body: Stmt,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub is_public: bool,  // new: true if has pub keyword
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}
```

#### New Expression Variants

Add support for qualified names:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    // ... existing variants ...

    /// Qualified function call: math::add(1, 2)
    QualifiedCall {
        module: String,       // "math"
        name: String,         // "add"
        args: Vec<Expr>,
        span: Span,
    },

    /// Qualified struct literal: math::Point { x: 1, y: 2 }
    QualifiedStructLiteral {
        module: String,
        struct_name: String,
        field_values: Vec<(String, Expr)>,
        span: Span,
    },
}
```

#### Update span() method

Add cases for new expression variants:

```rust
impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::IntLiteral { span, .. } => span,
            Expr::BoolLiteral { span, .. } => span,
            // ... existing cases ...
            Expr::QualifiedCall { span, .. } => span,
            Expr::QualifiedStructLiteral { span, .. } => span,
        }
    }
}
```

### Phase 3: Parser Changes ✅ COMPLETED

#### Program Structure

`parse_program` was updated to handle imports and pub modifiers:

```rust
// src/pagoda/parser.rs

fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut cursor = 0;
    let start = tokens.first().map(|t| t.span.start).unwrap_or(0);

    // Parse imports first (must come before any other declarations)
    let mut imports = Vec::new();
    while let Some(tok) = tokens.get(cursor) {
        if !matches!(tok.kind, TokenKind::Import) {
            break;
        }
        imports.push(parse_import(tokens, &mut cursor)?);
    }

    // Parse structs, functions, and statements
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut stmts = Vec::new();

    while cursor < tokens.len() && !matches!(tokens[cursor].kind, TokenKind::Eof) {
        let tok = &tokens[cursor];

        // Check for pub keyword
        let is_public = matches!(tok.kind, TokenKind::Pub);
        if is_public {
            cursor += 1;
        }

        let tok = tokens.get(cursor).ok_or_else(|| ParseError::UnexpectedEof {
            span_start: start,
            span_end: start,
        })?;

        match tok.kind {
            TokenKind::Struct => {
                let mut struct_def = parse_struct(tokens, &mut cursor)?;
                struct_def.is_public = is_public;
                structs.push(struct_def);
            }
            TokenKind::Fn => {
                let mut func = parse_function(tokens, &mut cursor)?;
                func.is_public = is_public;
                functions.push(func);
            }
            _ => {
                if is_public {
                    return Err(ParseError::UnexpectedToken {
                        expected: "struct or fn after pub".to_string(),
                        span_start: tok.span.start,
                        span_end: tok.span.end,
                        found: tok.kind.clone(),
                    });
                }
                stmts.push(parse_stmt(tokens, &mut cursor)?);
            }
        }
    }

    let end = tokens.last().map(|t| t.span.end).unwrap_or(start);

    Ok(Program {
        imports,
        structs,
        functions,
        stmts,
        span: Span {
            start,
            end,
            literal: String::new(),
        },
    })
}
```

#### Import Statement Parsing

```rust
fn parse_import(tokens: &[Token], cursor: &mut usize) -> Result<Import, ParseError> {
    let import_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;

    if !matches!(import_tok.kind, TokenKind::Import) {
        return Err(ParseError::UnexpectedToken {
            expected: "import".to_string(),
            span_start: import_tok.span.start,
            span_end: import_tok.span.end,
            found: import_tok.kind.clone(),
        });
    }
    *cursor += 1;

    let name_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
        span_start: import_tok.span.end,
        span_end: import_tok.span.end,
    })?;

    let module_name = match &name_tok.kind {
        TokenKind::Ident(name) => name.clone(),
        _ => {
            return Err(ParseError::UnexpectedToken {
                expected: "module name".to_string(),
                span_start: name_tok.span.start,
                span_end: name_tok.span.end,
                found: name_tok.kind.clone(),
            });
        }
    };
    *cursor += 1;

    let semi_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
        span_start: name_tok.span.end,
        span_end: name_tok.span.end,
    })?;

    if !matches!(semi_tok.kind, TokenKind::Semicolon) {
        return Err(ParseError::UnexpectedToken {
            expected: ";".to_string(),
            span_start: semi_tok.span.start,
            span_end: semi_tok.span.end,
            found: semi_tok.kind.clone(),
        });
    }
    *cursor += 1;

    Ok(Import {
        module_name,
        span: Span {
            start: import_tok.span.start,
            end: semi_tok.span.end,
            literal: format!("import {};", module_name),
        },
    })
}
```

#### Qualified Name Parsing

Update `parse_primary` to handle `module::name` syntax:

```rust
fn parse_primary(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let token = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;

    // Check for qualified names (module::name)
    if let TokenKind::Ident(first_name) = &token.kind {
        // Look ahead for ::
        if matches!(tokens.get(*cursor + 1).map(|t| &t.kind), Some(TokenKind::ColonColon)) {
            let module_name = first_name.clone();
            let start_span = token.span.clone();
            *cursor += 2; // Skip ident and ::

            let name_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
                span_start: start_span.end,
                span_end: start_span.end,
            })?;

            let item_name = match &name_tok.kind {
                TokenKind::Ident(name) => name.clone(),
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "identifier after ::".to_string(),
                        span_start: name_tok.span.start,
                        span_end: name_tok.span.end,
                        found: name_tok.kind.clone(),
                    });
                }
            };
            *cursor += 1;

            let next_tok = tokens.get(*cursor);

            // Check if it's a function call: module::func(...)
            if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::LParen)) {
                *cursor += 1;

                let mut args = Vec::new();
                if !matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::RParen)) {
                    loop {
                        args.push(parse_expr(tokens, cursor)?);
                        if !matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::Comma)) {
                            break;
                        }
                        *cursor += 1;
                    }
                }

                let rparen = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
                    span_start: start_span.start,
                    span_end: start_span.end,
                })?;

                if !matches!(rparen.kind, TokenKind::RParen) {
                    return Err(ParseError::UnexpectedToken {
                        expected: ")".to_string(),
                        span_start: rparen.span.start,
                        span_end: rparen.span.end,
                        found: rparen.kind.clone(),
                    });
                }
                *cursor += 1;

                return Ok(Expr::QualifiedCall {
                    module: module_name,
                    name: item_name,
                    args,
                    span: Span {
                        start: start_span.start,
                        end: rparen.span.end,
                        literal: format!("{}::{}(...)", module_name, item_name),
                    },
                });
            }
            // Check if it's a struct literal: module::Struct { ... }
            else if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::LBrace)) {
                *cursor += 1;

                let mut field_values = Vec::new();
                while !matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::RBrace)) {
                    let field_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
                        span_start: start_span.start,
                        span_end: start_span.end,
                    })?;

                    let field_name = match &field_tok.kind {
                        TokenKind::Ident(name) => name.clone(),
                        _ => {
                            return Err(ParseError::UnexpectedToken {
                                expected: "field name".to_string(),
                                span_start: field_tok.span.start,
                                span_end: field_tok.span.end,
                                found: field_tok.kind.clone(),
                            });
                        }
                    };
                    *cursor += 1;

                    // Expect colon
                    let colon_tok = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
                        span_start: field_tok.span.end,
                        span_end: field_tok.span.end,
                    })?;
                    if !matches!(colon_tok.kind, TokenKind::Colon) {
                        return Err(ParseError::UnexpectedToken {
                            expected: ":".to_string(),
                            span_start: colon_tok.span.start,
                            span_end: colon_tok.span.end,
                            found: colon_tok.kind.clone(),
                        });
                    }
                    *cursor += 1;

                    let value = parse_expr(tokens, cursor)?;
                    field_values.push((field_name, value));

                    // Optional comma
                    if matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::Comma)) {
                        *cursor += 1;
                    }
                }

                let rbrace = tokens.get(*cursor).ok_or_else(|| ParseError::UnexpectedEof {
                    span_start: start_span.start,
                    span_end: start_span.end,
                })?;

                if !matches!(rbrace.kind, TokenKind::RBrace) {
                    return Err(ParseError::UnexpectedToken {
                        expected: "}".to_string(),
                        span_start: rbrace.span.start,
                        span_end: rbrace.span.end,
                        found: rbrace.kind.clone(),
                    });
                }
                *cursor += 1;

                return Ok(Expr::QualifiedStructLiteral {
                    module: module_name,
                    struct_name: item_name,
                    field_values,
                    span: Span {
                        start: start_span.start,
                        end: rbrace.span.end,
                        literal: format!("{}::{} {{ ... }}", module_name, item_name),
                    },
                });
            }
        }
    }

    // Fall through to existing primary expression parsing
    // ... (existing code for literals, variables, etc.)
}
```

#### Update expr_with_span

Add cases for new expression types:

```rust
fn expr_with_span(expr: Expr, span: Span) -> Expr {
    match expr {
        Expr::IntLiteral { value, .. } => Expr::IntLiteral { value, span },
        Expr::BoolLiteral { value, .. } => Expr::BoolLiteral { value, span },
        // ... existing cases ...
        Expr::QualifiedCall { module, name, args, .. } => {
            Expr::QualifiedCall { module, name, args, span }
        }
        Expr::QualifiedStructLiteral { module, struct_name, field_values, .. } => {
            Expr::QualifiedStructLiteral { module, struct_name, field_values, span }
        }
    }
}
```

### Phase 4: Semantic Analysis ✅ MOSTLY COMPLETED

**Implemented:**
- ✅ Load imported modules from disk
- ✅ Detect circular imports (basic detection in place)
- ✅ Resolve qualified names (module::function, module::Struct)
- ✅ Enforce visibility rules (pub vs private)
- ✅ Type checking across modules

**Limitations:**
- ⚠️ Imported modules cannot have their own imports (no recursive loading yet)
- ⚠️ Module loading is done in `analyze_program_with_modules()` function

This phase handles:

#### Module Data Structure ✅ IMPLEMENTED

Located in `src/pagoda/semantics.rs`:

```rust
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub file_path: PathBuf,
    pub program: Program,
    pub checked_program: CheckedProgram,
    pub public_functions: HashMap<String, FunctionSignature>,
    pub public_structs: HashMap<String, StructSignature>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub param_count: usize,  // Simplified: just count for now
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructSignature {
    pub fields: HashMap<String, Type>,
}
```

#### New Error Types ✅ IMPLEMENTED

All error types have been added to `SemanticError` enum:

```rust
#[error("module '{name}' not found")]
ModuleNotFound { name: String, span: Span },

#[error("circular import detected: {cycle}")]
CircularImport { cycle: String, span: Span },

#[error("function '{name}' is private in module '{module}'")]
PrivateFunction { module: String, name: String, span: Span },

#[error("struct '{name}' is private in module '{module}'")]
PrivateStruct { module: String, name: String, span: Span },

#[error("module '{module}' does not export function '{name}'")]
UnknownModuleFunction { module: String, name: String, span: Span },

#[error("module '{module}' does not export struct '{name}'")]
UnknownModuleStruct { module: String, name: String, span: Span },
```

#### Module Loading ✅ IMPLEMENTED

**Current Implementation:**
The main entry point is `analyze_program_with_modules()` in `src/pagoda/semantics.rs`:

```rust
pub fn analyze_program_with_modules(
    program: Program,
    base_dir: &Path,
) -> Result<HashMap<String, Module>, SemanticError>
```

This function:
1. Loads all directly imported modules from disk
2. Parses and type-checks each module
3. Extracts public interfaces
4. Type-checks the main program with access to imports
5. Returns a HashMap of all loaded modules

**Limitation:** Imported modules cannot have their own imports (no recursive loading).

**Original Design (for future reference):**

```rust
pub fn analyze_with_modules_recursive(
    root_program: Program,
    root_path: &Path,
) -> Result<HashMap<String, Module>, SemanticError> {
    let mut modules = HashMap::new();
    let mut loading_stack = Vec::new();  // For circular import detection

    // Get the directory containing the root file
    let base_dir = root_path.parent().unwrap_or(Path::new("."));

    load_module_recursive(
        "main",
        root_program,
        root_path.to_path_buf(),
        base_dir,
        &mut modules,
        &mut loading_stack,
    )?;

    Ok(modules)
}

fn load_module_recursive(
    module_name: &str,
    program: Program,
    file_path: PathBuf,
    base_dir: &Path,
    modules: &mut HashMap<String, Module>,
    loading_stack: &mut Vec<String>,
) -> Result<(), SemanticError> {
    // Check for circular imports
    if loading_stack.contains(&module_name.to_string()) {
        let cycle = format!("{} -> {}", loading_stack.join(" -> "), module_name);
        return Err(SemanticError::CircularImport {
            cycle,
            span: program.span.clone(),
        });
    }

    // Already loaded?
    if modules.contains_key(module_name) {
        return Ok(());
    }

    loading_stack.push(module_name.to_string());

    // Load each imported module first (depth-first)
    for import in &program.imports {
        let module_file = base_dir.join(format!("{}.pag", import.module_name));

        if !module_file.exists() {
            return Err(SemanticError::ModuleNotFound {
                name: import.module_name.clone(),
                span: import.span.clone(),
            });
        }

        // Parse the imported module
        let source = std::fs::read_to_string(&module_file)
            .map_err(|_| SemanticError::ModuleNotFound {
                name: import.module_name.clone(),
                span: import.span.clone(),
            })?;

        let imported_program = crate::pagoda::parse_source(&source)
            .map_err(|e| SemanticError::UnsupportedExpr {
                span: import.span.clone(),
            })?;

        // Recursively load that module
        load_module_recursive(
            &import.module_name,
            imported_program,
            module_file,
            base_dir,
            modules,
            loading_stack,
        )?;
    }

    // Now type-check this module
    // It can see the public interfaces of all imported modules
    let mut imported_functions: HashMap<String, HashMap<String, FunctionSignature>> = HashMap::new();
    let mut imported_structs: HashMap<String, HashMap<String, StructSignature>> = HashMap::new();

    for import in &program.imports {
        let imported_module = modules.get(&import.module_name).unwrap();
        imported_functions.insert(
            import.module_name.clone(),
            imported_module.public_functions.clone(),
        );
        imported_structs.insert(
            import.module_name.clone(),
            imported_module.public_structs.clone(),
        );
    }

    let checked_program = analyze_program_with_imports(
        &program,
        &imported_functions,
        &imported_structs,
    )?;

    // Extract public interface from this module
    let mut public_functions = HashMap::new();
    for func in &program.functions {
        if func.is_public {
            public_functions.insert(
                func.name.clone(),
                FunctionSignature {
                    params: func.params.clone(),
                    return_type: Type::Int,  // TODO: infer actual return type
                },
            );
        }
    }

    let mut public_structs = HashMap::new();
    for struct_def in &program.structs {
        if struct_def.is_public {
            let mut fields = HashMap::new();
            for field in &struct_def.fields {
                let field_type = match field.ty.as_str() {
                    "i64" => Type::Int,
                    "string" => Type::String,
                    _ => {
                        // TODO: Handle struct types
                        return Err(SemanticError::UnsupportedExpr {
                            span: field.span.clone(),
                        });
                    }
                };
                fields.insert(field.name.clone(), field_type);
            }
            public_structs.insert(
                struct_def.name.clone(),
                StructSignature { fields },
            );
        }
    }

    modules.insert(
        module_name.to_string(),
        Module {
            name: module_name.to_string(),
            file_path,
            program,
            checked_program,
            public_functions,
            public_structs,
        },
    );

    loading_stack.pop();
    Ok(())
}
```

#### Type Checking with Imports

```rust
fn analyze_program_with_imports(
    program: &Program,
    imported_functions: &HashMap<String, HashMap<String, FunctionSignature>>,
    imported_structs: &HashMap<String, HashMap<String, StructSignature>>,
) -> Result<CheckedProgram, SemanticError> {
    // Build local scope with functions and structs from this module
    let mut functions: HashMap<String, usize> = HashMap::new();
    let mut structs: HashMap<String, HashMap<String, Type>> = HashMap::new();

    // Add local structs
    for struct_def in &program.structs {
        let mut fields = HashMap::new();
        for field in &struct_def.fields {
            let field_ty = match field.ty.as_str() {
                "i64" => Type::Int,
                "string" => Type::String,
                _ => {
                    return Err(SemanticError::UnsupportedExpr {
                        span: field.span.clone(),
                    });
                }
            };
            fields.insert(field.name.clone(), field_ty);
        }
        structs.insert(struct_def.name.clone(), fields);
    }

    // Add local functions
    for func in &program.functions {
        if func.params.len() > 255 {
            return Err(SemanticError::UnsupportedExpr {
                span: func.span.clone(),
            });
        }
        functions.insert(func.name.clone(), func.params.len());
    }

    // Type-check each function
    let mut checked_functions = Vec::new();
    for func in &program.functions {
        let mut fn_scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
        for pname in &func.params {
            fn_scopes
                .last_mut()
                .unwrap()
                .insert(pname.clone(), Type::Int);
        }

        let checked_body = analyze_stmt_with_imports(
            &func.body,
            &mut fn_scopes,
            &functions,
            &structs,
            imported_functions,
            imported_structs,
        )?;

        checked_functions.push(crate::pagoda::CheckedFunction {
            name: func.name.clone(),
            params: func.params.clone(),
            body: checked_body,
            span: func.span.clone(),
        });
    }

    // Type-check top-level statements
    let mut scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
    let mut checked_stmts = Vec::new();
    for stmt in &program.stmts {
        let checked = analyze_stmt_with_imports(
            stmt,
            &mut scopes,
            &functions,
            &structs,
            imported_functions,
            imported_structs,
        )?;
        checked_stmts.push(checked);
    }

    Ok(CheckedProgram {
        structs: program.structs.clone(),
        functions: checked_functions,
        stmts: checked_stmts,
        span: program.span.clone(),
    })
}

fn analyze_stmt_with_imports(
    stmt: &Stmt,
    scopes: &mut Vec<HashMap<String, Type>>,
    functions: &HashMap<String, usize>,
    structs: &HashMap<String, HashMap<String, Type>>,
    imported_functions: &HashMap<String, HashMap<String, FunctionSignature>>,
    imported_structs: &HashMap<String, HashMap<String, StructSignature>>,
) -> Result<CheckedStmt, SemanticError> {
    // Similar to analyze_stmt but pass imported_functions/structs to analyze_expr_with_imports
    // ... (delegate to analyze_expr_with_imports for expressions)
}

fn analyze_expr_with_imports(
    expr: &Expr,
    scopes: &Vec<HashMap<String, Type>>,
    functions: &HashMap<String, usize>,
    structs: &HashMap<String, HashMap<String, Type>>,
    imported_functions: &HashMap<String, HashMap<String, FunctionSignature>>,
    imported_structs: &HashMap<String, HashMap<String, StructSignature>>,
) -> Result<CheckedExpr, SemanticError> {
    match expr {
        // Handle QualifiedCall: module::function(args)
        Expr::QualifiedCall {
            module,
            name,
            args,
            span,
        } => {
            // Look up the module
            let module_functions = imported_functions.get(module).ok_or_else(|| {
                SemanticError::UnknownModuleFunction {
                    module: module.clone(),
                    name: name.clone(),
                    span: span.clone(),
                }
            })?;

            // Look up the function in that module
            let func_sig = module_functions.get(name).ok_or_else(|| {
                SemanticError::UnknownModuleFunction {
                    module: module.clone(),
                    name: name.clone(),
                    span: span.clone(),
                }
            })?;

            // Type-check arguments
            if args.len() != func_sig.params.len() {
                return Err(SemanticError::ArityMismatch {
                    name: format!("{}::{}", module, name),
                    expected: func_sig.params.len(),
                    found: args.len(),
                    span: span.clone(),
                });
            }

            for arg in args {
                let checked_arg = analyze_expr_with_imports(
                    arg,
                    scopes,
                    functions,
                    structs,
                    imported_functions,
                    imported_structs,
                )?;

                // TODO: Check argument types match parameter types
            }

            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: func_sig.return_type.clone(),
            })
        }

        // Handle QualifiedStructLiteral: module::Struct { fields }
        Expr::QualifiedStructLiteral {
            module,
            struct_name,
            field_values,
            span,
        } => {
            // Look up the module
            let module_structs = imported_structs.get(module).ok_or_else(|| {
                SemanticError::UnknownModuleStruct {
                    module: module.clone(),
                    name: struct_name.clone(),
                    span: span.clone(),
                }
            })?;

            // Look up the struct in that module
            let struct_sig = module_structs.get(struct_name).ok_or_else(|| {
                SemanticError::UnknownModuleStruct {
                    module: module.clone(),
                    name: struct_name.clone(),
                    span: span.clone(),
                }
            })?;

            // Type-check field values
            for (field_name, field_expr) in field_values {
                let field_type = struct_sig.fields.get(field_name).ok_or_else(|| {
                    SemanticError::UnknownVariable {
                        name: field_name.clone(),
                        span: field_expr.span().clone(),
                    }
                })?;

                let checked_field = analyze_expr_with_imports(
                    field_expr,
                    scopes,
                    functions,
                    structs,
                    imported_functions,
                    imported_structs,
                )?;

                if &checked_field.ty != field_type {
                    return Err(SemanticError::TypeMismatch {
                        expected: field_type.clone(),
                        found: checked_field.ty,
                        span: field_expr.span().clone(),
                    });
                }
            }

            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Struct(format!("{}::{}", module, struct_name)),
            })
        }

        // All other expressions: delegate to existing analyze_expr logic
        // but recursively call analyze_expr_with_imports for sub-expressions
        _ => {
            // TODO: Update all existing expression analysis to call
            // analyze_expr_with_imports recursively
            analyze_expr(expr, scopes, functions, structs)
        }
    }
}
```

### Phase 5: Bytecode Generation ✅ COMPLETED

Bytecode generation for module system was implemented in `src/pagoda/bytecode.rs`:

- **QualifiedCall**: Generates `call module_function` labels
- **QualifiedStructLiteral**: Generates struct allocation with qualified type lookup
- **Name Mangling**: Uses `{module}_{function}` format (e.g., `math_add`)

## Usage Example

### File Structure ✅ WORKING
```
examples/
  module_demo.pag   # Main program
  vec2.pag          # Vector module
  shapes.pag        # Shapes module
  math.pag          # Math utilities
```

### Testing
```bash
# Run all tests (includes module system tests)
cargo test

# Test module loading programmatically
# See test examples in the working code
```

### ⚠️ CLI Integration TODO
Currently the module system works via `analyze_program_with_modules()` but is not yet integrated into the main CLI (`--pagoda` flag). This requires updating `src/main.rs` to detect imports and use the new analysis function.

### Error Examples

**Module not found:**
```
error: module 'math' not found
  --> main.pag:1:8
   |
 1 | import math;
   |        ^^^^
```

**Circular import:**
```
error: circular import detected: a -> b -> a
  --> a.pag:1:8
   |
 1 | import b;
   |        ^
```

**Private function:**
```
error: function 'helper' is private in module 'math'
  --> main.pag:5:13
   |
 5 |     let x = math::helper(5);
   |             ^^^^^^^^^^^^
```

## Next Steps

### High Priority
1. **Recursive Module Loading**: Allow imported modules to have their own imports
   - The circular import detection is already in place
   - Need to implement depth-first loading
2. **CLI Integration**: Update `src/main.rs` to use `analyze_program_with_modules()`
3. **Return Type Inference**: Track actual return types instead of assuming i64

### Future Extensions
- Selective imports: `use math::{add, multiply};`
- Import aliases: `import math as m;`
- Re-exports: `pub use math::add;`
- Nested modules: `import utils::math;`
- Visibility levels: `pub(crate)`, `pub(super)`
- Module-level constants: `pub const PI: i64 = 3;`
- Improved name mangling (consider using `::` separator in labels)

## Resolved Design Questions

1. ✅ **Qualified struct types**: Yes, use `Type::Struct("module::Name")` format
2. ✅ **Visibility**: Started with simple `pub` vs private, more levels can be added later
3. ⏸️ **Standard library**: Deferred - no auto-imported modules yet
4. ⏸️ **Method mangling**: Deferred - no methods yet
