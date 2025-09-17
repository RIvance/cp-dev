# The CP Programming Language

CP is a *compositional programming* language, founded on a core calculus named *Fi+*. This repository is a novel implementation of CP in scala, with a full-featured multi-stage compiler. 

A crash course in CP can be found in Chapter 3 of Yaozhu Sun's PhD thesis: [*Compositional Programming in Action*](https://github.com/yzyzsun/PhD-thesis/blob/main/Thesis.pdf). 

This is the third major implementation of CP (CP3), following the original [CP-next (CP2)](https://github.com/yzyzsun/CP-next) and the [CP1](https://github.com/wxzh/CP). 

This implementation features a modern REPL, a multi-stage compiler (WIP), a novel module system, upper/lower bounded polymorphism, FFI (WIP), and many syntax enhancements. 

## Language Features

- A typed lambda calculus with five base types (`Int` `Double` `String` `Bool` `()`), built-in arrays (`[1; 2; 3] : [Int]`), and references[^Ref];
- The merge operator[^Merge], disjoint intersection types[^λi] and disjoint polymorphism[^Fi];
- Distributive intersection subtyping and nested composition[^NeColus];
- Compositional programming[^CP] with first-class traits[^SEDEL];
- Iso-recursive types with nominal unfolding[^IsoRec];
- Generalized record operations with type difference[^TypeDiff];
- Named and optional arguments via a blend of intersection and union types[^IU];
- Type-directed operational semantics for *Fi+*[^Fi+] with {HOAS,substitution,closure}-based {big,small}-step variants;
- A compiler targeting JavaScript, which features type-safe compilation of dynamic inheritance[^Comp];
- A compositionally embedded DSL for document authoring called ExT[^ExT].


## Online Demo

[PLGround](https://plground.org) provides an online CP interpreter and a wiki-like document repository. Documents are written in ExT and rendered in your web browser.

Since the frontend code uses the Fetch API, PLGround is expected to work on Chrome 42+, Firefox 39+, Edge 14+, Safari 10.1+, or other modern browsers.

## Toolchain Setup

### Prerequisites

#### 1. Install Rust Toolchain
The CP command-line interface is built with Rust. Install via [`rustup`](https://www.rust-lang.org/tools/install):

```sh
# One-line installation (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Alternatively, use package managers:
# macOS: brew install rustup
# Ubuntu: sudo apt install rustup
# Windows: download from rust-lang.org
```

After installation, verify with:
```sh
rustc --version
```

#### 2. Install GraalVM Toolchain
The CP compiler core is written in Scala and uses GraalVM for native compilation. We recommend using [SDKMAN!](https://sdkman.io/) for easy management:

```sh
# Install SDKMAN! (Java toolchain manager)
curl -s "https://get.sdkman.io" | bash

# Reload your shell or run:
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Install GraalVM Community Edition (required version)
sdk install java 17.0.9-graalce

# Set as default JDK
sdk default java 17.0.9-graalce

# Install native-image tool (for building standalone binaries)
gu install native-image
```

Verify installation:
```sh
java --version  # Should show GraalVM
native-image --version
```

### Building CP Toolchain

Compile the complete CP toolchain to a native binary:

```sh
# Build native executable (may take several minutes)
./gradlew nativeCompile

# The output binary will be at:
# ${PROJECT_ROOT}/cli/build/nativeCompile/comp
```

### Using the CP Toolchain

The compiled `comp` binary provides three main commands:

#### Start REPL (Interactive Shell)
```sh
comp          # Start interactive session
comp repl     # Alternative explicit command
```

#### Compile CP Programs (WIP)
```sh
# Compile single file
comp build example.cp

# Compile entire project (looks for __main__.cp)
comp build .

# Specify output directory and target
comp build -o ./index.js -t 'js' example.cp
```

#### Run CP Programs (Interpreted)
```sh
# Execute single file
comp run example.cp

# Run project (uses __main__.cp as entry point)
comp run .
```

### Troubleshooting

Common issues and solutions:

1. **GraalVM not detected**:
   ```sh
   sdk use java 17.0.9-graalce
   ```

2. **Native image build fails**:
   ```sh
   gu install native-image  # Ensure native-image is installed
   ```

3. **Check tool versions**:
   ```sh
   rustc --version    # Should be 1.70+
   java --version     # Should show GraalVM 17
   native-image --version
   ```

## What's New

### Syntax Enhancements

#### More Flexible Function Definitions
CP now supports multiple definition styles for improved readability and coding preference:

```scala
// Traditional CP style with curried parameters
max (a: Int) (b: Int) = { if a > b then a else b } : Int;

// Compact single-parameter-list style
max (a b: Int): Int = if a > b then { a } else { b };

// Familiar def syntax with comma-separated parameters
def max(a: Int, b: Int): Int = if a > b then a else b;
```

#### Codeblocks with Implicit Returns
Braced code blocks now automatically return their last expression, eliminating the need for explicit `in` keywords:

```scala
// New brace-based syntax (automatically returns the final expression)
def areaOfCircle (d: Float) = {
  let pi = 3.14159;
  let r = d / 2.0;
  pi * r * r  // Implicit return
};

// Equivalent to traditional OCaml-style with explicit 'in' keywords
def areaOfCircle (d: Float) =
  let pi = 3.14159 in
  let r = d / 2.0 in
  (pi * r * r);
```

#### Disjoint Qualification

The disjoint qualification can be expressed in either complete or concise form:

**For values/expressions:**

```scala
/\T where T * Int . \(x: T) -> x ,, 42
/\(T * Int) . \(x: T) -> x ,, 42
```

**For functions/types:**
```scala
def mixin[T](base: Trait<T>) where T * { amazing: Bool } = 
  trait [self: T] inherits base => { amazing = true };

def mixin[T * { amazing: Bool }](base: Trait<T>) =
  trait [self: T] inherits base => { amazing = true };

mixin (T * { amazing: Bool }) (base: Trait<T>) =
  trait [self: T] inherits base => { amazing = true };
```

The concise form `(T * Constraint)` is semantically equivalent to the complete form `T where T * Constraint` - both specify that type `T` must be disjoint from the given constraint. 

### New Features

#### Module System with Hierarchical Organization
CP now features a novel module system supporting multi-level namespaces and structured project organization:

```
├── __main__.cp          # Main entry point
├── mod1.cp              # Top-level module
└── mod2                 # Directory-based module
    ├── __module__.cp    # Module declaration file
    ├── submod1.cp       # Submodule
    └── submod2.cp       # Submodule
```

Module definitions with nested submodules:
```scala
/* mod1.cp */
def helloMod1: String = "Hello from `mod1`";

module submod { // Nested submodule definition
  def helloSubmod: String = "Hello from `mod1::submod`";
}
```

```scala
/* mod2/__module__.cp */
def helloMod2: String = "Hello from `mod2`";
```

Selective imports from multiple modules:

```scala
// Selective import with submodule access
import mod1::{helloMod1, submod::helloSubmod};
// Top-level module import
import mod2::helloMod2;
```

#### Upperbound and Lowerbound Support

CP now supports bounded polymorphism through **upperbound** (`<:`) and **lowerbound** (`:>`) constraints, working alongside existing disjoint (`*`) constraints.

```scala
def mixin[T](base: Trait<T>) where 
  T * { amazing: Bool },      // Disjoint constraint (existing feature)
  T <: { isAmazing: Bool },   // Upperbound constraint (new feature)
= trait [self: T] inherits base => { 
  amazing = self.isAmazing    // `self` guaranteed to have `isAmazing`
};
```

#### Tuple Types and Destructuring

First-class tuple support with type-safe destructuring:

```rust
// Tuple creation with explicit type annotation
let studentInfo: (String, Int) = ("Alice", 114514);
// Pattern matching destructuring
let (name, id) = studentInfo;  // name = "Alice", id = 114514
```

#### Comprehensive Destructuring Assignments
Advanced destructuring for both tuples and records:

```rust
// Tuple destructuring with potential rest pattern
let (x, y, z, ...) = e;  // Extract first three elements
// Record destructuring with field extraction
let { l1 = e1; l2 = e2; ... } = e;  // Extract specific fields from record
```

#### Pattern Matching for Records (WIP)
Expressive pattern matching for record types with field extraction:

```scala
type User = { name: String; age: Int; active: Bool };

def canVote(user: User): Bool = match user {
  // Extract age field and match active = true pattern
  case { age; active = true } => (age >= 18);
  // Catch-all case for other patterns
  case _ => false;
};
```

#### Native Code Blocks (WIP)
Platform-specific native code integration with type safety:

```scala
def myPrintln(str: String): String = native[Unit] { 
  // JavaScript-specific implementation
  case "JavaScript" => {#
    console.log(str)  // Direct JavaScript code execution
  #};
  // Future platforms can be added here
  // case "WASM" => {# ... #};
  // case "LLVM" => {# ... #};
}
```


[^Merge]: Jana Dunfield. [Elaborating Intersection and Union Types](https://research.cs.queensu.ca/home/jana/papers/intcomp-jfp/Dunfield14_elaboration.pdf). *JFP 2014*.  
[^λi]: Bruno C. d. S. Oliveira, Zhiyuan Shi, and João Alpuim. [Disjoint Intersection Types](https://i.cs.hku.hk/~bruno/papers/icfp2016.pdf). *ICFP 2016*.  
[^Fi]: João Alpuim, Bruno C. d. S. Oliveira, and Zhiyuan Shi. [Disjoint Polymorphism](https://i.cs.hku.hk/~bruno/papers/ESOP2017.pdf). *ESOP 2017*.  
[^SEDEL]: Xuan Bi and Bruno C. d. S. Oliveira. [Typed First-Class Traits](https://i.cs.hku.hk/~bruno/papers/traits.pdf). *ECOOP 2018*.  
[^NeColus]: Xuan Bi, Bruno C. d. S. Oliveira, and Tom Schrijvers. [The Essence of Nested Composition](https://i.cs.hku.hk/~bruno/papers/nested.pdf). *ECOOP 2018*.  
[^CP]: Weixin Zhang, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Compositional Programming](https://i.cs.hku.hk/~bruno/papers/toplas2021.pdf). *TOPLAS 2021*.  
[^Fi+]: Andong Fan, Xuejing Huang, Han Xu, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Direct Foundations for Compositional Programming](https://i.cs.hku.hk/~bruno/papers/ecoop22direct_extended.pdf). *ECOOP 2022*.  
[^IsoRec]: Yaoda Zhou, Bruno C. d. S. Oliveira, and Andong Fan. [A Calculus with Recursive Types, Record Concatenation and Subtyping](https://i.cs.hku.hk/~bruno/papers/aplas22recursive.pdf). *APLAS 2022*.  
[^ExT]: Yaozhu Sun, Utkarsh Dhandhania, and Bruno C. d. S. Oliveira. [Compositional Embeddings of Domain-Specific Languages](https://i.cs.hku.hk/~bruno/papers/oopsla22extended.pdf). *OOPSLA 2022*.  
[^TypeDiff]: Han Xu, Xuejing Huang, and Bruno C. d. S. Oliveira. [Making a Type Difference](https://i.cs.hku.hk/~bruno/papers/popl23making.pdf). *POPL 2023*.  
[^Ref]: Wenjia Ye, Yaozhu Sun, and Bruno C. d. S. Oliveira. [Imperative Compositional Programming](https://i.cs.hku.hk/~bruno/papers/oopsla24_imperative.pdf). *OOPSLA 2024*.  
[^IU]: Yaozhu Sun and Bruno C. d. S. Oliveira. [Named Arguments as Intersections, Optional Arguments as Unions](https://i.cs.hku.hk/~bruno/papers/esop25named.pdf). *ESOP 2025*.  
[^Comp]: Yaozhu Sun, Xuejing Huang, and Bruno C. d. S. Oliveira. [Type-Safe Compilation of Dynamic Inheritance via Merging](https://i.cs.hku.hk/~bruno/papers/TOPLAS25.pdf). *TOPLAS 2025*.
