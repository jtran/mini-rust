# Mini-Rust

This is an attempt to implement a borrow-checker for a tiny subset of Rust.

This is an extremely rough work-in-progress.  I will probably force push and
re-write all commits.

No compilation actually happens.  This is just parsing, type-checking, and
borrow-checking.

#### Current Status

This works for the test cases, but a lot is missing and you should assume it's
wrong in the general case.

Supported:

- shared and mutable references
- primitive types `i32`, `bool`, and `()`
- `enum` with constant constructors
- `match` statements with constant or binding patterns

Not yet implemented:

- Drops - things currently never go out of scope
- Moves - everything is `Copy`
- Functions
- `enum`s with constructor parameters (like in `Some(x)`) - they're essentially functions
- Comments - needs a custom lexer
- Showing and explaining the source of errors - needs a custom lexer
- Generics and type parameters - may need to implement part of this so that functions can use lifetimes
- Attributes - ideally implement the `Copy` attribute so that the default can be move

Out of scope:

- Tuples, arrays, and other primitive types
- `struct`s
- Traits
- Closures
- Async/Await
- Macros
- Modules
