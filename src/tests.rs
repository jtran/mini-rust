use crate::rust_grammar;
use crate::type_checker;

#[test]
fn test_parse() {
    assert!(rust_grammar::ModuleParser::new().parse("
enum Size {
    Zero,
    One,
    Many,
}

let x: i32 = 0 + 1;

let size: Size = match x {
    0 => Zero,
    1 => One,
    n => Many,
};

let is_one: bool = match size {
    One => true,
    x => false,
};

is_one
        ").is_ok());
}

#[test]
fn test_type_check() {
    let module = rust_grammar::ModuleParser::new().parse("
let mut x: i32 = 0;
x = x + 1;
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
    let module = rust_grammar::ModuleParser::new().parse("
let mut x: i32 = 0;
x = true;
        ").unwrap();
    assert!(type_checker::check(&module).is_err());
    let module = rust_grammar::ModuleParser::new().parse("
let x: i32 = 0;
x = 1;
        ").unwrap();
    assert!(type_checker::check(&module).is_err());

    let module = rust_grammar::ModuleParser::new().parse("
enum Size {
    Zero,
    One,
    Many,
}

let x: i32 = 0 + 1;

let size: Size = match x {
    0 => Zero,
    1 => One,
    n => Many,
};

let is_one: bool = match size {
    One => true,
    x => false,
};

is_one
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
    let module = rust_grammar::ModuleParser::new().parse("
enum Size {
    Zero,
    One,
    Many,
}

let size: Size = match 1 {
    0 => Zero,
    1 => One,
    n => Many,
};

let is_one: bool = match size {
    Size => true,
    x => false,
};
        ").unwrap();
    assert!(type_checker::check(&module).is_err());
    let module = rust_grammar::ModuleParser::new().parse("
enum Bool {
    T,
    F,
}

let truthy: Bool = match 0 {
    0 => F,
    n => T,
};
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
    let module = rust_grammar::ModuleParser::new().parse("
enum Bool {
    T,
    F,
}

let truthy: bool = match 0 {
    0 => F,
    n => T,
};
        ").unwrap();
    assert!(type_checker::check(&module).is_err());
    let module = rust_grammar::ModuleParser::new().parse("
enum Bool {
    T,
    F,
}

let truthy: bool = match 0 {
    0 => 0,
    n => T,
};
        ").unwrap();
    assert!(type_checker::check(&module).is_err());
    let module = rust_grammar::ModuleParser::new().parse("
let truthy: bool = match true {
    0 => true,
    n => false,
};
        ").unwrap();
    assert!(type_checker::check(&module).is_err());
}
