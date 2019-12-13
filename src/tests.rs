use crate::borrow_checker;
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

is_one;
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

is_one;
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

#[test]
fn test_type_check_borrow() {
    let module = rust_grammar::ModuleParser::new().parse("
let x: i32 = 0;
let y: &i32 = &x;
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
}

#[test]
fn test_type_check_dereference() {
    let module = rust_grammar::ModuleParser::new().parse("
let x: i32 = 0;
let y: &i32 = &x;
let z: i32 = *y;
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
}

#[test]
fn test_borrow_check_read_borrowed() {
    let module = rust_grammar::ModuleParser::new().parse("
let x: i32 = 0;
let y: &i32 = &x;
x;
*y;
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
    assert!(borrow_checker::check(&module).is_ok());
}

#[test]
fn test_borrow_check_write_to_shared_borrowed() {
    let module = rust_grammar::ModuleParser::new().parse("
let mut x: i32 = 0;
let y: &i32 = &x;
x = 1;
*y;
        ").unwrap();
    assert!(type_checker::check(&module).is_ok());
    assert!(borrow_checker::check(&module).is_err());
}

#[test]
fn test_borrow_check_write_to_mutable_borrowed() {
    let module = rust_grammar::ModuleParser::new().parse("
let mut x: i32 = 0;
let y: &mut i32 = &mut x;
*y = 1;
        ").unwrap();
    assert_eq!(type_checker::check(&module), Ok(()));
    assert_eq!(borrow_checker::check(&module), Ok(()));
}

#[test]
fn test_borrow_check_write_to_mutable_borrowed_with_dynamic_source() {
    let module = rust_grammar::ModuleParser::new().parse("
let mut x: i32 = 0;
let mut y: i32 = 1;
let r: &mut i32 = match 0 {
    0 => &mut x,
    n => &mut y,
};
*r = 1;
        ").unwrap();
    assert_eq!(type_checker::check(&module), Ok(()));
    assert_eq!(borrow_checker::check(&module), Ok(()));
}
