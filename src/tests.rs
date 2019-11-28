use crate::rust_grammar;

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
