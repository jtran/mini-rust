type Identifier = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub statements: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Enum(Identifier, Vec<EnumConstructor>),
    Let(Identifier, Box<Type>, Box<Expr>),
    Expression(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Variable(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumConstructor {
    pub name: Identifier,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
    Match(Box<Expr>, Vec<MatchArm>),
    LiteralInt(i32),
    LiteralBool(bool),
    Variable(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Constructor(Identifier),
    Binding(Identifier),
    LiteralBool(bool),
    LiteralInt(i32),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    DividedBy,

    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}