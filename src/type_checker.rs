use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::mem;
use std::ops::Deref;

use crate::ast::*;

pub fn check(module: &Module) -> Result<(), TypeError> {
    let mut checker = TypeChecker::new();

    checker.check(module)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeError {
    pub causes: Vec<TypeErrorCause>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeErrorCause {
    pub message: String,
}

impl TypeErrorCause {
    pub fn new(msg: &str) -> TypeErrorCause {
        TypeErrorCause { message: msg.to_string() }
    }
}

pub struct TypeChecker {
    ctx: Context,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            ctx: Context::new(),
        }
    }

    pub fn check(&mut self, module: &Module) -> Result<(), TypeError> {
        let mut causes = Vec::new();
        for stmt in module.statements.iter() {
            if let Err(cause) = self.check_statement(stmt) {
                causes.push(cause);
            }
        }

        if causes.is_empty() {
            Ok(())
        } else {
            Err(TypeError { causes })
        }
    }

    fn check_statement(&mut self, statement: &Stmt) -> Result<(), TypeErrorCause> {
        match statement {
            Stmt::Enum(id, ctors) => {
                // TODO: save this definition.
                let t = Type::NamedType(id.to_string());
                self.ctx.define_type(id.to_string(), t.clone());
                for ctor in ctors {
                    self.ctx.define(ctor.name.to_string(), t.clone());
                }
                Ok(())
            }
            Stmt::Let(bind_mod, id, t, e) => {
                let lhs_type = self.eval_type(t)?;
                let rhs_type_result = self.check_expression(e);

                self.ctx.define(id.to_string(), lhs_type.clone());
                match bind_mod {
                    BindingModifier::Plain => (),
                    BindingModifier::Mutable => {
                        let path = Path::Ident(id.to_string());
                        self.ctx.define_mutable(path, lhs_type.clone())
                    }
                }

                match self.mutable_path_and_type_from_type(id.to_string(), &lhs_type) {
                    None => (),
                    Some((BindingModifier::Plain, _, _)) => (), // Not mutable.
                    Some((BindingModifier::Mutable, path, t_mutable)) => {
                        self.ctx.define_mutable(path, t_mutable);
                    }
                }

                // Check the result after defining so that if there was a type
                // error, the identifier will still be defined for later code
                // using the expected type.
                let rhs_type = rhs_type_result?;

                if lhs_type != rhs_type {
                    return Err(TypeErrorCause::new(&format!("Let definition for \"{}\" expected type {:?} but the right hand side value has type {:?}", id, lhs_type, rhs_type)));
                }

                Ok(())
            }
            Stmt::Expression(e) => self.check_expression(e).map(|_| ()),
        }
    }

    fn check_expression(&mut self, expression: &Expr, ) -> Result<Type, TypeErrorCause> {
        match expression {
            Expr::Assignment(e1, e2) => {
                self.check_expression(e1)?;
                let t2 = self.check_expression(e2)?;

                // TODO: This should allow any place expression.  It should
                // also check that it's mutable.
                let path = Path::try_from(&**e1).map_err(|_| TypeErrorCause::new(&format!("Expected variable or place expression on left hand side of assignment, but found: {:?}", e1)))?;

                match self.ctx.lookup_mutable(&path) {
                    Some(t_expected) => {
                        if *t_expected != t2 {
                            return Err(TypeErrorCause::new(&format!("Left hand side of assignment expects type {:?} but right hand side has type {:?}", t_expected, t2)));
                        }
                    }
                    None => return Err(TypeErrorCause::new(&format!("Expected mutable place expression on left hand side of assignment: {}", path))),
                }

                Ok(Type::Unit)
            }
            Expr::Binary(e1, op, e2) => {
                let t1 = self.check_expression(e1)?;
                let t2 = self.check_expression(e2)?;
                match op {
                    BinaryOperator::Plus
                    | BinaryOperator::Minus
                    | BinaryOperator::Times
                    | BinaryOperator::DividedBy => {
                        match (&t1, &t2) {
                            (Type::Int, Type::Int) => Ok(Type::Int),
                            _ => Err(TypeErrorCause::new(&format!("Math binary operator expected two ints, but found {:?} on the left and {:?} on the right", t1, t2))),
                        }
                    }
                    BinaryOperator::Equal
                    | BinaryOperator::NotEqual
                    | BinaryOperator::Less
                    | BinaryOperator::LessOrEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterOrEqual => {
                        match (&t1, &t2) {
                            (Type::Int, Type::Int) => Ok(Type::Bool),
                            _ => Err(TypeErrorCause::new(&format!("Math comparison operator expected two ints, but found {:?} on the left and {:?} on the right", t1, t2))),
                        }
                    }
                }
            }
            Expr::AddressOf(m, e) => {
                let t = self.check_expression(e)?;

                if self.is_place_expression(e) {
                    Ok(Type::RefPtr(*m, Box::new(t)))
                } else {
                    Err(TypeErrorCause::new(&format!("Expected place expression (like a local variable) after the borrow operator \"&\" but found: {:?}", e)))
                }
            }
            Expr::Deref(e) => {
                let t = self.check_expression(e)?;

                if self.is_place_expression(e) {
                    match t {
                        Type::RefPtr(_, t_inner) => Ok(*t_inner),
                        _ => Err(TypeErrorCause::new(&format!("Dereference operator \"*\" expected a reference but found: {:?}", t))),
                    }
                } else {
                    Err(TypeErrorCause::new(&format!("Expected place expression (like a local variable) after the dereference operator \"*\" but found: {:?}", e)))
                }
            }
            Expr::Grouping(e) => self.check_expression(e),
            Expr::Match(e, arms) => {
                let t = self.check_expression(e)?;
                let mut arm_types = Vec::with_capacity(arms.len());
                let mut found_catch_all = false;
                for arm in arms {
                    // Check t goes with arm patterns.
                    let mut ctx_to_drop = None;
                    match &arm.pattern {
                        Pattern::Constructor(ctor_id) => {
                            let ctor_type = self.ctx.lookup(ctor_id).ok_or_else(|| TypeErrorCause::new(&format!("Unknown constructor in pattern: {}", ctor_id)))?;
                            if *ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Constructor pattern {} of type {:?} does not match discriminant type {:?}", ctor_id, ctor_type, t)));
                            }
                        }
                        Pattern::Binding(id) => {
                            // New binding.
                            let mut new_ctx = self.ctx.clone();
                            new_ctx.define(id.to_string(), t.clone());
                            ctx_to_drop = Some(new_ctx);
                            if found_catch_all {
                                return Err(TypeErrorCause::new(&format!("Match arms should never have more than one catch-all pattern: {}", id)));
                            }
                            found_catch_all = true;
                        }
                        Pattern::LiteralBool(_) => {
                            let ctor_type = Type::Bool;
                            if ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Pattern literal of type {:?} does not match discriminant type {:?}", ctor_type, t)));
                            }
                        }
                        Pattern::LiteralInt(_) => {
                            let ctor_type = Type::Int;
                            if ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Pattern literal of type {:?} does not match discriminant type {:?}", ctor_type, t)));
                            }
                        }
                    };

                    // Check body.
                    let arm_typ = match ctx_to_drop {
                        None => self.check_expression(&arm.body)?,
                        Some(new_ctx) => {
                            // New context.
                            self.check_expression_in_ctx(&arm.body, new_ctx)?
                        }
                    };
                    arm_types.push(arm_typ);
                }

                match arm_types.first() {
                    Some(t) => {
                        // Check that all arms are the same.
                        for arm_type in arm_types.iter() {
                            if *arm_type != *t {
                                return Err(TypeErrorCause::new(&format!("Match arm result type {:?} does not match previous arm type {:?}", arm_type, *t)))
                            }
                        }

                        Ok(t.clone())
                    }
                    None => Err(TypeErrorCause::new("Match has no arms")),
                }
            }
            Expr::LiteralInt(_) => Ok(Type::Int),
            Expr::LiteralBool(_) => Ok(Type::Bool),
            Expr::Tuple0 => Ok(Type::Unit),
            Expr::Variable(id) => {
                match self.ctx.lookup(id) {
                    Some(t) => Ok(t.clone()),
                    None => Err(TypeErrorCause::new(&format!("Unknown identifier {}", id))),
                }
            }
        }
    }

    fn check_expression_in_ctx(&mut self, expression: &Expr, ctx: Context) -> Result<Type, TypeErrorCause> {
        let old_ctx = mem::replace(&mut self.ctx, ctx);

        let result = self.check_expression(expression);

        self.ctx = old_ctx;

        result
    }

    // A place expression is an expression that represents a memory location.
    //
    // See https://doc.rust-lang.org/reference/expressions.html#place-expressions-and-value-expressions
    fn is_place_expression(&self, expression: &Expr) -> bool {
        use Expr::*;
        match expression {
            Grouping(e) => self.is_place_expression(e),
            Assignment(_, _)
            | Binary(_, _, _)
            | AddressOf(_, _)
            | Match(_, _)
            | LiteralInt(_)
            | LiteralBool(_)
            | Tuple0 => false,
            Variable(_) | Deref(_) => true,
        }
    }

    // Say you have the following code:
    //
    //    let x: &mut i32 = ...;
    //
    // We want to return the fact that the path *x is a mutable path and that
    // its type is i32.
    fn mutable_path_and_type_from_type(&self, id: Identifier, t: &Type) -> Option<(BindingModifier, Path, Type)> {
        match t {
            Type::Bool => Some((BindingModifier::Plain, Path::Ident(id), Type::Bool)),
            Type::Int => Some((BindingModifier::Plain, Path::Ident(id), Type::Int)),
            Type::NamedType(tid) => Some((BindingModifier::Plain, Path::Ident(id), Type::NamedType(tid.to_string()))),
            Type::RefPtr(m, t_sub) => {
                match self.mutable_path_and_type_from_type(id, t_sub) {
                    None => None,
                    Some((_, path, mut_type)) => {
                        let path = Path::Deref(Box::new(path));
                        let modifier = match m {
                            RefPtrKind::Shared => BindingModifier::Plain,
                            RefPtrKind::Mutable => BindingModifier::Mutable,
                        };

                        Some((modifier, path, mut_type))
                    }
                }
            }
            Type::Unit => Some((BindingModifier::Plain, Path::Ident(id), Type::Unit)),
            Type::Variable(_) => panic!("mutable_path_and_type_from_type: found variable {:?}", t),
        }
    }

    // Evaluating types isn't so involved right now.  But if we ever add
    // higher-kinded types -- for example, generic data types -- we will
    // absolutely need it.
    fn eval_type(&mut self, typ: &Type) -> Result<Type, TypeErrorCause> {
        match typ {
            Type::Bool => Ok(Type::Bool),
            Type::Int => Ok(Type::Int),
            Type::NamedType(id) => Ok(Type::NamedType(id.to_string())),
            Type::RefPtr(m, t) => {
                let t_evaled = self.eval_type(t)?;
                Ok(Type::RefPtr(*m, Box::new(t_evaled)))
            }
            Type::Unit => Ok(Type::Unit),
            Type::Variable(id) => self.ctx.lookup_type(id).cloned().ok_or_else(|| TypeErrorCause::new(&format!("Unknown type: {}", id))),
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Path {
    Ident(Identifier),
    Deref(Box<Path>),
}

impl TryFrom<&Expr> for Path {
    type Error = String;

    fn try_from(exp: &Expr) -> Result<Self, Self::Error> {
        match exp {
            Expr::Deref(e) => Ok(Path::Deref(Box::new(Path::try_from(e.deref())?))),
            Expr::Grouping(e) => Path::try_from(e.deref()),
            Expr::Variable(id) => Ok(Path::Ident(id.to_string())),
            _ => Err(format!("Couldn't create a path from expression: {:?}", exp)),
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Path::Ident(id) => write!(f, "{}", id),
            Path::Deref(path) => write!(f, "*{}", path),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Context {
    exprs: HashMap<String, Type>,
    types: HashMap<String, Type>,
    mutables: HashMap<Path, Type>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            exprs: HashMap::new(),
            types: HashMap::new(),
            mutables: HashMap::new(),
        }
    }

    pub fn define(&mut self, id: String, typ: Type) {
        self.exprs.insert(id, typ);
    }

    pub fn define_type(&mut self, id: String, typ: Type) {
        self.types.insert(id, typ);
    }

    pub fn define_mutable(&mut self, path: Path, typ: Type) {
        self.mutables.insert(path, typ);
    }

    pub fn lookup(&self, id: &str) -> Option<&Type> {
        self.exprs.get(id)
    }

    pub fn lookup_type(&self, id: &str) -> Option<&Type> {
        self.types.get(id)
    }

    pub fn lookup_mutable(&self, path: &Path) -> Option<&Type> {
        self.mutables.get(path)
    }
}
