use std::collections::HashMap;
use std::mem;

use crate::ast::*;

pub fn check(module: &Module) -> Result<(), TypeError> {
    let mut checker = TypeChecker::new();

    checker.check(&module)
}

pub struct TypeError {
    pub causes: Vec<TypeErrorCause>,
}

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
            Stmt::Let(id, t, e) => {
                let lhs_type = self.eval_type(t)?;
                let rhs_type_result = self.check_expression(e);

                self.ctx.define(id.to_string(), lhs_type.clone());

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
                            let ctor_type = self.ctx.lookup(&ctor_id).ok_or_else(|| TypeErrorCause::new(&format!("Unknown constructor in pattern: {}", ctor_id)))?;
                            if *ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Constructor pattern {} of type {:?} does not match condition type {:?}", ctor_id, ctor_type, t)));
                            }
                        }
                        Pattern::Binding(id) => {
                            // New binding.
                            let mut new_ctx = self.ctx.clone();
                            new_ctx.define(id.to_string(), t.clone());
                            ctx_to_drop = Some(new_ctx);
                            if found_catch_all {
                                return Err(TypeErrorCause::new(&format!("Match branches should never have more than one catch-all pattern: {}", id)));
                            }
                            found_catch_all = true;
                        }
                        Pattern::LiteralBool(_) => {
                            let ctor_type = Type::Bool;
                            if ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Pattern literal of type {:?} does not match condition type {:?}", ctor_type, t)));
                            }
                        }
                        Pattern::LiteralInt(_) => {
                            let ctor_type = Type::Int;
                            if ctor_type != t {
                                return Err(TypeErrorCause::new(&format!("Pattern literal of type {:?} does not match condition type {:?}", ctor_type, t)));
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
                        // Check that all branches are the same.
                        for arm_type in arm_types.iter() {
                            if *arm_type != *t {
                                return Err(TypeErrorCause::new(&format!("Match arm result type {:?} does not match previous branch type {:?}", arm_type, *t)))
                            }
                        }

                        Ok(t.clone())
                    }
                    None => Err(TypeErrorCause::new("Match has no arm branches")),
                }
            }
            Expr::LiteralInt(_) => Ok(Type::Int),
            Expr::LiteralBool(_) => Ok(Type::Bool),
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

    // Evaluating types isn't so involved right now.  But if we ever add
    // higher-kinded types -- for example, generic data types -- we will
    // absolutely need it.
    fn eval_type(&mut self, typ: &Type) -> Result<Type, TypeErrorCause> {
        match typ {
            Type::Bool => Ok(Type::Bool),
            Type::Int => Ok(Type::Int),
            Type::NamedType(id) => Ok(Type::NamedType(id.to_string())),
            Type::Variable(id) => self.ctx.lookup_type(id).cloned().ok_or_else(|| TypeErrorCause::new(&format!("Unknown type: {}", id))),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Context {
    exprs: HashMap<String, Type>,
    types: HashMap<String, Type>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            exprs: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn define(&mut self, id: String, typ: Type) {
        self.exprs.insert(id, typ);
    }

    pub fn define_type(&mut self, id: String, typ: Type) {
        self.types.insert(id, typ);
    }

    pub fn lookup(&self, id: &str) -> Option<&Type> {
        self.exprs.get(id)
    }

    pub fn lookup_type(&self, id: &str) -> Option<&Type> {
        self.types.get(id)
    }
}
