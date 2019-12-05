use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::ast::*;

type Identifier = String;

pub fn check(module: &Module) -> Result<(), BorrowError> {
    let mut checker = BorrowChecker::new();

    checker.check(&module)
}

pub struct BorrowError {
    pub causes: Vec<BorrowErrorCause>,
}

pub struct BorrowErrorCause {
    pub message: String,
}

impl BorrowErrorCause {
    pub fn new(msg: &str) -> BorrowErrorCause {
        BorrowErrorCause { message: msg.to_string() }
    }
}

pub struct BorrowChecker {
    ctx: Context,
}

impl BorrowChecker {
    pub fn new() -> BorrowChecker {
        BorrowChecker {
            ctx: Context::new(),
        }
    }

    pub fn check(&mut self, module: &Module) -> Result<(), BorrowError> {
        let mut causes = Vec::new();
        for stmt in module.statements.iter() {
            if let Err(cause) = self.check_statement(stmt) {
                causes.push(cause);
            }
        }

        if causes.is_empty() {
            Ok(())
        } else {
            Err(BorrowError { causes })
        }
    }

    fn check_statement(&mut self, statement: &Stmt) -> Result<(), BorrowErrorCause> {
        match statement {
            Stmt::Enum(_, _) => Ok(()),
            Stmt::Let(_, id, t, e) => {
                let loans = self.check_expression(e)?;

                match **t {
                    Type::RefPtr(_) => {
                        let loan = self.ctx.new_loan();
                        let path = Path::from(id);
                        self.ctx.add_loan(loan, path.clone(), LoanState::Borrowed, BorrowType::Shared);
                        eprintln!("Adding active borrows path={:?}, loans={:?}", path, loans);
                        self.ctx.add_active_borrow(path.clone(), &vec![loan]);
                        self.ctx.add_active_borrow(path.clone(), &loans);
                    }
                    _ => (),
                }

                Ok(())
            }
            Stmt::Expression(e) => self.check_expression(e).map(|_| ()),
        }
    }

    fn check_expression(&mut self, expression: &Expr) -> Result<Vec<Loan>, BorrowErrorCause> {
        match expression {
            Expr::Assignment(e1, e2) => {
                let mut loans = self.check_expression(e1)?;
                loans.extend(self.check_expression(e2)?);

                let path = self.path_from_exp(e1);
                if self.ctx.conflicts(&path, Operation::Write) {
                    return Err(BorrowErrorCause::new(&format!("Can't write to {} while it is borrowed", path)));
                }

                Ok(loans)
            }
            Expr::Binary(e1, _op, e2) => {
                let mut loans = self.check_expression(e1)?;
                loans.extend(self.check_expression(e2)?);

                Ok(loans)
            }
            Expr::AddressOf(e) => {
                self.check_expression(e)?;

                let loan = self.ctx.new_loan();
                let path = self.path_from_exp(e);
                self.ctx.add_loan(loan, path.clone(), LoanState::LentOut, BorrowType::Shared);

                eprintln!("Adding active borrows path={:?}, loans={:?}", path, loan);
                self.ctx.add_active_borrow(path, &vec![loan]);

                Ok(vec![loan])
            }
            Expr::Deref(e) => {
                self.check_expression(e)?;
                let path = self.path_from_exp(e);
                if self.ctx.conflicts(&path, Operation::Read) {
                    return Err(BorrowErrorCause::new(&format!("Can't read {} while it is borrowed", path)));
                }

                Ok(Vec::new())
            }
            Expr::Grouping(e) => self.check_expression(e),
            Expr::Match(e, arms) => {
                let mut loans = self.check_expression(e)?;
                for arm in arms {
                    loans.extend(self.check_expression(&arm.body)?);
                }

                Ok(loans)
            }
            Expr::LiteralInt(_) => Ok(Vec::new()),
            Expr::LiteralBool(_) => Ok(Vec::new()),
            Expr::Tuple0 => Ok(Vec::new()),
            Expr::Variable(id) => {
                let path = self.path_from_exp(expression);
                if self.ctx.conflicts(&path, Operation::Read) {
                    return Err(BorrowErrorCause::new(&format!("Can't read {} while it is borrowed", id)));
                }

                Ok(Vec::new())
            }
        }
    }

    fn path_from_exp(&self, exp: &Expr) -> Path {
        match exp {
            Expr::Variable(id) => Path::new(id.to_string()),
            _ => panic!("Couldn't create a path from expression: {:?}", exp),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Operation {
    Read,
    Write,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BorrowType {
    Shared,
    Mutable,
}

#[derive(Clone, Debug, PartialEq)]
struct Region(HashSet<Loan>);

impl Region {
    pub fn new() -> Region {
        Region(HashSet::new())
    }

    pub fn loans(&self) -> &HashSet<Loan> {
        &self.0
    }

    pub fn add(&mut self, loan: Loan) -> bool {
        self.0.insert(loan)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LoanState {
    LentOut,
    Borrowed,
}

// For now, a path can only be a single identifier.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct Path(Identifier);

impl Path {
    pub fn new(id: Identifier) -> Path {
        Path(id)
    }
}

impl From<&Identifier> for Path {
    fn from(id: &Identifier) -> Path {
        Path::new(id.to_string())
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct Loan {
    pub id: u32,
}

#[derive(Clone, Debug, PartialEq)]
struct Context {
    loans: HashMap<Loan, (Path, LoanState, BorrowType)>,
    active_borrows: HashMap<Path, Region>,
    num_loans: u32,
}

impl Context {
    pub fn new() -> Context {
        Context {
            loans: HashMap::new(),
            active_borrows: HashMap::new(),
            num_loans: 0,
        }
    }

    pub fn new_loan(&mut self) -> Loan {
        let loan = Loan { id: self.num_loans };
        self.num_loans += 1;

        loan
    }

    pub fn add_loan(&mut self, loan: Loan,
                    path: Path, loan_state: LoanState, typ: BorrowType) {
        self.loans.insert(loan, (path, loan_state, typ));
    }

    pub fn add_active_borrow(&mut self, path: Path, loans: &[Loan]) {
        self.active_borrows.entry(path)
            .and_modify(|region| {
                for loan in loans {
                    region.add(*loan);
                }
            })
            .or_insert_with(|| {
                let mut region = Region::new();
                for loan in loans {
                    region.add(*loan);
                }
                region
            });
    }

    pub fn conflicts(&self, path: &Path, op: Operation) -> bool {
        eprintln!("Checking for conflict path={:?}, op={:?}", path, op);
        match self.active_borrows.get(path) {
            None => false,
            Some(region) => {
                eprintln!("  Found region with {} loans", region.loans().len());
                for loan in region.loans() {
                    match self.loans.get(loan) {
                        None => panic!("Couldn't find loan in context loans: {:?}", loan),
                        Some((path, loan_state, borrow_type)) => {
                            eprintln!("  Found active borrow: path={:?}, loan_state={:?}, borrow_type={:?}, op={:?}", path, loan_state, borrow_type, op);
                            if op_conflicts(op, *loan_state, *borrow_type) {
                                eprintln!("  Conflict");
                                return true;
                            }
                        }
                    }
                }
                false
            }
        }
    }
}

fn op_conflicts(op: Operation, loan_state: LoanState, borrow_type: BorrowType) -> bool {
    match (op, loan_state, borrow_type) {
        (Operation::Read, LoanState::Borrowed, BorrowType::Shared)
        | (Operation::Read, LoanState::Borrowed, BorrowType::Mutable)
        | (Operation::Write, LoanState::Borrowed, BorrowType::Mutable)
        | (Operation::Read, LoanState::LentOut, BorrowType::Shared)
        => false,
        _ => true,
    }
}
