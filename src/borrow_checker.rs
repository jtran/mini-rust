use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

use crate::ast::*;
use crate::type_checker::Path;

type Identifier = String;

pub fn check(module: &Module) -> Result<(), BorrowError> {
    let mut checker = BorrowChecker::new();

    checker.check(module)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BorrowError {
    pub causes: Vec<BorrowErrorCause>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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
                self.check_expression(e)?;

                match **t {
                    Type::RefPtr(m, _) => {
                        let loan = self.ctx.new_loan();
                        let path = Path::from(id);
                        let borrow_type = BorrowType::from(m);
                        self.ctx.add_loan(loan, path.clone(), LoanState::Borrowed, borrow_type);
                        eprintln!("Adding active borrows path={:?}, loan={:?}", path, loan);
                        self.ctx.add_active_borrow(path, &[loan]);
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

                let path = path_from_exp(e1);
                match self.ctx.conflicts(&path, Operation::Write) {
                    None => (),
                    Some((loan_state, borrow_type)) => {
                        return Err(BorrowErrorCause::new(&format!("Can't write to {} while it is {} {}", path, borrow_type.to_pretty_string(), loan_state.to_pretty_string())));
                    }
                }

                Ok(loans)
            }
            Expr::Binary(e1, _op, e2) => {
                let mut loans = self.check_expression(e1)?;
                loans.extend(self.check_expression(e2)?);

                Ok(loans)
            }
            Expr::AddressOf(m, e) => {
                self.check_expression(e)?;

                let loan = self.ctx.new_loan();
                let path = path_from_exp(e);
                let borrow_type = BorrowType::from(*m);
                self.ctx.add_loan(loan, path.clone(), LoanState::LentOut, borrow_type);

                eprintln!("Adding active borrows path={:?}, loans={:?}", path, loan);
                self.ctx.add_active_borrow(path, &[loan]);

                Ok(vec![loan])
            }
            Expr::Deref(e) => {
                self.check_expression(e)?;
                let path = path_from_exp(expression);
                match self.ctx.conflicts(&path, Operation::Read) {
                    None => (),
                    Some((loan_state, borrow_type)) => {
                        return Err(BorrowErrorCause::new(&format!("Can't read {} while it is {} {}", path, borrow_type.to_pretty_string(), loan_state.to_pretty_string())));
                    }
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
            Expr::Variable(_) => {
                let path = path_from_exp(expression);
                match self.ctx.conflicts(&path, Operation::Read) {
                    None => (),
                    Some((loan_state, borrow_type)) => {
                        return Err(BorrowErrorCause::new(&format!("Can't read {} while it is {} {}", path, borrow_type.to_pretty_string(), loan_state.to_pretty_string())));
                    }
                }

                Ok(Vec::new())
            }
        }
    }
}

fn path_from_exp(e: &Expr) -> Path {
    Path::try_from(e).unwrap_or_else(|msg| panic!("{}", msg))
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

impl BorrowType {
    pub fn to_pretty_string(self) -> String {
        match self {
            BorrowType::Shared => "immutably".to_string(),
            BorrowType::Mutable => "mutably".to_string(),
        }
    }
}

impl From<RefPtrKind> for BorrowType {
    fn from(ref_kind: RefPtrKind) -> BorrowType {
        match ref_kind {
            RefPtrKind::Shared => BorrowType::Shared,
            RefPtrKind::Mutable => BorrowType::Mutable,
        }
    }
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

impl LoanState {
    pub fn to_pretty_string(self) -> String {
        match self {
            LoanState::LentOut => "lent out".to_string(),
            LoanState::Borrowed => "borrowed".to_string(),
        }
    }
}

impl From<&Identifier> for Path {
    fn from(id: &Identifier) -> Path {
        Path::Ident(id.to_string())
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

    pub fn conflicts(&self, path: &Path, op: Operation) -> Option<(LoanState, BorrowType)> {
        eprintln!("Checking for conflict path={:?}, op={:?}", path, op);
        match self.active_borrows.get(path) {
            None => None,
            Some(region) => {
                eprintln!("  Found region with {} loans", region.loans().len());
                for loan in region.loans() {
                    match self.loans.get(loan) {
                        None => panic!("Couldn't find loan in context loans: {:?}", loan),
                        Some((path, loan_state, borrow_type)) => {
                            eprintln!("  Found active borrow: path={:?}, loan_state={:?}, borrow_type={:?}, op={:?}", path, loan_state, borrow_type, op);
                            if op_conflicts(op, *loan_state, *borrow_type) {
                                eprintln!("  Conflict");
                                return Some((*loan_state, *borrow_type));
                            }
                        }
                    }
                }
                None
            }
        }
    }
}

fn op_conflicts(op: Operation, loan_state: LoanState, borrow_type: BorrowType) -> bool {
    !matches!((op, loan_state, borrow_type),
              (Operation::Read, LoanState::Borrowed, BorrowType::Shared)
              | (Operation::Read, LoanState::Borrowed, BorrowType::Mutable)
              | (Operation::Write, LoanState::Borrowed, BorrowType::Mutable)
              | (Operation::Read, LoanState::LentOut, BorrowType::Shared))
}
