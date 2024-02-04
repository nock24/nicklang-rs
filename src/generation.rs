use crate::parser::*;
use crate::arena::*;
use crate::tokenizer::*;
use std::collections::HashMap;
use std::fmt;

const NONE: Option<u8> = None;

const RET_ADDR_SZ: usize = 8;

const SYS_EXIT: usize = 60;
const SYS_WRITE: usize = 1;

const STDOUT: usize = 1;

type Result = std::result::Result<(), GenerationError>;

#[derive(Debug)]
pub enum GenerationError {
    IdentAlredyUsed(String),
    UndeclaredIdent(String),
    NoMainFunction,
    CannotMutateImmutableVar(String),
}

#[derive(PartialEq, Clone, Debug)]
struct Var {
    ident: String,
    type_: Type,
    mutable: bool,
    stack_loc: usize,
}
pub struct Generator {
    prog: Prog,
    pub output: String,
    section_data: String,
    stack_sz: usize,
    vars: Vec<Var>,
    scopes: Vec<usize>,
    label_cnt: u64,
    str_cnt: u64,
    str_lengths: HashMap<String, usize>,
    cur_func: String,
}
impl Generator {
    pub fn new(prog: Prog) -> Generator {
        return Generator {
            prog, 
            output: String::new(),
            section_data: String::new(),
            stack_sz: 0,
            vars: Vec::new(),
            scopes: Vec::new(),
            label_cnt: 0,
            str_cnt: 0,
            str_lengths: HashMap::new(),
            cur_func: "main".to_string(),
        }; 
    }
    pub fn gen_prog(&mut self) -> Result {
        self.output += "global _start\n_start:\n";
        self.section_data += "section .data\n";

        let Some(main_func) = self.prog.defs.iter().find(|&f| {
            let Def::Func(ref ident, ..) = *f.clone() else {
                return false;
            };
            return ident == "main";
        }) else {
            return Err(GenerationError::NoMainFunction);
        };
        self.gen_def(main_func.clone())?;

        self.directive("mov", "rdi", Some(0));
        self.directive("mov", "rax", Some(60));
        self.output += "    syscall\n";

        for def in self.prog.defs.clone() {
            let Def::Func(ref func_ident, ..) = *def;
            if func_ident != "main" {
                self.cur_func = func_ident.clone();
                self.gen_def(def)?;
            } 
        }
        self.output = self.section_data.clone() + &self.output;
        return Ok(());
    }
    fn gen_def(&mut self, def: ArenaPtr<Def>) -> Result {
        match *def {
            Def::Func(ref func_ident, ref params, ref _return_type, ref scope) => {
                if func_ident != "main" {
                    self.stack_sz += RET_ADDR_SZ;
                }
                self.output += &format!("{func_ident}:\n");
                for (i, param_data) in params.iter().enumerate() {
                    let offset = i * 2 * 8 + RET_ADDR_SZ;   
                    self.push(&format!("QWORD [rsp + {offset}]"));
                    self.vars.push(Var {
                        ident: param_data.ident.clone(), 
                        type_: param_data.type_.clone(),
                        mutable: param_data.mutable,
                        stack_loc: self.stack_sz - 8
                    });
                }
                self.gen_scope(scope.clone(), false)?;
                self.output += &format!("{}_ret:\n", self.cur_func);
                self.end_scope();
                if func_ident != "main" {
                    let params_sz = params.len() * 8;
                    self.directive("add", "rsp", Some(params_sz));
                    self.stack_sz -= params_sz;
                    self.stack_sz -= RET_ADDR_SZ;
                }
                if func_ident != "main" {
                    self.output += "    ret\n";
                }
            }
        }
        return Ok(());
    }
    fn gen_stmt(&mut self, stmt: ArenaPtr<Stmt>) -> Result {
        match *stmt {
            Stmt::InBuilt(ref inbuilt) => {
                self.gen_inbuilt(inbuilt.clone())?;
            }
            Stmt::Let(ref var_data, ref expr) => {
                if self.vars.iter().find(|x| x.ident == *var_data.ident).is_some() {
                    return Err(GenerationError::IdentAlredyUsed(var_data.ident.clone()));
                }
                self.gen_expr(expr.clone())?;
                self.vars.push(Var {
                    ident: var_data.ident.clone(), 
                    type_: var_data.type_.clone(),
                    mutable: var_data.mutable,
                    stack_loc: self.stack_sz - 8
                });
            }
            Stmt::Assign(ref ident, ref expr) => {
                let var = if let Some(var) = self.vars.iter().find(|x| x.ident == *ident) {
                    var.clone()
                } else {
                    return Err(GenerationError::UndeclaredIdent(ident.clone()));
                };
                if !var.mutable {
                    return Err(GenerationError::CannotMutateImmutableVar(var.ident.clone()));
                }
                self.gen_expr(expr.clone())?;
                self.pop("rax");
                let offset = self.stack_sz - var.stack_loc - 8;
                self.directive("mov", &format!("[rsp + {offset}]"), Some("rax"));
            }
            Stmt::Scope(ref scope) => {
                self.gen_scope(scope.to_vec(), true)?;
            }
            Stmt::If(ref expr, ref scope, ref pred) => {
                self.gen_expr(expr.clone())?;
                self.pop("rax");
                let label = self.create_label();
                self.directive("test", "rax", Some("rax"));
                self.directive("jz", &label, NONE);
                self.gen_scope(scope.to_vec(), true)?;
                if pred.is_some() {
                    let end_label = self.create_label();
                    self.directive("jmp", &end_label, NONE);
                    self.output += &format!("{label}:\n");
                    self.gen_if_pred(pred.clone().unwrap(), &end_label)?;
                    self.output += &format!("{end_label}:\n");
                } else {
                    self.output += &format!("{label}:\n");
                }
            }
            Stmt::Return(ref expr) => {
                self.gen_expr(expr.clone())?;
                self.pop("rax");
                let ret_label = &format!("{}_ret", self.cur_func);
                self.directive("jmp", ret_label, NONE);
            }
        }
        return Ok(());
    }
    fn gen_inbuilt(&mut self, func_call: ArenaPtr<InBuilt>) -> Result {
        match *func_call {
            InBuilt::Exit(ref expr) => {   
                self.gen_expr(expr.clone())?;
                self.pop("rdi");
                self.directive("mov", "rax", Some(SYS_EXIT));
                self.output += "    syscall\n";
            }
            InBuilt::Print(ref expr) => {
                self.gen_expr(expr.clone())?;
                self.pop("rsi");
                self.directive("mov", "rax", Some(SYS_WRITE));
                self.directive("mov", "rdi", Some(STDOUT));
                self.directive("mov", "rdx", Some(6));
                self.output += "    syscall\n";
            }
        }
        return Ok(());
    }
    fn gen_if_pred(&mut self, if_pred: ArenaPtr<IfPred>, end_label: &str) -> Result {
        match *if_pred {
            IfPred::ElseIf(ref expr, ref scope, ref pred) => {
                self.gen_expr(expr.clone())?;
                self.pop("rax");
                let label = self.create_label();
                self.directive("test", "rax", Some("rax"));
                self.directive("jz", &label, NONE);
                self.gen_scope(scope.to_vec(), true)?;
                self.directive("jmp", end_label, NONE);
                self.output += &format!("{label}:\n");
                if pred.is_some() {
                    self.gen_if_pred(pred.clone().unwrap(), end_label)?;
                }
            },
            IfPred::Else(ref scope) => {
                self.gen_scope(scope.to_vec(), true)?;
            }
        }
        return Ok(());
    }
    fn gen_scope(&mut self, scope: Scope, end_scope: bool) -> Result {
        self.begin_scope();
        for stmt in scope.iter() {
            self.gen_stmt(stmt.clone())?;
        }
        if end_scope {
            self.end_scope();
        }
        return Ok(());
    }
    fn gen_expr(&mut self, expr: ArenaPtr<Expr>) -> Result {
        match *expr {
            Expr::Atom(ref atom) => self.gen_atom(atom.clone())?,
            Expr::BinExpr(ref lhs, ref operator, ref rhs) => {
                match operator {
                    Op::Plus => {
                         self.gen_expr(lhs.clone())?;
                         self.gen_expr(rhs.clone())?;
                         self.pop("rbx");
                         self.pop("rax");
                         self.directive("add", "rax", Some("rbx"));
                         self.push("rax");
                    },
                    Op::Minus => {
                        self.gen_expr(lhs.clone())?;
                        self.gen_expr(rhs.clone())?;
                        self.pop("rbx");
                        self.pop("rax");
                        self.directive("sub", "rax", Some("rbx"));
                        self.push("rax");
                    }
                    Op::Star => {
                        self.gen_expr(lhs.clone())?;
                        self.gen_expr(rhs.clone())?;
                        self.pop("rbx");
                        self.pop("rax");
                        self.directive("mul", "rbx", NONE);
                        self.push("rax");
                    },
                    Op::Slash => {
                        self.gen_expr(lhs.clone())?;
                        self.gen_expr(rhs.clone())?;
                        self.pop("rbx");
                        self.pop("rax");
                        self.directive("div", "rbx", NONE);
                    }
                } 
            }
            Expr::FuncCall(ref func_ident, ref params) => {
                for param in params.iter().rev() {
                    self.gen_expr(param.clone())?;
                }
                self.directive("call", func_ident, NONE);
                let params_sz = params.len() * 8;
                self.directive("add", "rsp", Some(params_sz));
                self.stack_sz -= params_sz;
                self.push("rax");
            }
        }    
        return Ok(());
    }
    fn gen_atom(&mut self, atom: ArenaPtr<Atom>) -> Result {
        match *atom {
            Atom::Ident(ref ident) => {
                let var = self.vars.iter().find(|x| x.ident == *ident);
                if var.is_none() {
                    return Err(GenerationError::UndeclaredIdent(ident.clone()));
                }
                let offset = self.stack_sz - var.unwrap().stack_loc - 8;
                self.push(&format!("QWORD [rsp + {offset}]"));
            }
            Atom::IntLit(int) => {
                self.directive("mov", "rax", Some(int));
                self.push("rax");
            }
            Atom::Paren(ref expr) => {
                self.gen_expr(expr.clone())?; 
            }
            Atom::StrLit(ref str) => {
                let str_ident = self.create_string();
                self.define_str(&str_ident, str);
                self.push(&str_ident);
            }
        }
        return Ok(());
    }
    fn begin_scope(&mut self) {
        self.scopes.push(self.vars.len());
    }
    fn end_scope(&mut self) {
        let pop_cnt = self.vars.len() - self.scopes.last().unwrap();
        self.directive("add", "rsp", Some(pop_cnt * 8));
        self.stack_sz -= pop_cnt * 8;
        for _ in 0..pop_cnt {
            self.vars.pop();
        }
    }
    fn create_label(&mut self) -> String {
        self.label_cnt += 1;
        return format!("label{}", self.label_cnt);
    }
    fn create_string(&mut self) -> String {
        self.str_cnt += 1;
        return format!("str{}", self.str_cnt);
    }
    fn directive<T: fmt::Display>(&mut self, directive: &str, reg: &str, input: Option<T>) {
        if let Some(input) = input {
            self.output += &format!("    {directive} {reg}, {input}\n");
        } else {
            self.output += &format!("    {directive} {reg}\n");
        }
    }
    fn define_str(&mut self, ident: &str, input: &str) {
        self.section_data += &format!("    {ident} db \"{input}\", 0\n");
    }
    fn push(&mut self, reg: &str) {
        self.output += &format!("    push {reg}\n");
        self.stack_sz += 8;
    }
    fn pop(&mut self, reg: &str) {
        self.output += &format!("    pop {reg}\n");
        self.stack_sz -= 8;
    }
}
