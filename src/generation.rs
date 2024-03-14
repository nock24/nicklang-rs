use crate::parser::*;
use crate::arena::*;
use crate::tokenizer::*;
use std::fmt;

const NONE: Option<u8> = None;

const RET_ADDR_SZ: usize = 8;

const SYS_EXIT: usize = 60;
const SYS_WRITE: usize = 1;

const STDOUT: usize = 1;

type Result = std::result::Result<(), GenerationError>;

#[derive(Debug)]
pub enum GenerationError {
    NoMainFunction,
    NotImplemented,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            &Type::Int => 8,    // 64 bit integer
            &Type::Str => 16,   // 64 bit pointer and 64 bit length
            &Type::Ptr => 8,    // 64 bit address
            &Type::Null => 0,
        }
    }
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
    cur_func: String,
}
impl Generator {
    pub fn new(prog: Prog) -> Generator {
        Generator {
            prog, 
            output: String::new(),
            section_data: String::new(),
            stack_sz: 0,
            vars: Vec::new(),
            scopes: Vec::new(),
            label_cnt: 0,
            str_cnt: 0,
            cur_func: "main".to_string(),
        }
    }
    pub fn gen_prog(&mut self) -> Result {
        self.output += "section .text\nglobal _start\n_start:\n";
        self.section_data += "section .data\n";

        let Some(main_func) = self.prog.defs.iter().find(|&f| {
            let Def::Func(ref ident, ..) = *f.clone() else {
                return false;
            };
            ident == "main"
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
        Ok(())
    }
    fn gen_def(&mut self, def: ArenaPtr<Def>) -> Result {
        match *def {
            Def::Func(ref func_ident, ref params, ref _return_type, ref scope) => {
                if func_ident != "main" {
                    self.stack_sz += RET_ADDR_SZ;
                }
                self.output += &format!("{func_ident}:\n");
                let mut offset = RET_ADDR_SZ;
                for param_data in params.iter() {
                    match param_data.type_ {
                        Type::Int => {
                            self.push(&format!("QWORD [rsp + {offset}]"));         // push 64 bit integer
                        }
                        Type::Str => {
                            self.push(&format!("QWORD [rsp + {}]", offset + Type::Ptr.size()));         // push 64 bit string address
                            self.push(&format!("QWORD [rsp + {}]", offset + Type::Ptr.size()));         // push 64 bit string length
                        }
                        _ => return Err(GenerationError::NotImplemented)
                    }
                    self.vars.push(Var {
                        ident: param_data.ident.clone(), 
                        type_: param_data.type_.clone(),
                        mutable: param_data.mutable,
                        stack_loc: self.stack_sz - param_data.type_.size()
                    });
                    offset += 2 * param_data.type_.size();
                }
                self.gen_scope(scope.clone(), false)?;
                self.output += &format!("{}_ret:\n", self.cur_func);
                self.end_scope();
                if func_ident != "main" {
                    let params_size: usize = params.iter().map(|x| x.type_.size()).sum();
                    self.directive("add", "rsp", Some(params_size));
                    self.stack_sz -= params_size;
                    self.stack_sz -= RET_ADDR_SZ;
                }
                if func_ident != "main" {
                    self.output += "    ret\n";
                }
            }
        }
        Ok(())
    }
    fn gen_stmt(&mut self, stmt: ArenaPtr<Stmt>) -> Result {
        match *stmt {
            Stmt::InBuilt(ref func_call) => {
                self.gen_inbuilt_stmt(func_call.clone())?;
            }
            Stmt::Let(ref var_data, ref value) => {
                self.gen_let_stmt(var_data.clone(), value.clone())?;
            }
            Stmt::Assign(ref ident, ref value) => {
                self.gen_assign_stmt(ident.clone(), value.clone())?;
            }
            Stmt::Scope(ref scope) => {
                self.gen_scope(scope.to_vec(), true)?;
            }
            Stmt::For(ref idx_ident, ref start, ref end, ref scope) => {
                self.gen_for_loop(idx_ident.clone(), start.clone(), end.clone(), scope.clone())?;
            }
            Stmt::If(ref condition, ref scope, ref if_pred) => {
                self.gen_if_stmt(condition.clone(), scope.clone(), if_pred.clone())?;
            }
            Stmt::Return(ref value) => {
                self.gen_ret_stmt(value.clone())?;
            }
            Stmt::VoidFuncCall(ref func_call) => {
                self.gen_expr(func_call.clone())?;   
            }
        }
        Ok(())
    }
    fn gen_let_stmt(&mut self, var_data: VarData, value: ArenaPtr<Expr>) -> Result {
        self.gen_expr(value)?;
        self.vars.push(Var {
            ident: var_data.ident, 
            type_: var_data.type_.clone(),
            mutable: var_data.mutable,
            stack_loc: self.stack_sz - var_data.type_.size()
        });
        Ok(())
    }
    fn gen_assign_stmt(&mut self, ident: String, value: ArenaPtr<Expr>) -> Result {
        let var = self.vars.iter().find(|x| x.ident == *ident).unwrap().clone();
        self.gen_expr(value)?;
        match var.type_ {
            Type::Int => {
                let offset = self.stack_sz - var.stack_loc - Type::Int.size();
                self.pop("rax");
                self.directive("mov", &format!("[rsp + {offset}]"), Some("rax"));
            }
            Type::Str => {
                let offset = self.stack_sz - var.stack_loc - Type::Int.size();
                self.pop("rbx");      // pop off 64 bit string length
                self.pop("rax");      // pop off 64 bit string address
                self.directive("mov", &format!("[rsp + {offset}]"), Some("rax"));
                self.directive("mov", &format!("[rsp + {offset}]"), Some("rbx"));
            }
            _ => return Err(GenerationError::NotImplemented)
        }
        Ok(())
    }
    fn gen_for_loop(&mut self, idx_ident: String, start: ArenaPtr<Expr>, end: ArenaPtr<Expr>, scope: Scope) -> Result {
        self.gen_expr(start)?;
        let idx_loc = self.stack_sz - Type::Int.size();
        self.vars.push(Var {
            ident: idx_ident,
            type_: Type::Int,
            mutable: false,
            stack_loc: idx_loc,
        });
        
        self.gen_expr(end)?;
        let end_val_loc = self.stack_sz - Type::Int.size(); 

        let label = self.create_label();
        self.output += &format!("{label}:\n");

        self.gen_scope(scope, true)?;

        let idx_offset = self.stack_sz - idx_loc - Type::Int.size();
        self.directive("inc", &format!("QWORD [rsp + {idx_offset}]"), NONE);
        self.directive("mov", "rax", Some(&format!("[rsp + {idx_offset}]")));

        let end_val_offset = self.stack_sz - end_val_loc - Type::Int.size();
        self.directive("mov", "rbx", Some(&format!("[rsp + {end_val_offset}]")));

        self.directive("cmp", "rax", Some("rbx"));
        self.directive("jnz", &label, NONE);
        Ok(())
    }
    fn gen_if_stmt(&mut self, condition: ArenaPtr<Expr>, scope: Scope, if_pred: Option<ArenaPtr<IfPred>>) -> Result {
        self.gen_expr(condition)?;
        self.pop("rax");
        let label = self.create_label();
        self.directive("test", "rax", Some("rax"));
        self.directive("jz", &label, NONE);
        self.gen_scope(scope, true)?;
        if if_pred.is_some() {
            let end_label = self.create_label();
            self.directive("jmp", &end_label, NONE);
            self.output += &format!("{label}:\n");
            self.gen_if_pred(if_pred.unwrap(), &end_label)?;
            self.output += &format!("{end_label}:\n");
        } else {
            self.output += &format!("{label}:\n");
        }
        Ok(())
    }
    fn gen_ret_stmt(&mut self, value: ArenaPtr<Expr>) -> Result {
        self.gen_expr(value)?;
        let func = self.prog.defs.iter().find(|&f| {
            let Def::Func(ref ident, ..) = *f.clone() else {
                return false;
            };
            ident == &self.cur_func
        }).unwrap();
        let Def::Func(.., ref ret_type, _) = *func.clone();
        match ret_type {
            &Type::Int => self.pop("rax"),
            &Type::Str => {
                self.pop("rbx");
                self.pop("rax");
            }
            _ => return Err(GenerationError::NotImplemented)
        }
        let ret_label = &format!("{}_ret", self.cur_func);
        self.directive("jmp", ret_label, NONE);
        Ok(()) 
    }
    fn gen_inbuilt_stmt(&mut self, func_call: ArenaPtr<InBuilt>) -> Result {
        match *func_call {
            InBuilt::Exit(ref expr) => {   
                self.gen_expr(expr.clone())?;
                self.pop("rdi");
                self.directive("mov", "rax", Some(SYS_EXIT));
                self.output += "    syscall\n";
            }
            InBuilt::Printf(ref expr) => {
                self.gen_expr(expr.clone())?;
                self.pop("rdx");
                self.pop("rsi");
                self.directive("mov", "rax", Some(SYS_WRITE));
                self.directive("mov", "rdi", Some(STDOUT));
                self.output += "    syscall\n";
            }
        }
        Ok(())
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
        Ok(())
    }
    fn gen_scope(&mut self, scope: Scope, end_scope: bool) -> Result {
        self.begin_scope();
        for stmt in scope.iter() {
            self.gen_stmt(stmt.clone())?;
        }
        if end_scope {
            self.end_scope();
        }
        Ok(())
    }
    fn gen_expr(&mut self, expr: ArenaPtr<Expr>) -> Result {
        match *expr {
            Expr::Atom(ref atom) => {
                self.gen_atom(atom.clone())?
            }
            Expr::BinExpr(ref lhs, ref operator, ref rhs) => {
                self.gen_bin_expr(lhs.clone(), operator, rhs.clone())?;
            }
            Expr::FuncCall(ref func_ident, ref params) => {
                self.gen_func_call_expr(func_ident, params.clone())?;
            }
        }    
        Ok(())
    }
    fn gen_bin_expr(&mut self, lhs: ArenaPtr<Expr>, operator: &Op, rhs: ArenaPtr<Expr>) -> Result {
        match operator {
            Op::Plus => {
                 self.gen_expr(lhs)?;
                 self.gen_expr(rhs)?;
                 self.pop("rbx");
                 self.pop("rax");
                 self.directive("add", "rax", Some("rbx"));
                 self.push("rax");
            }
            Op::Minus => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.pop("rbx");
                self.pop("rax");
                self.directive("sub", "rax", Some("rbx"));
                self.push("rax");
            }
            Op::Star => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.pop("rbx");
                self.pop("rax");
                self.directive("mul", "rbx", NONE);
                self.push("rax");
            }
            Op::Slash => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                self.pop("rbx");
                self.pop("rax");
                self.directive("div", "rbx", NONE);
            }
        } 
        Ok(())
    }
    fn gen_func_call_expr(&mut self, func_ident: &String, params: Vec<ArenaPtr<Expr>>) -> Result {
        for param in params.into_iter().rev() {
            self.gen_expr(param)?;
        }
        let Def::Func(_, ref param_data, ref ret_type, _) = *self.prog.defs.iter().find(|&f| {
            let Def::Func(ref ident, ..) = *f.clone() else {
                return false;
            };
            ident == func_ident
        }).unwrap().clone();
        let params_size: usize = param_data.iter().map(|x| x.type_.size()).sum();
        self.directive("call", func_ident, NONE);
        self.directive("add", "rsp", Some(params_size));
        self.stack_sz -= params_size;
        if !ret_type.is_null() {
            match ret_type {
                Type::Int => self.push("rax"),
                Type::Str => {
                    self.push("rax");
                    self.push("rbx");
                }
                _ => return Err(GenerationError::NotImplemented)
            }
        }
        Ok(())
    }
    fn gen_atom(&mut self, atom: ArenaPtr<Atom>) -> Result {
        match *atom {
            Atom::Ident(ref ident) => {
                let var = self.vars.iter().find(|x| x.ident == *ident).unwrap();
                match var.type_ {
                    Type::Int => {
                        let offset = self.stack_sz - var.stack_loc - Type::Int.size();
                        self.push(&format!("QWORD [rsp + {offset}]"));        // push 64 bit integer
                    }
                    Type::Str => {
                        let offset = self.stack_sz - var.stack_loc - Type::Int.size();
                        self.push(&format!("QWORD [rsp + {offset}]"));        // push 64 bit address
                        self.push(&format!("QWORD [rsp + {offset}]"));        // push 64 bit length
                    }
                    _ => return Err(GenerationError::NotImplemented)
                }
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
                self.push(&format!("QWORD {}", str.len()));
            }
        }
        Ok(())
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
        format!("label{}", self.label_cnt)
    }
    fn create_string(&mut self) -> String {
        self.str_cnt += 1;
        format!("str{}", self.str_cnt)
    }
    fn directive<T: fmt::Display>(&mut self, directive: &str, reg: &str, input: Option<T>) {
        if let Some(input) = input {
            self.output += &format!("    {directive} {reg}, {input}\n");
        } else {
            self.output += &format!("    {directive} {reg}\n");
        }
    }
    fn define_str(&mut self, ident: &str, input: &str) {
        let mut output = format!("    {ident} db \"");
        let mut i = 0;
        while i < input.len() {
            if input.chars().nth(i).unwrap() == '\\' && input.chars().nth(i + 1).unwrap() == 'n' {
                output += "\", 10, \"";
                i += 1;
            } else {
                output += &input.chars().nth(i).unwrap().to_string();
            }
            i += 1;
        }
        output += "\", 0\n";
        self.section_data += &output;
    }
    fn push(&mut self, item: &str) {
        self.output += &format!("    push {item}\n");
        self.stack_sz += 8;
    }
    fn pop(&mut self, reg: &str) {
        self.output += &format!("    pop {reg}\n");
        if reg.chars().nth(0).unwrap() == 'r' {
            self.stack_sz -= 8;
        } else {
            self.stack_sz -= 4;
        }
    }
}
