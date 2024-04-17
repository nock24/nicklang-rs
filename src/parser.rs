use crate::tokenizer::*;
use crate::arena::*;

use std::fmt;

#[allow(dead_code)]
const MB: usize = 1000 * 1000;

type Result<T> = std::result::Result<T, ParseError>;
pub type Scope = Vec<ArenaPtr<Stmt>>;

#[derive(Debug)]
pub enum ParseErrorType {
    Expected(String),
    MismatchedTypes,
    UndeclaredIdent(String),
    CannotMutateImmutableVar(String),
    IdentAlreadyUsed(String),
    UndeclaredFunc(String),
    CannotReturnInVoidFunc,
}
pub struct ParseError {
    type_: ParseErrorType,
    line_num: u32,
}
impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {:?} on line {}", self.type_, self.line_num)
    }
}
fn get_prec(token: TokenType) -> Option<(Op, u8)> {
    match token {
        TokenType::Op(Op::Plus) => Some((Op::Plus, 0)),
        TokenType::Op(Op::Minus) => Some((Op::Minus, 0)),
        TokenType::Op(Op::Star) => Some((Op::Star, 1)),
        TokenType::Op(Op::Slash) => Some((Op::Slash, 1)),
        _ => None
    } 
}
impl Type {
    pub fn is_null(&self) -> bool {
        match self {
            Type::Null => true,
            _ => false,
        }
    }
}
#[derive(Clone, Debug)]
pub struct VarData {
    pub ident: String,
    pub type_: Type,
    pub mutable: bool,
}
impl VarData {
    pub fn new() -> Self {
        Self {
            ident: String::new(),
            type_: Type::Int,
            mutable: false,
        }
    }
}
impl TokenType {
    pub fn is_inbuilt(&self) -> bool {
        match self {
            TokenType::Exit | TokenType::Printf => true,
            _ => false,
        }
    }
}
#[derive(Debug)]
pub struct Prog {
    pub defs: Vec<ArenaPtr<Def>>
}
#[derive(Debug, Clone)]
pub enum Def {
    Func(String, Vec<VarData>, Type, Scope),
    Struct(String, Vec<VarData>),
}
#[derive(Debug, Clone)]
pub enum Stmt {
    InBuilt(ArenaPtr<InBuilt>),
    Let(VarData, ArenaPtr<Expr>),
    Assign(String, ArenaPtr<Expr>),
    Scope(Scope),
    For(String, ArenaPtr<Expr>, ArenaPtr<Expr>, Scope),
    If(ArenaPtr<Expr>, Scope, Option<ArenaPtr<IfPred>>),
    Return(ArenaPtr<Expr>),
    VoidFuncCall(ArenaPtr<Expr>),
}
#[derive(Debug, Clone)]
pub enum InBuilt {
    Exit(ArenaPtr<Expr>),
    Printf(ArenaPtr<Expr>),
}
#[derive(Debug, Clone)]
pub enum IfPred {
    ElseIf(ArenaPtr<Expr>, Scope, Option<ArenaPtr<IfPred>>),
    Else(Scope),
}
#[derive(Debug, Clone)]
pub enum Expr {
    BinExpr(ArenaPtr<Expr>, Op, ArenaPtr<Expr>),
    Atom(ArenaPtr<Atom>),
    FuncCall(String, Vec<ArenaPtr<Expr>>),
}
#[derive(Debug, Clone)]
pub enum Atom {
    IntLit(i32),
    StrLit(String),
    Ident(String),
    Paren(ArenaPtr<Expr>),
}
pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    allocator: ArenaAllocator,
    vars: Vec<VarData>,
    defs: Vec<ArenaPtr<Def>>,
    cur_func_ret_type: Type,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens, 
            idx: 0,
            allocator: ArenaAllocator::new(1 * MB),
            vars: Vec::new(),
            defs: Vec::new(),
            cur_func_ret_type: Type::Null,
        } 
    }
    pub fn error(&self, error_type: ParseErrorType) -> ParseError {
        let line_num = match self.peek(0) {
            Some(token) => token.line_num,
            None => self.peek(-1).unwrap().line_num,
        };
        ParseError {type_: error_type, line_num}
    }
    pub fn expected(&self, str: &str) -> ParseError {
        self.error(ParseErrorType::Expected(str.to_string()))
    }
    pub fn parse_prog(&mut self) -> Result<Prog> {
        while self.peek(0).is_some() {
            let def = self.parse_def()?;
            self.defs.push(def);
        }
        let prog = Prog {defs: self.defs.clone()};
        Ok(prog)
    }
    fn parse_def(&mut self) -> Result<ArenaPtr<Def>> {
        if self.peek(0).is_none() {
            return Err(self.expected("struct or function definition"));
        }
        match self.consume() {
            TokenType::Func => {
                self.parse_func()   
            }
            TokenType::Struct => {
                self.parse_struct()
            }
            _ => Err(self.expected("struct of function defintion"))
        }
    }
    fn parse_struct(&mut self) -> Result<ArenaPtr<Def>> {
        let struct_ident;
        if self.peek(0).is_none() {
            return Err(self.expected("identifier"));
        }
        struct_ident = match self.consume() {
            TokenType::Ident(ident) => ident.to_string(),
            _ => return Err(self.expected("identifer"))
        };
        self.try_consume(&TokenType::OpenCurly)?;
        let mut members = Vec::new();
        while self.peek(0).is_some() && self.peek(0).unwrap().type_ != TokenType::CloseCurly {
            let mut member_data = VarData::new();
            member_data.mutable = false;
            member_data.ident = match self.consume() {
                TokenType::Ident(ident) => ident.to_string(),
                _ => return Err(self.expected("member"))
            };
            self.try_consume(&TokenType::Colon)?;
            if self.peek(0).is_none() {
                return Err(self.expected("member type"));
            }
            member_data.type_ = match self.consume() {
                TokenType::Type(type_) => type_,
                _ => return Err(self.expected("member type"))
            };
            members.push(member_data);
            if self.peek(0).is_some() && self.peek(0).unwrap().type_ == TokenType::CloseCurly {
                break;
            }
            self.try_consume(&TokenType::Comma)?;
        }
        self.consume();
        let def = self.allocator.alloc(Def::Struct(struct_ident, members));
        Ok(def)
    }
    fn parse_func(&mut self) -> Result<ArenaPtr<Def>> {
        let func_ident;
        if self.peek(0).is_none() {
            return Err(self.expected("identfier"));
        }
        func_ident = match self.consume() {
            TokenType::Ident(ident) => ident.to_string(),
            _ => return Err(self.expected("identfier")),
        };
        self.try_consume(&TokenType::OpenParen)?;
        let mut params = Vec::new();
        while self.peek(0).is_some() && self.peek(0).unwrap().type_ != TokenType::CloseParen {
            let mut param_data = VarData::new();
            param_data.mutable = true;
            param_data.ident = match self.consume() {
                TokenType::Ident(ident) => ident.to_string(),
                _ => return Err(self.expected("paramater"))
            };
            self.try_consume(&TokenType::Colon)?;
            if self.peek(0).is_none() {
                return Err(self.expected("variable type"));
            }
            param_data.type_ = match self.consume() {
                TokenType::Type(type_) => type_,
                _ => return Err(self.expected("variable type"))
            };
            params.push(param_data);
            if self.peek(0).is_some() && self.peek(0).unwrap().type_ == TokenType::CloseParen {
                break;
            }
            self.try_consume(&TokenType::Comma)?;
        }
        self.consume();
        let mut return_type = Type::Null;
        if self.try_consume(&TokenType::Arrow).is_ok() {
            return_type = match self.consume() {
                TokenType::Type(type_) => type_,
                _ => return Err(self.expected("return type"))
            };
        }
        self.vars.extend(params.clone());
        self.cur_func_ret_type = return_type.clone();
        let scope = self.parse_scope()?;
        let def = self.allocator.alloc(Def::Func(func_ident, params, return_type, scope));
        Ok(def)
    }
    fn parse_stmt(&mut self) -> Result<ArenaPtr<Stmt>> {
        if self.peek(0).is_some() {
            if self.peek(0).unwrap().type_.is_inbuilt() {
                let inbuilt_type = self.consume();
                let stmt = self.parse_inbuilt(&inbuilt_type)?;
                return Ok(stmt); 
            } 
            match self.peek(0).unwrap().type_ {
                TokenType::Let => {
                    self.consume();
                    let mut var_data = VarData::new();
                    if self.peek(0).is_some() {
                        if self.peek(0).unwrap().type_ == TokenType::Mut {
                            self.consume();
                            var_data.mutable = true;
                            if self.peek(0).is_none() {
                                return Err(self.expected("identifier"));
                            }
                        }
                        var_data.ident = match self.consume() {
                            TokenType::Ident(ident) => ident.to_string(),
                            _ => return Err(self.expected("identifier")),
                        };
                    } else {
                        return Err(self.expected("identifier"));
                    }
                    if self.vars.iter().find(|x| x.ident == var_data.ident).is_some() {
                        return Err(self.error(ParseErrorType::IdentAlreadyUsed(var_data.ident.clone())));
                    }
                    self.try_consume(&TokenType::Colon)?;
                    if self.peek(0).is_some() {
                        var_data.type_ = match self.consume() {
                            TokenType::Type(type_) => type_,
                            _ => return Err(self.expected("variable type")),
                        }
                    } else {
                        return Err(self.expected("variable type"));
                    }
                    self.try_consume(&TokenType::Eq)?;
                    let expr = self.parse_expr(Some(&var_data.type_), 0)?;
                    self.vars.push(var_data.clone());
                    let stmt = self.allocator.alloc(Stmt::Let(var_data, expr));
                    self.try_consume(&TokenType::SemiColon)?;
                    Ok(stmt)
                }
                TokenType::Ident(ident) => {
                    if self.peek(1).is_none() {
                        return Err(self.expected("var declaration or void func call"));
                    }
                    if self.peek(1).unwrap().type_ == TokenType::Eq {
                        self.consume();
                        self.consume();
                        let var = if let Some(var) = self.vars.iter().find(|x| x.ident == *ident) {
                            var.clone()
                        } else {
                            return Err(self.error(ParseErrorType::UndeclaredIdent(ident.clone()))); 
                        };
                        if !var.mutable {
                            return Err(self.error(ParseErrorType::CannotMutateImmutableVar(ident.clone())));
                        }
                        let expr = self.parse_expr(Some(&var.type_), 0)?;
                        let stmt = self.allocator.alloc(Stmt::Assign(ident.clone(), expr));
                        self.try_consume(&TokenType::SemiColon)?;
                        return Ok(stmt);
                    } 
                    if self.peek(1).unwrap().type_ == TokenType::OpenParen {
                        let expr = self.parse_expr(Some(&Type::Null), 0)?;
                        let stmt = self.allocator.alloc(Stmt::VoidFuncCall(expr));
                        self.try_consume(&TokenType::SemiColon)?;
                        return Ok(stmt);
                    } 
                    Err(self.expected("var declaration or void func call"))
                }
                TokenType::OpenCurly => {
                    let scope = self.parse_scope()?;
                    let stmt = self.allocator.alloc(Stmt::Scope(scope));
                    Ok(stmt)
                }
                TokenType::If => {
                    self.consume();
                    let expr = self.parse_expr(Some(&Type::Int), 0)?;
                    let scope = self.parse_scope()?;
                    let pred = self.parse_if_pred()?;
                    let stmt = self.allocator.alloc(Stmt::If(expr, scope, pred));
                    Ok(stmt)
                }
                TokenType::Return => {
                    self.consume();
                    if self.cur_func_ret_type.is_null() {
                        return Err(self.error(ParseErrorType::CannotReturnInVoidFunc));
                    }
                    let expr = self.parse_expr(Some(&self.cur_func_ret_type.clone()), 0)?;
                    self.try_consume(&TokenType::SemiColon)?;
                    let stmt = self.allocator.alloc(Stmt::Return(expr));
                    Ok(stmt)
                }
                TokenType::For => {
                    self.consume();
                    let idx_ident;
                    if self.peek(0).is_none() {
                        return Err(self.expected("index identifier"));
                    }
                    if let TokenType::Ident(ref ident) = self.consume() {
                        idx_ident = ident.clone();
                    } else {
                        return Err(self.expected("index identifier"));
                    };
                    
                    self.vars.push(VarData {
                        ident: idx_ident.clone(),
                        type_: Type::Int,
                        mutable: false,
                    });

                    self.try_consume(&TokenType::In)?;
                    let start = self.parse_expr(Some(&Type::Int), 0)?;
                    self.try_consume(&TokenType::Arrow)?;
                    let end = self.parse_expr(Some(&Type::Int), 0)?;
                    let scope = self.parse_scope()?;
                    let stmt = self.allocator.alloc(Stmt::For(idx_ident, start, end, scope));
                    Ok(stmt)
                }
                _ => Err(self.expected("statement"))
            }
        } else {
            Err(self.expected("statement"))
        }
    }
    fn parse_inbuilt(&mut self, inbuilt_type: &TokenType) -> Result<ArenaPtr<Stmt>> {
        match inbuilt_type {
            TokenType::Exit => {
                self.try_consume(&TokenType::OpenParen)?;
                let expr = self.parse_expr(Some(&Type::Int), 0)?;
                let inbuilt = self.allocator.alloc(InBuilt::Exit(expr));
                let stmt = self.allocator.alloc(Stmt::InBuilt(inbuilt));
                self.try_consume(&TokenType::CloseParen)?;
                self.try_consume(&TokenType::SemiColon)?;
                Ok(stmt)
            }
            TokenType::Printf => {
                self.try_consume(&TokenType::OpenParen)?;
                let fmt_str = self.parse_expr(Some(&Type::Str), 0)?;
                self.try_consume(&TokenType::CloseParen)?;
                self.try_consume(&TokenType::SemiColon)?;
                let inbuilt = self.allocator.alloc(InBuilt::Printf(fmt_str));
                let stmt = self.allocator.alloc(Stmt::InBuilt(inbuilt));
                Ok(stmt)
            }
            _ => Err(self.expected("inbuilt function"))
        }
    }
    fn parse_if_pred(&mut self) -> Result<Option<ArenaPtr<IfPred>>> {
        if self.try_consume(&TokenType::Else).is_ok() {
            if self.try_consume(&TokenType::If).is_ok() {
                let expr = self.parse_expr(Some(&Type::Int), 0)?;
                let scope = self.parse_scope()?;
                let pred = self.parse_if_pred()?;
                let elseif = self.allocator.alloc(IfPred::ElseIf(expr, scope, pred));
                return Ok(Some(elseif));
            }
            let scope = self.parse_scope()?;
            let else_ = self.allocator.alloc(IfPred::Else(scope));
            return Ok(Some(else_));
        }
        Ok(None)
    }
    fn parse_scope(&mut self) -> Result<Scope> {
        self.try_consume(&TokenType::OpenCurly)?;
        let mut scope = Scope::new();
        loop {
            let stmt = self.parse_stmt();
            if stmt.is_ok() {
                scope.push(stmt.unwrap());
            } else {
                if self.peek(0).is_none() || self.peek(0).unwrap().type_ != TokenType::CloseCurly {
                    return Err(stmt.err().unwrap());
                }
                self.consume();
                return Ok(scope);
            }
        }
    }
    fn parse_expr(&mut self, expected_type: Option<&Type>, min_prec: u8) -> Result<ArenaPtr<Expr>> {
        let atom = self.parse_atom()?;
        if self.peek(0).is_none() {
            return Err(self.expected("expression"));
        }
        if let Atom::StrLit(_) = *atom {
            if expected_type != Some(&Type::Str) {
                return Err(self.error(ParseErrorType::MismatchedTypes));
            }
            let expr = self.allocator.alloc(Expr::Atom(atom));
            return Ok(expr);
        }
        let mut lhs;
        if self.peek(0).unwrap().type_ == TokenType::OpenParen {
            self.consume();
            if let Atom::Ident(ref func_ident) = *atom {
                let mut param_types = Vec::new();
                let Some(func) = self.defs.iter().find(|&f| {
                    let Def::Func(ref ident, ref param_data, ..) = *f.clone() else {
                        return false;
                    };
                    param_types = param_data.iter().map(|x| x.type_.clone()).collect();
                    ident == func_ident
                }) else {
                    return Err(self.error(ParseErrorType::UndeclaredFunc(func_ident.clone())));
                };
                let Def::Func(.., ref ret_type, _) = *func.clone() else { todo!() };
                if Some(ret_type) != expected_type {
                    return Err(self.error(ParseErrorType::MismatchedTypes));
                }
                let mut params = Vec::new();
                let mut i = 0;
                while self.peek(0).is_some() && self.peek(0).unwrap().type_ != TokenType::CloseParen {
                    params.push(self.parse_expr(Some(&param_types[i]), 0)?);
                    if self.peek(0).is_some() && self.peek(0).unwrap().type_ == TokenType::CloseParen {
                        break;
                    }
                    self.try_consume(&TokenType::Comma)?;
                    i += 1;
                }
                self.consume(); 
                lhs = self.allocator.alloc(Expr::FuncCall(func_ident.to_string(), params));
                if ret_type.is_null() {
                    return Ok(lhs);
                }
            } else {
                return Err(self.expected("identifier"));
            }
        } else {
            lhs = self.allocator.alloc(Expr::Atom(atom));
        }
        loop {
            let cur_token = self.peek(0);
            if cur_token.is_none() {
                break;
            }
            let Some((operator, prec)) = get_prec(cur_token.unwrap().type_) else {
                break;
            };
            if prec < min_prec {
                break;
            } 
            self.consume();
            let next_min_prec = prec + 1;
            let rhs = self.parse_expr(expected_type, next_min_prec)?;
            let expr = self.allocator.alloc(Expr::BinExpr(lhs, operator.clone(), rhs));
            lhs = expr;
        }
        Ok(lhs)
    }
    fn parse_atom(&mut self) -> Result<ArenaPtr<Atom>> {
        if self.peek(0).is_none() {
            return Err(self.expected("atom"));
        }
        match self.consume() {
            TokenType::IntLit(int) => {
                let atom = self.allocator.alloc(Atom::IntLit(int));
                Ok(atom)
            }
            TokenType::StrLit(str) => {
                let atom = self.allocator.alloc(Atom::StrLit(str));
                Ok(atom)
            }
            TokenType::Ident(ident) => {
                let atom = self.allocator.alloc(Atom::Ident(ident.clone()));
                Ok(atom)
            }
            TokenType::OpenParen => {
                let expr = self.parse_expr(Some(&Type::Int), 0)?;
                self.try_consume(&TokenType::CloseParen)?;
                let atom = self.allocator.alloc(Atom::Paren(expr));
                Ok(atom)
            }
            _ => Err(self.expected("atom"))
        } 
    }
    fn peek(&self, offset: i32) -> Option<Box<Token>> {
        let idx = self.idx as i32 + offset;
        match self.tokens.get(idx as usize) {
            Some(token) => Some(Box::new(token.clone())),
            None => None,
        }
    }
    fn consume(&mut self) -> TokenType {
        self.idx += 1;
        self.tokens[self.idx - 1].type_.clone()
    }
    fn try_consume(&mut self, token_type: &TokenType) -> Result<()> {
        if self.peek(0).is_some() && self.peek(0).unwrap().type_ == *token_type {
            self.consume();
        } else {
            return Err(self.expected(match *token_type {
                TokenType::SemiColon => ";",
                TokenType::Eq => "=",
                TokenType::OpenParen => "(",
                TokenType::CloseParen => ")",
                TokenType::OpenCurly => "{",
                TokenType::CloseCurly => "}",
                TokenType::If => "if",
                TokenType::Else => "else",
                TokenType::Func => "fn",
                TokenType::Mut => "mut",
                TokenType::Type(_) => "type",
                TokenType::Colon => ":",
                _ => "n/a",
            }));
        }
        Ok(())
    }
}
