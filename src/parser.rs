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
    return match token {
        TokenType::Op(Op::Plus) => Some((Op::Plus, 0)),
        TokenType::Op(Op::Minus) => Some((Op::Minus, 0)),
        TokenType::Op(Op::Star) => Some((Op::Star, 1)),
        TokenType::Op(Op::Slash) => Some((Op::Slash, 1)),
        _ => None
    } 
}
#[derive(Clone, Debug)]
pub struct VarData {
    pub ident: String,
    pub type_: Type,
    pub mutable: bool,
}
impl VarData {
    pub fn new() -> VarData {
        return VarData {
            ident: String::new(),
            type_: Type::Int,
            mutable: false,
        };
    }
}
impl TokenType {
    pub fn is_inbuilt(&self) -> bool {
        return match self {
            TokenType::Exit | TokenType::Print => true,
            _ => false,
        };
    }
}
#[derive(Debug)]
pub struct Prog {
    pub defs: Vec<ArenaPtr<Def>>
}
#[derive(Debug, Clone)]
pub enum Def {
    Func(String, Vec<VarData>, Option<Type>, Scope),
}
#[derive(Debug, Clone)]
pub enum Stmt {
    InBuilt(ArenaPtr<InBuilt>),
    Let(VarData, ArenaPtr<Expr>),
    Assign(String, ArenaPtr<Expr>),
    Scope(Scope),
    If(ArenaPtr<Expr>, Scope, Option<ArenaPtr<IfPred>>),
    Return(ArenaPtr<Expr>),
}
#[derive(Debug, Clone)]
pub enum InBuilt {
    Exit(ArenaPtr<Expr>),
    Print(ArenaPtr<Expr>),
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
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens, 
            idx: 0,
            allocator: ArenaAllocator::new(1 * MB),
            vars: Vec::new(),
        }; 
    }
    pub fn error(&self, error_type: ParseErrorType) -> ParseError {
        let line_num = match self.peek(0) {
            Some(token) => token.line_num,
            None => self.peek(-1).unwrap().line_num,
        };
        return ParseError {type_: error_type, line_num};
    }
    pub fn parse_prog(&mut self) -> Result<Prog> {
        let mut prog = Prog {defs: Vec::new()};
        while self.peek(0).is_some() {
            let def = self.parse_def()?;
            prog.defs.push(def); 
        }
        return Ok(prog);
    }
    fn parse_def(&mut self) -> Result<ArenaPtr<Def>> {
        self.try_consume(&TokenType::Func)?;
        let func_ident;
        if self.peek(0).is_some() {
            func_ident = match self.consume() {
                TokenType::Ident(ident) => ident.to_string(),
                _ => return Err(self.error(ParseErrorType::Expected("identfier".to_string()))),
            };
        } else {
            return Err(self.error(ParseErrorType::Expected("identfier".to_string())));
        }
        self.try_consume(&TokenType::OpenParen)?;
        let mut params = Vec::new();
        while self.peek(0).is_some() && self.peek(0).unwrap().type_ != TokenType::CloseParen {
            let mut param_data = VarData::new();
            param_data.mutable = true;
            param_data.ident = match self.consume() {
                TokenType::Ident(ident) => ident.to_string(),
                _ => return Err(self.error(ParseErrorType::Expected("paramater".to_string())))
            };
            self.try_consume(&TokenType::Colon)?;
            if self.peek(0).is_some() {
                param_data.type_ = match self.consume() {
                    TokenType::Type(type_) => type_,
                    _ => return Err(self.error(ParseErrorType::Expected("variable type".to_string())))
                }
            } else {
                return Err(self.error(ParseErrorType::Expected("variable type".to_string())));
            }
            params.push(param_data);
            if self.peek(0).is_some() && self.peek(0).unwrap().type_ == TokenType::CloseParen {
                break;
            }
            self.try_consume(&TokenType::Comma)?;
        }
        self.consume();
        let mut return_type = None;
        if self.try_consume(&TokenType::Arrow).is_ok() {
            return_type = match self.consume() {
                TokenType::Type(type_) => Some(type_),
                _ => return Err(self.error(ParseErrorType::Expected("return type".to_string())))
            };
        }
        self.vars.extend(params.clone());
        let scope = self.parse_scope()?;
        let def = self.allocator.alloc(Def::Func(func_ident, params, return_type, scope));
        return Ok(def);
    }
    fn parse_stmt(&mut self) -> Result<ArenaPtr<Stmt>> {
        if self.peek(0).is_some() {
            if self.peek(0).unwrap().type_.is_inbuilt() {
                let inbuilt_type = self.consume();
                let stmt = self.parse_inbuilt(&inbuilt_type)?;
                return Ok(stmt); 
            } 
            return match self.peek(0).unwrap().type_ {
                TokenType::Let => {
                    self.consume();
                    let mut var_data = VarData::new();
                    if self.peek(0).is_some() {
                        if self.peek(0).unwrap().type_ == TokenType::Mut {
                            self.consume();
                            var_data.mutable = true;
                            if self.peek(0).is_none() {
                                return Err(self.error(ParseErrorType::Expected("identifier".to_string())));
                            }
                        }
                        var_data.ident = match self.consume() {
                            TokenType::Ident(ident) => ident.to_string(),
                            _ => return Err(self.error(ParseErrorType::Expected("identifier".to_string()))),
                        };
                    } else {
                        return Err(self.error(ParseErrorType::Expected("identifier".to_string())));
                    }
                    if self.vars.iter().find(|x| x.ident == var_data.ident).is_some() {
                        return Err(self.error(ParseErrorType::IdentAlreadyUsed(var_data.ident.clone())));
                    }
                    self.try_consume(&TokenType::Colon)?;
                    if self.peek(0).is_some() {
                        var_data.type_ = match self.consume() {
                            TokenType::Type(type_) => type_,
                            _ => return Err(self.error(ParseErrorType::Expected("variable type".to_string()))),
                        }
                    } else {
                        return Err(self.error(ParseErrorType::Expected("variable type".to_string())));
                    }
                    self.try_consume(&TokenType::Eq)?;
                    let expr = self.parse_expr(&var_data.type_, 0)?;
                    /*
                    let mismatched_types = match var_data.type_ {
                        Type::Int => {
                            if let Expr::Atom(ref atom) = *expr {
                                match *atom.clone() {
                                    Atom::StrLit(_) => true,
                                    _ => false,
                                }
                            } else {
                                true
                            }
                        } 
                        Type::Str => {
                            if let Expr::Atom(ref atom) = *expr {
                                match *atom.clone() {
                                    Atom::StrLit(_) => false,
                                    _ => true,
                                }
                            } else {
                                true
                            }
                        }
                    };
                    if mismatched_types {
                        return Err(self.error(ParseErrorType::MismatchedTypes));
                    }
                    */
                    self.vars.push(var_data.clone());
                    let stmt = self.allocator.alloc(Stmt::Let(var_data, expr));
                    self.try_consume(&TokenType::SemiColon)?;
                    Ok(stmt)
                }
                TokenType::Ident(ident) => {
                    self.consume();
                    self.try_consume(&TokenType::Eq)?;
                    let var = if let Some(var) = self.vars.iter().find(|x| x.ident == *ident) {
                        var.clone()
                    } else {
                        return Err(self.error(ParseErrorType::UndeclaredIdent(ident.clone()))); 
                    };
                    if !var.mutable {
                        return Err(self.error(ParseErrorType::CannotMutateImmutableVar(ident.clone())));
                    }
                    let expr = self.parse_expr(&var.type_, 0)?;
                    let stmt = self.allocator.alloc(Stmt::Assign(ident.clone(), expr));
                    self.try_consume(&TokenType::SemiColon)?;
                    return Ok(stmt);
                }
                TokenType::OpenCurly => {
                    let scope = self.parse_scope()?;
                    let stmt = self.allocator.alloc(Stmt::Scope(scope));
                    Ok(stmt)
                }
                TokenType::If => {
                    self.consume();
                    let expr = self.parse_expr(&Type::Int, 0)?;
                    let scope = self.parse_scope()?;
                    let pred = self.parse_if_pred()?;
                    let stmt = self.allocator.alloc(Stmt::If(expr, scope, pred));
                    Ok(stmt)
                }
                TokenType::Return => {
                    self.consume();
                    let expr = self.parse_expr(&Type::Int, 0)?;
                    self.try_consume(&TokenType::SemiColon)?;
                    let stmt = self.allocator.alloc(Stmt::Return(expr));
                    Ok(stmt)
                }
                _ => {
                    Err(self.error(ParseErrorType::Expected("statement".to_string())))
                }
            }
        } else {
            return Err(self.error(ParseErrorType::Expected("statement".to_string())));
        }
    }
    fn parse_inbuilt(&mut self, inbuilt_type: &TokenType) -> Result<ArenaPtr<Stmt>> {
        return match inbuilt_type {
            TokenType::Exit => {
                self.try_consume(&TokenType::OpenParen)?;
                let expr = self.parse_expr(&Type::Int, 0)?;
                let inbuilt = self.allocator.alloc(InBuilt::Exit(expr));
                let stmt = self.allocator.alloc(Stmt::InBuilt(inbuilt));
                self.try_consume(&TokenType::CloseParen)?;
                self.try_consume(&TokenType::SemiColon)?;
                Ok(stmt)
            }
            TokenType::Print => {
                self.try_consume(&TokenType::OpenParen)?;
                let expr = self.parse_expr(&Type::Int, 0)?;
                let inbuilt = self.allocator.alloc(InBuilt::Print(expr));
                let stmt = self.allocator.alloc(Stmt::InBuilt(inbuilt));
                self.try_consume(&TokenType::CloseParen)?;
                self.try_consume(&TokenType::SemiColon)?;
                Ok(stmt)
            }
            _ => Err(self.error(ParseErrorType::Expected("inbuilt function".to_string())))
        }
    }
    fn parse_if_pred(&mut self) -> Result<Option<ArenaPtr<IfPred>>> {
        if self.try_consume(&TokenType::Else).is_ok() {
            if self.try_consume(&TokenType::If).is_ok() {
                let expr = self.parse_expr(&Type::Int, 0)?;
                let scope = self.parse_scope()?;
                let pred = self.parse_if_pred()?;
                let elseif = self.allocator.alloc(IfPred::ElseIf(expr, scope, pred));
                return Ok(Some(elseif));
            }
            let scope = self.parse_scope()?;
            let else_ = self.allocator.alloc(IfPred::Else(scope));
            return Ok(Some(else_));
        } else {
            return Ok(None);
        }
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
    fn parse_expr(&mut self, expected_type: &Type, min_prec: u8) -> Result<ArenaPtr<Expr>> {
        let atom = self.parse_atom()?;
        if self.peek(0).is_none() {
            return Err(self.error(ParseErrorType::Expected("expression".to_string())));
        }
        if let Atom::StrLit(_) = *atom {
            if expected_type != &Type::Str {
                return Err(self.error(ParseErrorType::MismatchedTypes));
            }
            let expr = self.allocator.alloc(Expr::Atom(atom));
            return Ok(expr);
        }
        if expected_type != &Type::Int {
            return Err(self.error(ParseErrorType::MismatchedTypes));
        }
        let mut lhs;
        if self.peek(0).unwrap().type_ == TokenType::OpenParen {
            self.consume();
            if let Atom::Ident(ref func_ident) = *atom {
                let mut params = Vec::new();
                while self.peek(0).is_some() && self.peek(0).unwrap().type_ != TokenType::CloseParen {
                    params.push(self.parse_expr(expected_type, 0)?);
                    if self.peek(0).is_some() && self.peek(0).unwrap().type_ == TokenType::CloseParen {
                        break;
                    }
                    self.try_consume(&TokenType::Comma)?;
                }
                self.consume();
                lhs = self.allocator.alloc(Expr::FuncCall(func_ident.to_string(), params));
            } else {
                return Err(self.error(ParseErrorType::Expected("identifier".to_string())));
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
        return Ok(lhs);
    }
    fn parse_atom(&mut self) -> Result<ArenaPtr<Atom>> {
        if self.peek(0).is_none() {
            return Err(self.error(ParseErrorType::Expected("atom".to_string())));
        }
        return match self.consume() {
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
                let expr = self.parse_expr(&Type::Int, 0)?;
                self.try_consume(&TokenType::CloseParen)?;
                let atom = self.allocator.alloc(Atom::Paren(expr));
                Ok(atom)
            }
            _ => Err(self.error(ParseErrorType::Expected("atom".to_string())))
        } 
    }
    fn peek(&self, offset: i32) -> Option<Box<Token>> {
        let idx = self.idx as i32 + offset;
        return match self.tokens.get(idx as usize) {
            Some(token) => Some(Box::new(token.clone())),
            None => None,
        }
    }
    fn consume(&mut self) -> TokenType {
        self.idx += 1;
        return self.tokens[self.idx - 1].type_.clone();
    }
    fn try_consume(&mut self, token_type: &TokenType) -> Result<()> {
        if self.peek(0).is_some() && self.peek(0).unwrap().type_ == *token_type {
            self.consume();
        } else {
            return Err(self.error(ParseErrorType::Expected(match *token_type {
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
            }.to_string())));
        }
        return Ok(());
    }
}
