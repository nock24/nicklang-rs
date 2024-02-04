use std::fmt;

#[derive(Debug)]
pub enum TokenizationError {
    UnexpectedChar(char),
    UnclosedComment,
    UnclosedStringLit,
}
#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Ident(String),  

    // literals
    IntLit(i32),    
    StrLit(String),

    // inbuilt functions
    Exit,           
    Print,

    // definition keywords
    Func,

    // control flow
    If,             
    Else,           
    Return,

    // other keywords
    Type(Type),
    Let,            
    Mut,

    // characters/symbols
    Op(Op),         
    Colon,
    Comma,
    SemiColon,      
    OpenParen,      
    CloseParen,     
    OpenCurly,      
    CloseCurly,     
    Eq,             
    Arrow,
}
#[derive(Clone)]
pub struct Token {
    pub type_: TokenType,
    pub line_num: u32,
}
impl PartialEq for Token {
    fn eq(&self, rhs: &Token) -> bool {
        return self.type_ == rhs.type_;
    }
}
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.type_)
    }
}
impl Token {
    pub fn new(token_type: TokenType, line_num: u32) -> Token {
        return Token {type_: token_type, line_num};
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Type { 
    Int,
    Str,
    Ptr,
}
pub struct Tokenizer {
    src: String,
    idx: usize,
    line_cnt: u32,
}
impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        return Tokenizer {src, idx: 0, line_cnt: 1}; 
    }
    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizationError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut buf = String::new();
        while self.peek(0).is_some() {
            if self.peek(0).unwrap().is_alphabetic() {
                buf.push(self.consume());
                while self.peek(0).is_some() && self.peek(0).unwrap().is_alphabetic() {
                    buf.push(self.consume());
                }
                tokens.push(Token::new(match buf.as_str() {
                    "fn" => TokenType::Func,
                    "exit" => TokenType::Exit,
                    "let" => TokenType::Let,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "int" => TokenType::Type(Type::Int),
                    "str" => TokenType::Type(Type::Str),
                    "return" => TokenType::Return,
                    "mut" => TokenType::Mut,
                    "print" => TokenType::Print,
                    _ => TokenType::Ident(buf.clone())
                }, self.line_cnt));
                buf.clear();
            } else if self.peek(0).unwrap().is_digit(10) {
                buf.push(self.consume());
                while self.peek(0).is_some() && self.peek(0).unwrap().is_digit(10) {
                    buf.push(self.consume());
                }
                tokens.push(Token::new(TokenType::IntLit(buf.parse().unwrap()), self.line_cnt));
                buf.clear();
            } else if self.peek(0).unwrap().is_whitespace() {
                if self.consume() == '\n' {
                    self.line_cnt += 1;
                }
            } else if self.peek(0).unwrap() == '/' 
                    && self.peek(1).is_some()
                    && self.peek(1).unwrap() == '/' {
                while self.peek(0).is_some() && self.peek(0).unwrap() != '\n' {
                    self.consume();
                }
            } else if self.peek(0).unwrap() == '/'
                    && self.peek(1).is_some()
                    && self.peek(1).unwrap() == '*' {
                while self.peek(0).is_some() {
                    if self.peek(0).unwrap() == '*'
                        && self.peek(1).is_some()
                        && self.peek(1).unwrap() == '/' {
                        break;
                    }
                    self.consume();
                }
                if self.peek(0).is_none() || self.peek(1).is_none() {
                    return Err(TokenizationError::UnclosedComment);
                }
                self.consume();
                self.consume();
            } else if self.peek(0).unwrap() == '"' {
                self.consume();
                let mut str_lit = String::new();
                while self.peek(0).is_some() {
                    match self.consume() {
                        '"' => break,
                        x => str_lit.push(x),
                    }
                }
                if self.peek(0).is_none() {
                    return Err(TokenizationError::UnclosedStringLit);
                }
                tokens.push(Token::new(TokenType::StrLit(str_lit), self.line_cnt));
            } else {
                tokens.push(Token::new(match self.consume() {
                    '+' => TokenType::Op(Op::Plus),
                    '-' => {
                        if self.peek(0).is_some() && self.peek(0).unwrap() == '>' {
                            self.consume();
                            TokenType::Arrow
                        } else {
                            TokenType::Op(Op::Minus)
                        }
                    }
                    '*' => TokenType::Op(Op::Star),
                    '/' => TokenType::Op(Op::Slash),

                    '(' => TokenType::OpenParen,
                    ')' => TokenType::CloseParen,
                    '{' => TokenType::OpenCurly,
                    '}' => TokenType::CloseCurly,
                    '=' => TokenType::Eq,
                    ';' => TokenType::SemiColon,
                    ',' => TokenType::Comma,
                    ':' => TokenType::Colon,

                    char => return Err(TokenizationError::UnexpectedChar(char))
                }, self.line_cnt)); 
            }
        }
        self.idx = 0;
        return Ok(tokens);
    }
    fn peek(&self, offset: usize) -> Option<char> {
        return self.src.chars().nth(self.idx + offset);
    }
    fn consume(&mut self) -> char {
        self.idx += 1;
        return self.src.chars().nth(self.idx - 1).unwrap();
    }
}
