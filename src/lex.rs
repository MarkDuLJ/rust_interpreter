use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use thiserror::Error;

pub struct Lexer<'de>{
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, miette::Error>>,
}

impl <'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte:0,
            peeked:None,
        }
    }

    pub fn expect(&mut self, expected: TokenKind, unexpected: &str) 
            -> Result<Token<'de>, miette::Error> {

        self.expect_where(|next| next.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str
    ) -> Result<Token<'de>, miette::Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette!{
                labels = vec![LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")],
                help = format!("Expected {token:?}"),
                "{unexpected}",
            }.with_source_code(self.whole.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(Eof.into()), 
        }
    }
}

impl <'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                Slash,
                String,
                Number,
                Ident,
                IfEqualElse(TokenKind, TokenKind)
            }

            let just = move |kind: TokenKind| {
                Some(Ok(Token {
                    kind,
                    offset: c_at,
                    origin: c_str,
                }))
            };

            let started = match c {
                '(' => return just(TokenKind::LeftParen),
                ')' => return just(TokenKind::RightParen),
                '{' => return just(TokenKind::LeftBrace),
                '}' => return just(TokenKind::RightBrace),
                ',' => return just(TokenKind::Comma),
                '.' => return just(TokenKind::Dot),
                '-' => return just(TokenKind::Minus),
                '+' => return just(TokenKind::Plus),
                ';' => return just(TokenKind::Semicolon),
                '*' => return just(TokenKind::Star),
                '/' => Started::Slash,
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError{
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }.into()));
                }
            };

            break match started {
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        let line_end = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.byte += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    }else {
                        Some(Ok(Token{
                            origin: c_str,
                            offset: c_at,
                            kind: TokenKind::Slash
                        }))
                    }
                },
                Started::String => {
                    if let Some(end) = self.rest.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        Some(Ok(Token {
                            origin: literal,
                            offset: c_at,
                            kind: TokenKind::String
                        }))
                    }else {
                        let err = StringTerminationError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        };


                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];

                        return Some(Err(err.into()));
                    }
                },
                Started::Number => {
                    let first_non_digit = c_onwards
                            .find(|c| !matches!(c, '.' | '0'..='9'))
                            .unwrap_or_else(|| c_onwards.len());
                    
                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), None) if two.is_empty()=> {
                            literal = &literal[..one.len()];
                        },
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        },
                        _ => {},  // do nothing
                    }

                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let n = match literal.parse(){
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette!{
                                labels = vec![LabeledSpan::at(self.byte - literal.len()..self.byte, "this number")],
                                "{e}",
                            }.with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token { origin: literal, offset: c_at, kind: TokenKind::Number(n) }));
                },
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "print" => TokenKind::Print,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token { origin: literal, offset: c_at, kind }));
                },
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        Some(Ok(Token { origin: span, offset: c_at, kind: yes }))
                    }else {
                        Some(Ok(Token { origin: c_str, offset: c_at, kind: no }))
                    }
                },
            }
        }
    }
}

#[derive(Debug)]
pub struct Token<'de> {
    pub origin: &'de str,
    pub offset: usize,
    pub kind: TokenKind,
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Diagnostic, Error, Debug)]
#[error("Unexpected token '{token}'")]
pub struct SingleTokenError {
    src: String,
    pub token: char,
    err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecognized = &self.src[..=self.err_span.offset()];
        until_unrecognized.lines().count()
    }
}


#[derive(Diagnostic, Error, Debug)]
#[error("Unterminated string")]
pub struct StringTerminationError {
    src: String,
    err_span: SourceSpan,
}

impl StringTerminationError {
    pub fn line(&self) -> usize {
        let until_unrecognized = &self.src[..=self.err_span.offset()];
        until_unrecognized.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token EOF")]
pub struct Eof;