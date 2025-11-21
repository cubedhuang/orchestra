use miette::{Diagnostic, SourceSpan};
use phf::phf_map;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum LexError {
    #[error("unexpected character '{character}'")]
    #[diagnostic(code(lex::unexpected_character))]
    UnexpectedCharacter {
        character: char,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("unterminated string")]
    #[diagnostic(code(lex::unterminated_string))]
    UnterminatedString {
        #[label("here")]
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semi,
    Comma,
    Dot,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    // Keywords
    Let,
    Fn,
    For,
    While,
    If,

    Number,
    String,
    Identifier,

    Eoi,
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "let" => TokenKind::Let,
    "fn" => TokenKind::Fn,
    "for" => TokenKind::For,
    "while" => TokenKind::While,
    "if" => TokenKind::If,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'src> {
    kind: TokenKind,
    lexeme: &'src [u8],
    location: usize,
}

pub struct Lexer<'src> {
    start: usize,
    current: usize,
    done: bool,
    source: &'src [u8],
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        Some(self.scan_token())
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            start: 0,
            current: 0,
            done: false,
            source: source.as_bytes(),
        }
    }

    fn scan_token(&mut self) -> Result<Token<'src>, LexError> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            self.done = true;
            return Ok(self.make_token(TokenKind::Eoi));
        }

        let c = self.advance();
        match c {
            '(' => Ok(self.make_token(TokenKind::LeftParen)),
            ')' => Ok(self.make_token(TokenKind::RightParen)),
            '{' => Ok(self.make_token(TokenKind::LeftBrace)),
            '}' => Ok(self.make_token(TokenKind::RightBrace)),
            ';' => Ok(self.make_token(TokenKind::Semi)),
            ',' => Ok(self.make_token(TokenKind::Comma)),
            '.' => Ok(self.make_token(TokenKind::Dot)),
            '-' => Ok(self.make_token(TokenKind::Minus)),
            '+' => Ok(self.make_token(TokenKind::Plus)),
            '/' => Ok(self.make_token(TokenKind::Slash)),
            '*' => Ok(self.make_token(TokenKind::Star)),
            '!' => {
                let kind = if self.matches('=') {
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                };
                Ok(self.make_token(kind))
            }
            '=' => {
                let kind = if self.matches('=') {
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                };
                Ok(self.make_token(kind))
            }
            '<' => {
                let kind = if self.matches('=') {
                    TokenKind::LessEq
                } else {
                    TokenKind::Less
                };
                Ok(self.make_token(kind))
            }
            '>' => {
                let kind = if self.matches('=') {
                    TokenKind::GreaterEq
                } else {
                    TokenKind::Greater
                };
                Ok(self.make_token(kind))
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' => self.identifier(),
            _ => Err(LexError::UnexpectedCharacter {
                character: c,
                span: SourceSpan::new(self.start.into(), 1),
            }),
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token<'src> {
        Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            location: self.start,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1] as char
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || expected != self.source[self.current] as char {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current] as char
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1] as char
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn string(&mut self) -> Result<Token<'src>, LexError> {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            Err(LexError::UnterminatedString {
                span: SourceSpan::new((self.current - 1).into(), 0),
            })
        } else {
            self.advance();
            Ok(self.make_token(TokenKind::String))
        }
    }

    fn number(&mut self) -> Result<Token<'src>, LexError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        Ok(self.make_token(TokenKind::Number))
    }

    fn identifier(&mut self) -> Result<Token<'src>, LexError> {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        let lexeme = unsafe { str::from_utf8_unchecked(&self.source[self.start..self.current]) };
        let kind = KEYWORDS
            .get(lexeme)
            .map(|l| l.clone())
            .unwrap_or(TokenKind::Identifier);

        Ok(self.make_token(kind))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        let source = "+-*/(){};,.= == ! != < <= > >=";
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Plus);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Minus);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Star);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Slash);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::LeftParen);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::RightParen);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::LeftBrace);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::RightBrace);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Semi);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Dot);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Eq);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::EqEq);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Bang);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::BangEq);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Less);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::LessEq);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Greater);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::GreaterEq);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Eoi);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn keywords() {
        let source = "hello let fn for while if ";
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Identifier);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Let);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Fn);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::For);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::While);
        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::If);

        assert_eq!(lexer.scan_token().unwrap().kind, TokenKind::Eoi);
        assert!(lexer.next().is_none());
    }
}
