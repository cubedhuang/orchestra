use std::{fmt::Display, iter::Peekable, str::CharIndices};

use miette::{Diagnostic, SourceSpan};
use phf::phf_map;
use thiserror::Error;

use crate::error::Result;

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

    #[error("unterminated comment")]
    #[diagnostic(code(lex::unterminated_comment))]
    UnterminatedComment {
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
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    At,
    AtEq,
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
    If,
    Else,
    While,
    For,
    Return,
    Or,
    And,

    Number,
    String,
    Identifier,

    Eoi,
    // invalid state for parser.previous
    Invalid,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Symbols
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Ampersand => write!(f, "'&'"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::At => write!(f, "'@'"),
            TokenKind::AtEq => write!(f, "'@='"),
            TokenKind::LeftParen => write!(f, "'('"),
            TokenKind::RightParen => write!(f, "')'"),
            TokenKind::LeftBrace => write!(f, "'{{'"),
            TokenKind::RightBrace => write!(f, "'}}'"),
            TokenKind::Semi => write!(f, "';'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::EqEq => write!(f, "'=='"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::BangEq => write!(f, "'!='"),
            TokenKind::Less => write!(f, "'<'"),
            TokenKind::LessEq => write!(f, "'<='"),
            TokenKind::Greater => write!(f, "'>'"),
            TokenKind::GreaterEq => write!(f, "'>='"),

            // Keywords
            TokenKind::Let => write!(f, "'let'"),
            TokenKind::Fn => write!(f, "'fn'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Else => write!(f, "'else'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::For => write!(f, "'for'"),
            TokenKind::Return => write!(f, "'return'"),
            TokenKind::Or => write!(f, "'or'"),
            TokenKind::And => write!(f, "'and'"),

            TokenKind::Number => write!(f, "<number>"),
            TokenKind::String => write!(f, "<string>"),
            TokenKind::Identifier => write!(f, "<identifier>"),

            TokenKind::Eoi => write!(f, "EOI"),
            // invalid state for parser.previous
            TokenKind::Invalid => write!(f, "INVALID"),
        }
    }
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "let" => TokenKind::Let,
    "fn" => TokenKind::Fn,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "while" => TokenKind::While,
    "for" => TokenKind::For,
    "return" => TokenKind::Return,
    "or" => TokenKind::Or,
    "and" => TokenKind::And,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
    pub span: SourceSpan,
}

impl<'src> Token<'src> {
    pub fn invalid() -> Token<'static> {
        Token {
            kind: TokenKind::Invalid,
            lexeme: "",
            span: SourceSpan::new(0.into(), 0),
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'src> {
    start: usize,
    current: usize,
    done: bool,
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>>;

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
            source: source,
            chars: source.char_indices().peekable(),
        }
    }

    pub fn scan_token(&mut self) -> Result<Token<'src>> {
        self.skip_whitespace()?;
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
            '&' => Ok(self.make_token(TokenKind::Ampersand)),
            '|' => Ok(self.make_token(TokenKind::Pipe)),
            '^' => Ok(self.make_token(TokenKind::Caret)),
            '~' => Ok(self.make_token(TokenKind::Tilde)),
            '@' => {
                let kind = if self.matches('=') {
                    TokenKind::AtEq
                } else {
                    TokenKind::At
                };
                Ok(self.make_token(kind))
            }
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
            }
            .into()),
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token<'src> {
        Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            span: SourceSpan::new(self.start.into(), self.current - self.start),
        }
    }

    fn advance(&mut self) -> char {
        if let Some((idx, ch)) = self.chars.next() {
            self.current = idx + ch.len_utf8();
            ch
        } else {
            '\0'
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if let Some(&(_, ch)) = self.chars.peek() {
            if ch == expected {
                self.advance();
                return true;
            }
        }
        false
    }

    fn peek(&mut self) -> char {
        self.chars.peek().map(|&(_, ch)| ch).unwrap_or('\0')
    }

    fn peek_next(&mut self) -> char {
        let mut iter = self.source[self.current..].chars();
        iter.next();
        iter.next().unwrap_or('\0')
    }

    fn skip_whitespace(&mut self) -> Result<()> {
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
                    } else if self.peek_next() == '*' {
                        self.advance();
                        self.advance();
                        self.skip_multiline_comment(self.current - 2)?;
                    } else {
                        return Ok(());
                    }
                }
                _ => return Ok(()),
            }
        }
    }

    fn skip_multiline_comment(&mut self, start: usize) -> Result<()> {
        let mut depth = 1;
        while depth > 0 && !self.is_at_end() {
            if self.peek() == '/' && self.peek_next() == '*' {
                self.advance();
                self.advance();
                depth += 1;
            } else if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                depth -= 1;
            } else {
                self.advance();
            }
        }
        if depth != 0 {
            Err(LexError::UnterminatedComment {
                span: SourceSpan::new(start.into(), self.current - start),
            }
            .into())
        } else {
            Ok(())
        }
    }

    fn string(&mut self) -> Result<Token<'src>> {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            Err(LexError::UnterminatedString {
                span: SourceSpan::new(self.start.into(), self.current - self.start),
            }
            .into())
        } else {
            self.advance();
            Ok(self.make_token(TokenKind::String))
        }
    }

    fn number(&mut self) -> Result<Token<'src>> {
        // parse extra so that they can be correctly discarded later
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        Ok(self.make_token(TokenKind::Number))
    }

    fn identifier(&mut self) -> Result<Token<'src>> {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        let lexeme = &self.source[self.start..self.current];
        let kind = KEYWORDS
            .get(lexeme)
            .copied()
            .unwrap_or(TokenKind::Identifier);

        Ok(self.make_token(kind))
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        let source = "+-*&|^/(){};,.= == ! != < <= > >=";
        let expected = [
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Ampersand,
            TokenKind::Pipe,
            TokenKind::Caret,
            TokenKind::Tilde,
            TokenKind::Slash,
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::LeftBrace,
            TokenKind::RightBrace,
            TokenKind::Semi,
            TokenKind::Comma,
            TokenKind::Dot,
            TokenKind::Eq,
            TokenKind::EqEq,
            TokenKind::Bang,
            TokenKind::BangEq,
            TokenKind::Less,
            TokenKind::LessEq,
            TokenKind::Greater,
            TokenKind::GreaterEq,
            TokenKind::Eoi,
        ];

        let lexer = Lexer::new(source);
        for (token, expected) in lexer.zip(expected) {
            assert_eq!(token.unwrap().kind, expected);
        }
    }

    #[test]
    fn keywords() {
        let source = "hello let fn for while if ";
        let expected = [
            TokenKind::Identifier,
            TokenKind::Let,
            TokenKind::Fn,
            TokenKind::For,
            TokenKind::While,
            TokenKind::If,
            TokenKind::Eoi,
        ];

        let lexer = Lexer::new(source);
        for (token, expected) in lexer.zip(expected) {
            assert_eq!(token.unwrap().kind, expected);
        }
    }

    #[test]
    fn number_literals() {
        let source = "42 0xFF 0b1010 0o77 0x1a3 0B101 0O644";
        let expected = [
            ("42", TokenKind::Number),
            ("0xFF", TokenKind::Number),
            ("0b1010", TokenKind::Number),
            ("0o77", TokenKind::Number),
            ("0x1a3", TokenKind::Number),
            ("0B101", TokenKind::Number),
            ("0O644", TokenKind::Number),
            ("", TokenKind::Eoi),
        ];

        let lexer = Lexer::new(source);
        for (token, (expected_lexeme, expected_kind)) in lexer.zip(expected) {
            let token = token.unwrap();
            assert_eq!(token.kind, expected_kind);
            assert_eq!(token.lexeme, expected_lexeme);
        }
    }
}
