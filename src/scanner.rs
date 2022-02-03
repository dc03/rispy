use crate::ast::*;
use crate::error;

pub struct Scanner<'a> {
    filename: String,
    source: &'a String,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a String, file: String) -> Scanner<'a> {
        Scanner {
            source: src,
            pos: 0,
            line: 1,
            col: 1,
            filename: file,
        }
    }

    fn try_match(&mut self, string: &str) -> bool {
        if self.source.as_str()[self.pos..].starts_with(string) {
            self.pos += string.len();
            if string == "\n" {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += string.len();
            }
            true
        } else {
            false
        }
    }

    fn try_match_char(&mut self, ch: char) -> bool {
        if self.source.chars().nth(self.pos) == Some(ch) {
            self.pos += 1;
            self.col += 1;
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            }
            true
        } else {
            false
        }
    }

    fn try_match_from_str(&mut self, string: &str) -> bool {
        for ch in string.chars() {
            if self.try_match_char(ch) {
                return true;
            }
        }

        false
    }

    fn try_match_using<F>(&mut self, f: F) -> bool
    where
        F: FnOnce(&char) -> bool,
    {
        if f(&self.source.chars().nth(self.pos).unwrap()) {
            if self.source.chars().nth(self.pos) == Some('\n') {
                self.line += 1;
                self.col = 1;
            }
            self.col += 1;
            self.pos += 1;

            true
        } else {
            false
        }
    }

    fn try_match_alpha(&mut self) -> bool {
        self.try_match_using(char::is_ascii_alphabetic)
    }

    fn try_match_digit(&mut self) -> bool {
        self.try_match_using(char::is_ascii_digit)
    }

    fn try_match_letter(&mut self) -> bool {
        self.try_match_alpha() || self.try_match_digit() || self.try_match_char('_')
    }

    fn try_match_operator(&mut self) -> bool {
        self.try_match_from_str("+-*/%")
    }

    fn try_scan_num(&mut self) -> Option<Token<'a>> {
        let col = self.col;
        let byte_start = self.pos;

        if self.try_match_digit() {
            while self.try_match_digit() {}

            let byte_end = self.pos;
            Some(Token {
                lexeme: &self.source[byte_start..byte_end],
                byte_start: byte_start,
                byte_end: byte_end,
                line: self.line,
                column: col,
                ttype: TokenType::Number,
            })
        } else {
            None
        }
    }

    fn try_scan_symbol(&mut self) -> Option<Token<'a>> {
        let col = self.col;
        let byte_start = self.pos;

        if self.try_match_letter() {
            while self.try_match_letter() || self.try_match_operator() || self.try_match_char('?') {
            }

            let byte_end = self.pos;
            Some(Token {
                lexeme: &self.source[byte_start..byte_end],
                byte_start: byte_start,
                byte_end: byte_end,
                line: self.line,
                column: col,
                ttype: TokenType::Symbol,
            })
        } else if self.try_match_operator() {
            let byte_end = self.pos;
            Some(Token {
                lexeme: &self.source[byte_start..byte_end],
                byte_start: byte_start,
                byte_end: byte_end,
                line: self.line,
                column: col,
                ttype: TokenType::Symbol,
            })
        } else {
            None
        }
    }

    fn try_scan_punc(&mut self) -> Option<Token<'a>> {
        let col = self.col;
        let byte_start = self.pos;
        if self.try_match_from_str("(){}") {
            let ch = self.source.chars().nth(self.pos - 1).unwrap();

            let ttype = match ch {
                '(' => TokenType::ParenOpen,
                ')' => TokenType::ParenClose,
                '{' => TokenType::LBraceOpen,
                '}' => TokenType::LBraceClose,
                _ => panic!("Unreachable code"),
            };

            Some(Token {
                lexeme: &self.source[byte_start..byte_start + 1],
                byte_start: byte_start,
                byte_end: byte_start + 1,
                line: self.line,
                column: col,
                ttype: ttype,
            })
        } else {
            None
        }
    }

    fn try_match_whitespace(&mut self) -> bool {
        self.try_match_from_str("\n\r\t ")
    }

    pub fn scan_one(&mut self) -> Option<Token<'a>> {
        if self.pos >= self.source.len() {
            return None;
        }

        let tok = self.try_scan_num();
        if tok.is_some() {
            return tok;
        }
        let tok = self.try_scan_symbol();
        if tok.is_some() {
            return tok;
        }
        let tok = self.try_scan_punc();
        if tok.is_some() {
            return tok;
        }

        if self.try_match_whitespace() {
            while self.try_match_whitespace() {}
            return self.scan_one();
        }

        error::print_error(
            format!(
                "Unknown character for start of token: `{}`",
                self.source.chars().nth(self.pos).unwrap()
            ),
            self.source,
            self.line,
            self.col,
            &self.filename,
        );
        None
    }
}

#[cfg(test)]
mod test {
    use super::Scanner;
    use crate::scanner::*;
    use TokenType::*;

    #[test]
    fn basic() {
        let source = "(+ 1 2 3 4 x-y? _a+b-c)".to_string();
        let mut scanner = Scanner::new(&source, "test".to_string());

        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "(",
                byte_start: 0,
                byte_end: 1,
                line: 1,
                column: 1,
                ttype: ParenOpen
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "+",
                byte_start: 1,
                byte_end: 2,
                line: 1,
                column: 2,
                ttype: Symbol
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "1",
                byte_start: 3,
                byte_end: 4,
                line: 1,
                column: 4,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "2",
                byte_start: 5,
                byte_end: 6,
                line: 1,
                column: 6,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "3",
                byte_start: 7,
                byte_end: 8,
                line: 1,
                column: 8,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "4",
                byte_start: 9,
                byte_end: 10,
                line: 1,
                column: 10,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "x-y?",
                byte_start: 11,
                byte_end: 15,
                line: 1,
                column: 12,
                ttype: Symbol
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "_a+b-c",
                byte_start: 16,
                byte_end: 22,
                line: 1,
                column: 17,
                ttype: Symbol
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: ")",
                byte_start: 22,
                byte_end: 23,
                line: 1,
                column: 23,
                ttype: ParenClose
            })
        );
    }

    #[test]
    fn newline() {
        let source = "(- 1 2\n3\n4)".to_string();
        let mut scanner = Scanner::new(&source, "test".to_string());

        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "(",
                byte_start: 0,
                byte_end: 1,
                line: 1,
                column: 1,
                ttype: ParenOpen
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "-",
                byte_start: 1,
                byte_end: 2,
                line: 1,
                column: 2,
                ttype: Symbol
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "1",
                byte_start: 3,
                byte_end: 4,
                line: 1,
                column: 4,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "2",
                byte_start: 5,
                byte_end: 6,
                line: 1,
                column: 6,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "3",
                byte_start: 7,
                byte_end: 8,
                line: 2,
                column: 1,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: "4",
                byte_start: 9,
                byte_end: 10,
                line: 3,
                column: 1,
                ttype: Number
            })
        );
        assert_eq!(
            scanner.scan_one(),
            Some(Token {
                lexeme: ")",
                byte_start: 10,
                byte_end: 11,
                line: 3,
                column: 2,
                ttype: ParenClose
            })
        );
    }
}
