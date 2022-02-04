use crate::ast::*;
use crate::error;
use unicode_segmentation::UnicodeSegmentation;

pub struct Scanner<'a> {
    filename: String,
    source: &'a String,
    pos: usize,
    line: usize,
    col: usize,

    graphemes: Vec<&'a str>,
    current: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a String, file: String) -> Scanner<'a> {
        Scanner {
            source: src,
            pos: 0,
            line: 1,
            col: 1,
            filename: file,

            graphemes: src.graphemes(true).collect::<Vec<&str>>(),
            current: 0,
        }
    }

    fn is_alpha(string: &str) -> bool {
        !Self::is_digit(string)
            && !Self::is_operator(string)
            && !Self::is_whitespace(string)
            && !Self::is_punc(string)
    }

    fn is_digit(string: &str) -> bool {
        string.len() == 1 && string.chars().nth(0).unwrap().is_numeric()
    }

    fn is_operator(string: &str) -> bool {
        string.len() == 1 && ["+", "-", "*", "/", "%"].contains(&string)
    }

    fn is_punc(string: &str) -> bool {
        string == "(" || string == ")" || string == "{" || string == "}"
    }

    fn is_letter(string: &str) -> bool {
        Self::is_alpha(string) || Self::is_digit(string)
    }

    fn is_whitespace(string: &str) -> bool {
        string.len() == 1 && char::is_whitespace(string.chars().nth(0).unwrap())
    }

    fn next_grapheme(&mut self) -> Option<&'a str> {
        if self.current >= self.graphemes.len() {
            None
        } else {
            let grapheme = self.graphemes[self.current];

            self.pos += grapheme.len();
            if grapheme.contains("\n") {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += grapheme.chars().count();
            }

            self.current += 1;
            Some(self.graphemes[self.current - 1])
        }
    }

    fn putback_one(&mut self) -> bool {
        if self.current == 0 {
            false
        } else {
            self.current -= 1;
            self.pos -= self.graphemes[self.current].len();
            self.col -= 1;
            if self.graphemes[self.current] == "\n" || self.graphemes[self.current] == "\r\n" {
                self.line -= 1;
                self.col = 0;
            }
            true
        }
    }

    pub fn scan_one(&mut self) -> Option<Token<'a>> {
        let col = self.col;

        let next = self.next_grapheme();

        if let Some(next) = next {
            // println!("DBG: {:?}", next);
            match next {
                _ if Self::is_whitespace(next) => self.scan_one(),
                "(" => Some(Token::from_pos(
                    next,
                    self.pos,
                    self.line,
                    col,
                    TokenType::ParenOpen,
                )),
                ")" => Some(Token::from_pos(
                    next,
                    self.pos,
                    self.line,
                    col,
                    TokenType::ParenClose,
                )),
                "{" => Some(Token::from_pos(
                    next,
                    self.pos,
                    self.line,
                    col,
                    TokenType::LBraceOpen,
                )),
                "}" => Some(Token::from_pos(
                    next,
                    self.pos,
                    self.line,
                    col,
                    TokenType::LBraceClose,
                )),
                s if Self::is_operator(s) => Some(Token::from_pos(
                    &self.source.as_str()[self.pos - 1..self.pos],
                    self.pos,
                    self.line,
                    self.col - 1,
                    TokenType::Symbol,
                )),
                s if Self::is_digit(s) => {
                    let col = self.col - 1;
                    let mut lex_len = s.len();

                    while let Some(next) = self.next_grapheme() {
                        if Self::is_digit(next) {
                            lex_len += next.len();
                        } else {
                            if !self.putback_one() {
                                panic!("putback_one() failed");
                            }
                            break;
                        }
                    }

                    Some(Token::from_pos(
                        &self.source.as_str()[self.pos - lex_len..self.pos],
                        self.pos,
                        self.line,
                        col,
                        TokenType::Number,
                    ))
                }
                s if Self::is_letter(s) => {
                    let col = self.col - 1;
                    let mut lex_len = s.len();

                    while let Some(next) = self.next_grapheme() {
                        if Self::is_letter(next) || Self::is_operator(next) {
                            lex_len += next.len();
                        } else {
                            if !self.putback_one() {
                                panic!("putback_one() failed");
                            }
                            break;
                        }
                    }

                    Some(Token::from_pos(
                        &self.source.as_str()[self.pos - lex_len..self.pos],
                        self.pos,
                        self.line,
                        col,
                        TokenType::Symbol,
                    ))
                }
                s => Some(Token {
                    lexeme: "",
                    byte_start: 0,
                    byte_end: 1,
                    line: 1,
                    column: 1,
                    ttype: TokenType::Symbol,
                }),
            }
        } else {
            None
        }
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
