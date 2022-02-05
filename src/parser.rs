use crate::{ast::*, error::*};

pub struct Parser<'a, 'b> {
    toks: Vec<Token<'a>>,
    source: &'a String,
    filename: &'b str,
    current: usize,
}

pub type ExprNode<'a> = Option<Box<Expr<'a>>>;

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(toks: Vec<Token<'a>>, source: &'a String, filename: &'b str) -> Self {
        Parser {
            toks: toks,
            source: source,
            filename: filename,
            current: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.toks.len()
    }

    fn try_advance(&mut self) -> Result<&Token<'a>, &'static str> {
        if self.is_at_end() {
            panic!("Cannot advance beyond EOF")
        } else {
            self.current += 1;
            Ok(&self.toks[self.current - 1])
        }
    }

    fn try_match(&mut self, ttype: TokenType) -> bool {
        if !self.is_at_end() && self.toks[self.current].ttype == ttype {
            match self.try_advance() {
                Ok(_) => true,
                Err(m) => {
                    print_error(
                        m.to_string(),
                        self.source,
                        self.toks[self.current].line,
                        self.toks[self.current].column,
                        self.filename,
                    );
                    false
                }
            }
        } else {
            false
        }
    }

    fn try_peek(&self) -> Option<&Token<'a>> {
        if !self.is_at_end() {
            Some(&self.toks[self.current])
        } else {
            None
        }
    }

    fn try_current(&self) -> Option<&Token<'a>> {
        if self.current > 0 {
            Some(&self.toks[self.current - 1])
        } else {
            None
        }
    }

    fn consume(&mut self, msg: &str, ttype: TokenType) -> Option<&Token<'a>> {
        if self.try_peek().is_none() || self.try_peek().unwrap().ttype != ttype {
            match self.try_peek() {
                Some(t) => print_error(
                    msg.to_string(),
                    self.source,
                    t.line,
                    t.column,
                    self.filename,
                ),
                None => print_generic_error(msg.to_string(), self.filename),
            }
            None
        } else {
            // bug: https://doc.rust-lang.org/nomicon/lifetime-mismatch.html#improperly-reduced-borrows
            //
            // inlining the following four variables into the `print_error` function call causes a compile error
            let (source, line, column, filename) = (
                self.source,
                self.try_peek().unwrap().line,
                self.try_peek().unwrap().column,
                self.filename,
            );

            // here the lifetime is deduced to be
            //
            // let result: Result<&'a Token, &str> = (&mut 'a self).try_advance();
            //
            // because we use `result` in the match, this `'a` lifetime gets extended to the whole match block, and
            // trying to have an immutable borrow in `print_error` while the mutable borrow in `try_advance` is alive is
            // not allowed.
            //
            // 'a: {
            let result = self.try_advance();
            match result {
                Ok(token) => Some(token),
                Err(msg) => {
                    // The following usage causes the error:
                    // 'b: {
                    // print_error(msg.to_string(),
                    //             (&'b self).source,
                    //             previous.line,
                    //             previous.column,
                    //             (&'b self).filename)
                    print_error(msg.to_string(), source, line, column, filename);
                    None
                    // } 'b ends here
                }
            }
            // } 'a ends here
        }
    }

    // parse a series of expressions ending with a `ttype`
    fn parse_parenthesized(
        &mut self,
        ttype: TokenType,
        ttype_lex: char,
    ) -> (ExprNode<'a>, Vec<ExprNode<'a>>) {
        let func = self.parse_expr();
        let mut exprs = vec![];

        loop {
            let next = self.try_peek();
            if next.is_some() && next.unwrap().ttype == ttype || next.is_none() {
                break;
            }
            exprs.push(self.parse_expr());
        }

        if self
            .consume(
                format!("Expected {} after expression", ttype_lex).as_str(),
                ttype,
            )
            .is_none()
        {
            return (None, vec![]);
        }

        (func, exprs)
    }

    fn parse_sexpr(&mut self, tok: &Token<'a>) -> ExprNode<'a> {
        let (func, args) = self.parse_parenthesized(TokenType::ParenClose, ')');

        let func = func?;
        let args = args
            .into_iter()
            .filter(|arg| arg.is_some())
            .map(|arg| arg.unwrap())
            .collect::<Vec<Box<Expr<'a>>>>();

        Some(Box::new(Expr::SExpr(SExpr {
            func: func,
            args: args,
            tok: *tok,
        })))
    }

    fn parse_qexpr(&mut self, tok: &Token<'a>) -> ExprNode<'a> {
        let (func, args) = self.parse_parenthesized(TokenType::LBraceClose, '}');

        let func = func?;
        let args = args
            .into_iter()
            .filter(|arg| arg.is_some())
            .map(|arg| arg.unwrap())
            .collect::<Vec<Box<Expr<'a>>>>();

        Some(Box::new(Expr::QExpr(QExpr {
            func: func,
            args: args,
            tok: *tok,
        })))
    }

    fn parse_expr(&mut self) -> ExprNode<'a> {
        let next = *self.try_peek()?;

        if self.try_match(TokenType::ParenOpen) {
            self.parse_sexpr(&next)
        } else if self.try_match(TokenType::LBraceOpen) {
            self.parse_qexpr(&next)
        } else if self.try_match(TokenType::Number) {
            Some(Box::new(Expr::Num(Number {
                tok: next,
                val: next.lexeme.parse::<isize>().unwrap(), // This should always be a valid number
            })))
        } else if self.try_match(TokenType::Symbol) {
            Some(Box::new(Expr::Sym(Symbol { tok: next })))
        } else {
            let previous = self.try_current();
            match previous {
                Some(tok) => print_error(
                    "Unrecognized token for start of expression".to_string(),
                    self.source,
                    tok.line,
                    tok.column,
                    self.filename,
                ),
                None => print_generic_error(
                    "Unrecognized token for start of expression".to_string(),
                    self.filename,
                ),
            }
            None
        }
    }

    pub fn parse(&mut self) -> Vec<Box<SExpr<'a>>> {
        let mut prog = vec![];

        if !self.toks.is_empty() {
            while !self.is_at_end() {
                let tok = self.consume(
                    "Expected '(' at the start of expression",
                    TokenType::ParenOpen,
                );

                if let Some(tok) = tok {
                    let tok = *tok;
                    match self.parse_sexpr(&tok) {
                        Some(sexpr) => match *sexpr {
                            Expr::SExpr(expr) => prog.push(Box::new(expr)),
                            _ => unreachable!(),
                        },
                        None => {}
                    };
                } else {
                    break;
                }
            }
        }

        prog
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::*, parser::Parser, scanner::Scanner};

    fn parse_string<'a>(source: &'a String) -> Vec<Box<SExpr<'a>>> {
        let mut scanner = Scanner::new(source, "test");

        let mut toks = vec![];
        while let Some(tok) = scanner.scan_one() {
            toks.push(tok);
        }

        let mut parser = Parser::new(toks, source, "test");
        parser.parse()
    }

    #[test]
    fn basics() {
        let source = "(+ 1 2 3 4)".to_string();
        let ast = parse_string(&source);

        assert_eq!(ast.len(), 1);
        assert_eq!(
            ast[0],
            Box::new(SExpr {
                func: Box::new(Expr::Sym(Symbol {
                    tok: Token {
                        lexeme: "+",
                        byte_start: 1,
                        byte_end: 2,
                        line: 1,
                        column: 2,
                        ttype: TokenType::Symbol
                    }
                })),
                args: vec![
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "1",
                            byte_start: 3,
                            byte_end: 4,
                            line: 1,
                            column: 4,
                            ttype: TokenType::Number
                        },
                        val: 1
                    })),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "2",
                            byte_start: 5,
                            byte_end: 6,
                            line: 1,
                            column: 6,
                            ttype: TokenType::Number
                        },
                        val: 2
                    })),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "3",
                            byte_start: 7,
                            byte_end: 8,
                            line: 1,
                            column: 8,
                            ttype: TokenType::Number
                        },
                        val: 3
                    })),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "4",
                            byte_start: 9,
                            byte_end: 10,
                            line: 1,
                            column: 10,
                            ttype: TokenType::Number
                        },
                        val: 4
                    }))
                ],
                tok: Token {
                    lexeme: "(",
                    byte_start: 0,
                    byte_end: 1,
                    line: 1,
                    column: 1,
                    ttype: TokenType::ParenOpen
                }
            })
        );
    }

    #[test]
    fn nested() {
        let source = "(+ {+ + 4a(+1)} (((+ 1) 2) 4) 2 3 4)".to_string();
        let ast = parse_string(&source);

        assert_eq!(ast.len(), 1);
        assert_eq!(
            ast[0],
            Box::new(SExpr {
                func: Box::new(Expr::Sym(Symbol {
                    tok: Token {
                        lexeme: "+",
                        byte_start: 1,
                        byte_end: 2,
                        line: 1,
                        column: 2,
                        ttype: TokenType::Symbol,
                    },
                }),),
                args: vec![
                    Box::new(Expr::QExpr(QExpr {
                        func: Box::new(Expr::Sym(Symbol {
                            tok: Token {
                                lexeme: "+",
                                byte_start: 4,
                                byte_end: 5,
                                line: 1,
                                column: 5,
                                ttype: TokenType::Symbol,
                            },
                        },)),
                        args: vec![
                            Box::new(Expr::Sym(Symbol {
                                tok: Token {
                                    lexeme: "+",
                                    byte_start: 6,
                                    byte_end: 7,
                                    line: 1,
                                    column: 7,
                                    ttype: TokenType::Symbol,
                                },
                            },)),
                            Box::new(Expr::Num(Number {
                                tok: Token {
                                    lexeme: "4",
                                    byte_start: 8,
                                    byte_end: 9,
                                    line: 1,
                                    column: 9,
                                    ttype: TokenType::Number,
                                },
                                val: 4,
                            },)),
                            Box::new(Expr::Sym(Symbol {
                                tok: Token {
                                    lexeme: "a",
                                    byte_start: 9,
                                    byte_end: 10,
                                    line: 1,
                                    column: 10,
                                    ttype: TokenType::Symbol,
                                },
                            },)),
                            Box::new(Expr::SExpr(SExpr {
                                func: Box::new(Expr::Sym(Symbol {
                                    tok: Token {
                                        lexeme: "+",
                                        byte_start: 11,
                                        byte_end: 12,
                                        line: 1,
                                        column: 12,
                                        ttype: TokenType::Symbol,
                                    },
                                },)),
                                args: vec![Box::new(Expr::Num(Number {
                                    tok: Token {
                                        lexeme: "1",
                                        byte_start: 12,
                                        byte_end: 13,
                                        line: 1,
                                        column: 13,
                                        ttype: TokenType::Number,
                                    },
                                    val: 1,
                                },)),],
                                tok: Token {
                                    lexeme: "(",
                                    byte_start: 10,
                                    byte_end: 11,
                                    line: 1,
                                    column: 11,
                                    ttype: TokenType::ParenOpen,
                                },
                            },)),
                        ],
                        tok: Token {
                            lexeme: "{",
                            byte_start: 3,
                            byte_end: 4,
                            line: 1,
                            column: 4,
                            ttype: TokenType::LBraceOpen,
                        },
                    },)),
                    Box::new(Expr::SExpr(SExpr {
                        func: Box::new(Expr::SExpr(SExpr {
                            func: Box::new(Expr::SExpr(SExpr {
                                func: Box::new(Expr::Sym(Symbol {
                                    tok: Token {
                                        lexeme: "+",
                                        byte_start: 19,
                                        byte_end: 20,
                                        line: 1,
                                        column: 20,
                                        ttype: TokenType::Symbol,
                                    },
                                },)),
                                args: vec![Box::new(Expr::Num(Number {
                                    tok: Token {
                                        lexeme: "1",
                                        byte_start: 21,
                                        byte_end: 22,
                                        line: 1,
                                        column: 22,
                                        ttype: TokenType::Number,
                                    },
                                    val: 1,
                                },)),],
                                tok: Token {
                                    lexeme: "(",
                                    byte_start: 18,
                                    byte_end: 19,
                                    line: 1,
                                    column: 19,
                                    ttype: TokenType::ParenOpen,
                                },
                            },)),
                            args: vec![Box::new(Expr::Num(Number {
                                tok: Token {
                                    lexeme: "2",
                                    byte_start: 24,
                                    byte_end: 25,
                                    line: 1,
                                    column: 25,
                                    ttype: TokenType::Number,
                                },
                                val: 2,
                            },)),],
                            tok: Token {
                                lexeme: "(",
                                byte_start: 17,
                                byte_end: 18,
                                line: 1,
                                column: 18,
                                ttype: TokenType::ParenOpen,
                            },
                        },)),
                        args: vec![Box::new(Expr::Num(Number {
                            tok: Token {
                                lexeme: "4",
                                byte_start: 27,
                                byte_end: 28,
                                line: 1,
                                column: 28,
                                ttype: TokenType::Number,
                            },
                            val: 4,
                        },)),],
                        tok: Token {
                            lexeme: "(",
                            byte_start: 16,
                            byte_end: 17,
                            line: 1,
                            column: 17,
                            ttype: TokenType::ParenOpen,
                        },
                    },)),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "2",
                            byte_start: 30,
                            byte_end: 31,
                            line: 1,
                            column: 31,
                            ttype: TokenType::Number,
                        },
                        val: 2,
                    },)),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "3",
                            byte_start: 32,
                            byte_end: 33,
                            line: 1,
                            column: 33,
                            ttype: TokenType::Number,
                        },
                        val: 3,
                    },)),
                    Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "4",
                            byte_start: 34,
                            byte_end: 35,
                            line: 1,
                            column: 35,
                            ttype: TokenType::Number,
                        },
                        val: 4,
                    },)),
                ],
                tok: Token {
                    lexeme: "(",
                    byte_start: 0,
                    byte_end: 1,
                    line: 1,
                    column: 1,
                    ttype: TokenType::ParenOpen,
                },
            })
        )
    }

    #[test]
    fn multiple() {
        let source = "(+ 1)(- 2)(* 3)(/ {1 2})".to_string();
        let ast = parse_string(&source);
        assert_eq!(ast.len(), 4);

        assert_eq!(
            ast,
            vec![
                Box::new(SExpr {
                    func: Box::new(Expr::Sym(Symbol {
                        tok: Token {
                            lexeme: "+",
                            byte_start: 1,
                            byte_end: 2,
                            line: 1,
                            column: 2,
                            ttype: TokenType::Symbol,
                        },
                    },)),
                    args: vec![Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "1",
                            byte_start: 3,
                            byte_end: 4,
                            line: 1,
                            column: 4,
                            ttype: TokenType::Number,
                        },
                        val: 1,
                    },)),],
                    tok: Token {
                        lexeme: "(",
                        byte_start: 0,
                        byte_end: 1,
                        line: 1,
                        column: 1,
                        ttype: TokenType::ParenOpen,
                    },
                }),
                Box::new(SExpr {
                    func: Box::new(Expr::Sym(Symbol {
                        tok: Token {
                            lexeme: "-",
                            byte_start: 6,
                            byte_end: 7,
                            line: 1,
                            column: 7,
                            ttype: TokenType::Symbol,
                        },
                    },)),
                    args: vec![Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "2",
                            byte_start: 8,
                            byte_end: 9,
                            line: 1,
                            column: 9,
                            ttype: TokenType::Number,
                        },
                        val: 2,
                    },)),],
                    tok: Token {
                        lexeme: "(",
                        byte_start: 5,
                        byte_end: 6,
                        line: 1,
                        column: 6,
                        ttype: TokenType::ParenOpen,
                    },
                }),
                Box::new(SExpr {
                    func: Box::new(Expr::Sym(Symbol {
                        tok: Token {
                            lexeme: "*",
                            byte_start: 11,
                            byte_end: 12,
                            line: 1,
                            column: 12,
                            ttype: TokenType::Symbol,
                        },
                    },)),
                    args: vec![Box::new(Expr::Num(Number {
                        tok: Token {
                            lexeme: "3",
                            byte_start: 13,
                            byte_end: 14,
                            line: 1,
                            column: 14,
                            ttype: TokenType::Number,
                        },
                        val: 3,
                    },)),],
                    tok: Token {
                        lexeme: "(",
                        byte_start: 10,
                        byte_end: 11,
                        line: 1,
                        column: 11,
                        ttype: TokenType::ParenOpen,
                    },
                }),
                Box::new(SExpr {
                    func: Box::new(Expr::Sym(Symbol {
                        tok: Token {
                            lexeme: "/",
                            byte_start: 16,
                            byte_end: 17,
                            line: 1,
                            column: 17,
                            ttype: TokenType::Symbol,
                        },
                    },)),
                    args: vec![Box::new(Expr::QExpr(QExpr {
                        func: Box::new(Expr::Num(Number {
                            tok: Token {
                                lexeme: "1",
                                byte_start: 19,
                                byte_end: 20,
                                line: 1,
                                column: 20,
                                ttype: TokenType::Number,
                            },
                            val: 1,
                        },)),
                        args: vec![Box::new(Expr::Num(Number {
                            tok: Token {
                                lexeme: "2",
                                byte_start: 21,
                                byte_end: 22,
                                line: 1,
                                column: 22,
                                ttype: TokenType::Number,
                            },
                            val: 2,
                        },)),],
                        tok: Token {
                            lexeme: "{",
                            byte_start: 18,
                            byte_end: 19,
                            line: 1,
                            column: 19,
                            ttype: TokenType::LBraceOpen,
                        },
                    },)),],
                    tok: Token {
                        lexeme: "(",
                        byte_start: 15,
                        byte_end: 16,
                        line: 1,
                        column: 16,
                        ttype: TokenType::ParenOpen,
                    },
                }),
            ]
        )
    }

    #[test]
    fn errors() {
        let source = "(+".to_string();
        let ast = parse_string(&source);
        assert!(ast.is_empty());

        let source = "+ 1 2".to_string();
        let ast = parse_string(&source);
        assert!(ast.is_empty());

        let source = "+1 2)".to_string();
        let ast = parse_string(&source);
        assert!(ast.is_empty());

        let source = "(+ 1 2 (1 2 3 4".to_string();
        let ast = parse_string(&source);
        assert!(ast.is_empty());

        let source = "(+ 1 2 3 4) (+ 1 2 3".to_string();
        let ast = parse_string(&source);
        assert_eq!(ast.len(), 1);
    }
}
