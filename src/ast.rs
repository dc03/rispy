#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    ParenOpen,
    ParenClose,
    LBraceOpen,
    LBraceClose,

    Number,
    Symbol,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub byte_start: usize,
    pub byte_end: usize,
    pub line: usize,
    pub column: usize,
    pub ttype: TokenType,
}

impl<'a> Token<'a> {
    pub fn from_pos(
        lexeme: &'a str,
        pos: usize,
        line: usize,
        column: usize,
        ttype: TokenType,
    ) -> Self {
        Token {
            lexeme: lexeme,
            byte_start: pos - lexeme.len(),
            byte_end: pos,
            line: line,
            column: column,
            ttype: ttype,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    SExpr(SExpr<'a>),
    QExpr(QExpr<'a>),
    Num(Number<'a>),
    Sym(Symbol<'a>),
}

#[derive(Debug, PartialEq)]
pub struct SExpr<'a> {
    pub func: Box<Expr<'a>>,
    pub args: Vec<Box<Expr<'a>>>,
    pub tok: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub struct QExpr<'a> {
    pub func: Box<Expr<'a>>,
    pub args: Vec<Box<Expr<'a>>>,
    pub tok: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Number<'a> {
    pub tok: Token<'a>,
    pub val: isize,
}

#[derive(Debug, PartialEq)]
pub struct Symbol<'a> {
    pub tok: Token<'a>,
}
