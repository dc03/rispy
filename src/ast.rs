#[derive(Debug, PartialEq)]
pub enum TokenType {
    ParenOpen,
    ParenClose,
    LBraceOpen,
    LBraceClose,

    Number,
    Symbol,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub byte_start: usize,
    pub byte_end: usize,
    pub line: usize,
    pub column: usize,
    pub ttype: TokenType,
}

pub enum Expr<'a> {
    SExpr(SExpr<'a>),
    QExpr(QExpr<'a>),
    Num(Number<'a>),
    Oper(Symbol<'a>),
}

pub struct SExpr<'a> {
    pub func: Box<Expr<'a>>,
    pub args: Vec<Box<Expr<'a>>>,
}

pub struct QExpr<'a> {
    pub func: Box<Expr<'a>>,
    pub args: Vec<Box<Expr<'a>>>,
}

pub struct Number<'a> {
    pub tok: Token<'a>,
    pub val: isize,
}

pub struct Symbol<'a> {
    pub tok: Token<'a>,
}