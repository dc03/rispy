use std::env;

mod ast;
mod executor;
mod parser;
mod scanner;

mod error;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} [FILE]", args[0]);
        return ();
    }

    let source_file = &args[1];
    let source = std::fs::read_to_string(source_file).expect("Could not read file");

    let mut scanner = scanner::Scanner::new(&source, "test");
    let mut toks = vec![];
    while let Some(tok) = scanner.scan_one() {
        toks.push(tok);
    }

    let mut parser = parser::Parser::new(toks, &source, "test");
    let ast = parser.parse();
    let ast = ast
        .into_iter()
        .map(|sexpr| Box::new(ast::Expr::SExpr(*sexpr)))
        .collect::<Vec<Box<ast::Expr>>>();

    let astref = ast.iter().collect();

    let mut executor = executor::Executor::new();
    executor.execute(&astref);
}
