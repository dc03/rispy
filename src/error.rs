const ERROR: &str = "ERROR";

fn print_base(
    msg: String,
    source: &String,
    line: usize,
    col: usize,
    filename: &str,
    type_: &'static str,
) {
    println!("[{}]: {}", type_, msg);
    println!(" --> in {}:{}:{}", filename, line, col);

    let mut lines = source.lines();
    println!(" {} | {}", line, lines.nth(line - 1).unwrap());
}

pub fn print_error(msg: String, source: &String, line: usize, col: usize, filename: &str) {
    print_base(msg, source, line, col, filename, ERROR);
}

pub fn print_generic_error(msg: String, filename: &str) {
    println!("[{}]: {}", ERROR, msg);
    println!(" --> in {}", filename);
}

pub fn runtime_error(msg: String) {
    println!("[RUNTIME ERROR]: {}", msg);
}
