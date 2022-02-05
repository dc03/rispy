use std::collections::HashMap;
use std::io::Write;

use crate::ast::*;
use crate::error::*;

type Native<'a, 'b> = fn(&mut Executor<'a, 'b>, Vec<Value<'a, 'b>>) -> Option<Value<'a, 'b>>;
type ExprPtr<'a> = Box<Expr<'a>>;
// type Exprs<'a> = Vec<ExprPtr<'a>>;

#[derive(Clone, PartialEq)]
struct FunctionFrame<'a, 'b> {
    params: Vec<String>,
    body: Vec<&'b ExprPtr<'a>>,
}

#[derive(Clone)]
enum Value<'a, 'b> {
    Integer(isize),
    Boolean(bool),
    Function(FunctionFrame<'a, 'b>),
    List(Vec<&'b Box<Expr<'a>>>),
    Native(Native<'a, 'b>),
    Nil,
}

pub struct Executor<'a, 'b> {
    envs: Vec<HashMap<String, Value<'a, 'b>>>,
    natives: HashMap<String, Native<'a, 'b>>,
}

impl<'a, 'b> Executor<'a, 'b> {
    pub fn new() -> Self {
        Executor {
            envs: Default::default(),
            natives: HashMap::from([
                ("+".to_string(), native_plus as Native<'a, 'b>),
                ("-".to_string(), native_minus as Native<'a, 'b>),
                ("*".to_string(), native_mul as Native<'a, 'b>),
                ("/".to_string(), native_div as Native<'a, 'b>),
                ("%".to_string(), native_mod as Native<'a, 'b>),
                ("=".to_string(), native_equals as Native<'a, 'b>),
                ("<".to_string(), native_lesser as Native<'a, 'b>),
                (">".to_string(), native_greater as Native<'a, 'b>),
                ("print".to_string(), native_print as Native<'a, 'b>),
                ("define".to_string(), native_define as Native<'a, 'b>),
                ("cond".to_string(), native_cond as Native<'a, 'b>),
                ("eval".to_string(), native_eval as Native<'a, 'b>),
                ("cons".to_string(), native_cons as Native<'a, 'b>),
                ("car".to_string(), native_car as Native<'a, 'b>),
                ("cdr".to_string(), native_cdr as Native<'a, 'b>),
            ]),
        }
    }

    fn lookup_symbol(&mut self, symbol: &str) -> Option<&Value<'a, 'b>> {
        for env in self.envs.iter().rev() {
            if env.contains_key(symbol) {
                return env.get(symbol);
            }
        }
        None
    }

    fn execute_sexpr(&mut self, sexpr: &'b SExpr<'a>) -> Option<Value<'a, 'b>> {
        let func = self.execute_one(&sexpr.func);

        let args = sexpr
            .args
            .iter()
            .map(|expr| self.execute_one(expr))
            .collect::<Vec<Option<Value<'a, 'b>>>>();

        if args.iter().any(|value| value.is_none()) {
            return None;
        }

        let args = args
            .iter()
            .map(|value| value.as_ref().unwrap().clone())
            .collect::<Vec<Value<'a, 'b>>>();

        if let Some(func) = func {
            match func {
                Value::Function(f) => {
                    self.envs.push(Default::default());
                    for (param, arg) in f.params.iter().zip(args.into_iter()) {
                        self.envs.last_mut().unwrap().insert(param.clone(), arg);
                    }

                    for i in 0..(f.body.len() - 1) {
                        self.execute_one(f.body[i]);
                    }
                    let result = self.execute_one(f.body.last().unwrap());
                    self.envs.pop();
                    result
                }
                Value::Native(n) => n(self, args),
                _ => {
                    runtime_error("Value being called is not a function".to_string());
                    None
                }
            }
        } else {
            None
        }
    }

    fn execute_one(&mut self, expr: &'b ExprPtr<'a>) -> Option<Value<'a, 'b>> {
        match &**expr {
            Expr::SExpr(s) => self.execute_sexpr(s),
            Expr::QExpr(q) => {
                let mut exprs = vec![&q.func];
                exprs.append(&mut q.args.iter().collect::<Vec<&Box<Expr<'a>>>>());
                Some(Value::List(exprs))
            }
            Expr::Num(n) => Some(Value::Integer(n.val)),
            Expr::Sym(s) => {
                if let Some(v) = self.lookup_symbol(s.tok.lexeme) {
                    Some(v.clone())
                } else if self.natives.contains_key(s.tok.lexeme) {
                    Some(Value::Native(*self.natives.get(s.tok.lexeme).unwrap()))
                } else {
                    runtime_error(format!("No such symbol `{}` defined", s.tok.lexeme));
                    None
                }
            }
        }
    }

    pub fn execute(&mut self, prog: &Vec<&'b ExprPtr<'a>>) {
        self.envs.push(Default::default());
        self.envs
            .last_mut()
            .unwrap()
            .insert("#t".to_string(), Value::Boolean(true));
        self.envs
            .last_mut()
            .unwrap()
            .insert("#f".to_string(), Value::Boolean(false));
        self.envs
            .last_mut()
            .unwrap()
            .insert("nil".to_string(), Value::Nil);

        for expr in prog {
            if let None = self.execute_one(&expr) {
                break;
            }
        }
    }
}

macro_rules! make_arithmetic_function {
    ($fname:ident, $begin:expr, $op:expr, $name:literal) => {
        fn $fname<'a, 'b>(
            _: &mut Executor<'a, 'b>,
            args: Vec<Value<'a, 'b>>,
        ) -> Option<Value<'a, 'b>> {
            let mut result = $begin;

            for value in args {
                match value {
                    Value::Integer(n) => {
                        if let None = $op(&mut result, n) {
                            return None;
                        }
                    }
                    _ => {
                        runtime_error(format!("Incorrect argument for `{}`", $name));
                        return None;
                    }
                }
            }

            Some(Value::Integer(result))
        }
    };
}

make_arithmetic_function! {native_plus, 0, |result: &mut isize, n: isize| { *result += n; Some(Value::Nil) }, "+"}
make_arithmetic_function! {native_minus, 0, |result: &mut isize, n: isize| {
    if *result == 0 {
        *result = n;
    } else {
        *result -= n;
    }
    Some(Value::Nil)
}, "-"}
make_arithmetic_function! {native_mul, 1, |result: &mut isize, n: isize| { *result *= n; Some(Value::Nil) }, "*"}
make_arithmetic_function! {native_div, 0, |result: &mut isize, n: isize| {
    if *result == 0 {
        *result = n;
        Some(Value::Nil)
    } else if n == 0 {
        runtime_error("Cannot divide by zero".to_string());
        None
    } else {
        *result /= n;
        Some(Value::Nil)
    }
}, "/"}
make_arithmetic_function! {native_mod, 0, |result: &mut isize, n: isize| {
    if *result == 0 {
        *result = n;
        Some(Value::Nil)
    } else if n == 0 {
        runtime_error("Cannot compute modulo by zero".to_string());
        None
    } else if n < 0 {
        runtime_error("Cannot compute modulo using negative value".to_string());
        None
    } else if result < &mut 0 {
        runtime_error("Cannot compute modulo of negative value".to_string());
        None
    } else {
        *result %= n;
        Some(Value::Nil)
    }
}, "%"}

macro_rules! make_logical_function {
    ($fname:ident, $op:expr, $name:literal) => {
        fn $fname<'a, 'b>(
            _: &mut Executor<'a, 'b>,
            args: Vec<Value<'a, 'b>>,
        ) -> Option<Value<'a, 'b>> {
            if args.len() != 2 {
                runtime_error(
                    "Incorrect number of arguments for logical comparison, require 2".to_string(),
                );
                return None;
            }
            if let Some(v) = $op(args[0].clone(), args[1].clone()) {
                Some(Value::Boolean(v))
            } else {
                None
            }
        }
    };
}

make_logical_function! {native_equals, |arg1: Value, arg2: Value| {
    Some(match (arg1, arg2) {
        (Value::Integer(n1), Value::Integer(n2)) => n1 == n2,
        (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
        (Value::Function(f1), Value::Function(f2)) => f1 == f2,
        (Value::Native(n1), Value::Native(n2)) => n1 as *const Native == n2 as *const Native,
        (Value::List(l1), Value::List(l2)) => l1 == l2,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    })
}, "="}
make_logical_function! {native_lesser, |arg1: Value, arg2: Value| {
    match (arg1, arg2) {
        (Value::Integer(n1), Value::Integer(n2)) => Some(n1 < n2),
        (Value::Boolean(b1), Value::Boolean(b2)) => Some(b1 < b2),
        (Value::Function(_), Value::Function(_)) => { runtime_error("Cannot compare functions using `<`".to_string()); None },
        (Value::Native(_), Value::Native(_)) => { runtime_error("Cannot compare functions using `<`".to_string()); None },
        (Value::List(_), Value::List(_)) => { runtime_error("Cannot compare lists using `<`".to_string()); None },
        (Value::Nil, Value::Nil) => Some(false),
        _ => None,
    }
}, "<"}
make_logical_function! {native_greater, |arg1: Value, arg2: Value| {
    match (arg1, arg2) {
        (Value::Integer(n1), Value::Integer(n2)) => Some(n1 > n2),
        (Value::Boolean(b1), Value::Boolean(b2)) => Some(b1 > b2),
        (Value::Function(_), Value::Function(_)) => { runtime_error("Cannot compare functions using `>`".to_string()); None },
        (Value::Native(_), Value::Native(_)) => { runtime_error("Cannot compare functions using `>`".to_string()); None },
        (Value::List(_), Value::List(_)) => { runtime_error("Cannot compare lists using `>`".to_string()); None },
        (Value::Nil, Value::Nil) => Some(false),
        _ => None,
    }
}, ">"}

fn print_expr<'a>(expr: &Box<Expr<'a>>) {
    match &**expr {
        Expr::SExpr(s) => {
            print!("(");
            print_expr(&s.func);
            print!(" ");
            for arg in &s.args {
                print_expr(arg);
                print!(" ");
            }
            print!(")");
        }
        Expr::QExpr(q) => {
            print!("{{");
            print_expr(&q.func);
            print!(" ");
            for arg in &q.args {
                print_expr(arg);
                print!(" ");
            }
            print!("}}");
        }
        Expr::Num(n) => {
            print!("{}", n.tok.lexeme);
        }
        Expr::Sym(s) => {
            print!("{}", s.tok.lexeme);
        }
    }
    std::io::stdout().flush().expect("Could not flush stdout");
}

fn native_print<'a, 'b>(
    _: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.is_empty() {
        runtime_error("Cannot print nothing".to_string());
        None
    } else {
        for arg in args {
            match arg {
                Value::Integer(n) => println!("{}", n),
                Value::Boolean(b) => println!("{}", b),
                Value::Function(_) => println!("Function"),
                Value::List(l) => {
                    print!("{{");
                    for arg in 0..(l.len() - 1) {
                        print_expr(&l[arg]);
                        print!(" ");
                    }
                    print_expr(l.last()?);
                    println!("}}")
                }
                Value::Native(_) => println!("Native function"),
                Value::Nil => println!("nil"),
            }
        }
        Some(Value::Nil)
    }
}

fn native_define<'a, 'b>(
    this: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.len() != 2 {
        runtime_error("Define takes only two parameters".to_string());
        None
    } else {
        match args.as_slice() {
            [Value::List(func), Value::List(body)] => {
                let mut symbols = vec![];
                for value in func {
                    match ***value {
                        Expr::Sym(ref s) => {
                            symbols.push(s.tok.lexeme);
                        }
                        _ => {
                            runtime_error("Define takes names to define".to_string());
                            return None;
                        }
                    };
                }

                if symbols.len() < 1 {
                    runtime_error("At least one name should be given in define".to_string());
                    None
                } else {
                    let (name, params) = symbols.split_at(1);
                    this.envs.last_mut().unwrap().insert(
                        name[0].to_string(),
                        Value::Function(FunctionFrame {
                            params: params.iter().map(|s| s.to_string()).collect(),
                            body: body.clone(),
                        }),
                    );
                    Some(Value::Nil)
                }
            }
            [Value::List(ref func), Value::Integer(body)] => {
                let mut symbols = vec![];
                for value in func {
                    match ***value {
                        Expr::Sym(ref s) => {
                            symbols.push(s.tok.lexeme);
                        }
                        _ => {
                            runtime_error("Define takes a name to define".to_string());
                            return None;
                        }
                    };
                }

                if symbols.len() != 1 {
                    runtime_error("A variable can only be defined to a value".to_string());
                    None
                } else {
                    this.envs
                        .last_mut()
                        .unwrap()
                        .insert(symbols[0].to_string(), Value::Integer(*body));
                    Some(Value::Nil)
                }
            }
            _ => {
                runtime_error("Define takes a name to define and its value".to_string());
                None
            }
        }
    }
}

fn native_cond<'a, 'b>(
    this: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.is_empty() {
        runtime_error("Cond requires at least one alternative".to_string());
        None
    } else {
        for arg in args {
            match arg {
                Value::List(ref inner) => {
                    if inner.len() < 2 {
                        runtime_error("Incorrect number of arguments in cond arm".to_string());
                        return None;
                    }

                    let cond = inner[0];
                    let body = &inner[1..];
                    for expr in body {
                        match expr.as_ref() {
                            Expr::SExpr(_) => {}
                            Expr::Num(_) => {}
                            Expr::Sym(_) => {}
                            _ => {
                                runtime_error("Expected sexpr as the body of cond arm".to_string());
                                return None;
                            }
                        }
                    }
                    match cond.as_ref() {
                        Expr::SExpr(_) => {
                            let result = this.execute_one(cond)?;
                            match result {
                                Value::Boolean(b) => {
                                    if b {
                                        for i in 0..(body.len() - 1) {
                                            this.execute_one(body[i]);
                                        }
                                        return this.execute_one(body.last()?);
                                    }
                                }
                                _ => {
                                    runtime_error(
                                        "Expected boolean value for condition".to_string(),
                                    );
                                    return None;
                                }
                            }
                        }
                        Expr::Sym(s) => {
                            if s.tok.lexeme == "else" {
                                for i in 0..(body.len() - 1) {
                                    this.execute_one(body[i]);
                                }
                                return this.execute_one(body.last()?);
                            }
                        }
                        _ => {
                            runtime_error(
                                "Incorrect condition in cond arm, expected sexpr or `else`"
                                    .to_string(),
                            );
                            return None;
                        }
                    }
                }
                _ => {
                    runtime_error("Incorrect argument to cond".to_string());
                    return None;
                }
            }
        }

        Some(Value::Nil)
    }
}

fn native_eval<'a, 'b>(
    this: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.len() != 1 {
        runtime_error("Incorrect number of arguments to `eval`".to_string());
        return None;
    } else {
        let expr = &args[0];
        match expr {
            Value::List(body) => {
                if body.is_empty() {
                    runtime_error("Cannot `eval` empty body".to_string());
                    return None;
                }
                for i in 0..(body.len() - 1) {
                    this.execute_one(body[i]);
                }
                return this.execute_one(body.last()?);
            }
            _ => {
                runtime_error("Incorrect argument to `eval`".to_string());
                return None;
            }
        }
    }
}

fn native_cons<'a, 'b>(
    _: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.len() != 2 {
        runtime_error("Incorrect number of arguments to `cons`".to_string());
        None
    } else {
        let value = &args[0];
        let rest = &args[1];
        match value {
            Value::List(value) => match rest {
                Value::List(l) => {
                    let mut l = l.clone();
                    let mut v = value.clone();
                    v.append(&mut l);
                    Some(Value::List(v))
                }
                Value::Nil => Some(Value::List(value.clone())),
                _ => {
                    runtime_error("Incorrect argument to `cons`".to_string());
                    None
                }
            },
            _ => {
                runtime_error("Cannot pass non-list as first argument to `cons`".to_string());
                return None;
            }
        }
    }
}

fn native_car<'a, 'b>(
    this: &mut Executor<'a, 'b>,
    args: Vec<Value<'a, 'b>>,
) -> Option<Value<'a, 'b>> {
    if args.len() != 1 {
        runtime_error("`car` only takes a list".to_string());
        None
    } else {
        match &args[0] {
            Value::List(l) => {
                if l.is_empty() {
                    runtime_error("Empty argument to `car`".to_string());
                    None
                } else {
                    Some(this.execute_one(l[0])?)
                }
            }
            _ => {
                runtime_error("Expected list as argument to `car`".to_string());
                None
            }
        }
    }
}

fn native_cdr<'a, 'b>(_: &mut Executor<'a, 'b>, args: Vec<Value<'a, 'b>>) -> Option<Value<'a, 'b>> {
    if args.len() != 1 {
        runtime_error("`cdr` only takes a list".to_string());
        None
    } else {
        let l = &args[0];
        match l {
            Value::List(l) => {
                if l.is_empty() {
                    Some(Value::Nil)
                } else {
                    Some(Value::List(l[1..].to_vec()))
                }
            }
            _ => {
                runtime_error("`cdr` requires list as argument".to_string());
                None
            }
        }
    }
}
