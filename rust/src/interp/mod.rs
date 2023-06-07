use self::{
    env::{Env, GEnv, LEnv},
    value::{Function, Value},
};
use crate::parser::ast::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block {
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        let locals = env.locals.extend(self.locals.iter(), std::iter::empty());
        let env = &mut Env {
            locals,
            globals: env.globals,
        };
        self.body.interp(env);
        self.ret.interp(env)
    }
}

impl Stat_ {
    // Interprétation d'une instruction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> () {
        match self {
            Stat_::Nop => (),
            Stat_::Seq(s1, s2) => {
                s1.interp(env);
                s2.interp(env)
            }
            Stat_::Assign(v, e) => match v {
                Var::Name(name) => {
                    let value = e.interp(env);
                    env.set(name, value);
                }
                Var::IndexTable(table, key) => {
                    let table = table.interp(env).as_table();
                    let key = key.interp(env).as_table_key();
                    let value = e.interp(env);
                    table.as_ref().borrow_mut().insert(key, value);
                }
            },
            Stat_::StatFunctionCall(fc) => {
                fc.interp(env);
            }
            Stat_::WhileDoEnd(cond, body) => {
                while cond.interp(env).as_bool() {
                    body.interp(env);
                }
            }
            Stat_::If(cond, then_do, else_do) => {
                if cond.interp(env).as_bool() {
                    then_do.interp(env);
                } else {
                    else_do.interp(env);
                }
            }
        }
    }
}

impl FunctionCall {
    // Interprétation d'un appel de fonction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        match self.0.interp(env).as_function() {
            Function::Print => {
                let strings = self.1.iter().map(|e| e.interp(env).to_string());
                println!("{}", strings.collect::<Vec<_>>().join("\t"));
                Value::Nil
            }
            Function::Closure(args, locals, block) => {
                let locals = locals.extend(args.iter(), self.1.iter().map(|e| e.interp(env)));
                let env = &mut Env {
                    locals,
                    globals: env.globals,
                };
                block.interp(env)
            }
        }
    }
}

impl Exp_ {
    // Interprétation d'une expression
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        match self {
            Exp_::Nil => Value::Nil,
            Exp_::False => Value::Bool(false),
            Exp_::True => Value::Bool(true),
            Exp_::Number(i) => Value::Number(*i),
            Exp_::LiteralString(s) => Value::String(s.clone()),
            Exp_::Var(v) => match v {
                Var::Name(name) => env.lookup(name),
                Var::IndexTable(table, key) => table
                    .interp(env)
                    .as_table()
                    .borrow()
                    .get(&key.interp(env).as_table_key())
                    .unwrap_or_else(|| &Value::Nil)
                    .clone(),
            },
            Exp_::ExpFunctionCall(fc) => fc.interp(env),
            Exp_::FunctionDef(def) => {
                Value::Function(Function::Closure(&def.0, env.locals.clone(), &def.1))
            }
            Exp_::BinOp(op, e1, e2) => match op {
                BinOp::Addition => Value::add(e1.interp(env), e2.interp(env)),
                BinOp::Subtraction => Value::sub(e1.interp(env), e2.interp(env)),
                BinOp::Multiplication => Value::mul(e1.interp(env), e2.interp(env)),
                BinOp::Equality => Value::Bool(Value::eq(&e1.interp(env), &e2.interp(env))),
                BinOp::Less => Value::Bool(Value::lt(e1.interp(env), e2.interp(env))),
                BinOp::LessEq => Value::Bool(Value::le(e1.interp(env), e2.interp(env))),
                BinOp::Greater => Value::Bool(!Value::le(e1.interp(env), e2.interp(env))),
                BinOp::GreaterEq => Value::Bool(!Value::lt(e1.interp(env), e2.interp(env))),
                BinOp::LogicalAnd => {
                    let v1 = e1.interp(env);
                    if v1.as_bool() {
                        e2.interp(env)
                    } else {
                        v1
                    }
                }
                BinOp::LogicalOr => {
                    let v1 = e1.interp(env);
                    if v1.as_bool() {
                        v1
                    } else {
                        e2.interp(env)
                    }
                }
                BinOp::Inequality => Value::Bool(!Value::eq(&e1.interp(env), &e2.interp(env))),
            },
            Exp_::UnOp(op, expr) => match op {
                UnOp::UnaryMinus => Value::neg(expr.interp(env)),
                UnOp::Not => {
                    if expr.interp(env).as_bool() {
                        Value::Bool(false)
                    } else {
                        Value::Bool(true)
                    }
                }
            },
            Exp_::Table(key_value) => {
                let mut table = HashMap::new();
                for (key, value) in key_value {
                    table.insert(key.interp(env).as_table_key(), value.interp(env));
                }
                Value::Table(Rc::new(RefCell::new(table)))
            }
        }
    }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block) {
    let mut globals = GEnv(HashMap::new());
    let printid = "print".to_owned();
    globals.0.insert(&printid, Value::Function(Function::Print));
    let mut env = Env {
        locals: Rc::new(LEnv::Nil),
        globals: &mut globals,
    };
    ast.interp(&mut env);
}
