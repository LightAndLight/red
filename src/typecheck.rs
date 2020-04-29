use std::collections::HashMap;

use crate::ast::Rule;
use crate::ident::Ident;
use crate::path::{Path, Seg};
use crate::types::Type;

#[derive(Debug)]
enum TypeError<'a> {
    EmptyExpr,
    TypeMismatch{ expected: Type, actual: Type },
    Undefined{ ident: &'a Ident },
    ExpectedRecord{ got: Type },
    ExpectedList{ got: Type },
}

#[derive(Debug)]
pub struct Error<'a> {
    path: Path,
    error: TypeError<'a>,
}

#[derive(Debug)]
pub enum CheckResult<'a, A> {
    Success(A),
    Failure(Vec<Error<'a>>)
}

impl<'x, X> CheckResult<'x, X> {
    fn fail(error: Error<'x>) -> CheckResult<'x, X> {
        CheckResult::Failure(vec![error])
    }

    fn succeed(t: X) -> Self {
        CheckResult::Success(t)
    }

    fn map<B, F: FnOnce(X) -> B>(self, f: F) -> CheckResult<'x, B> {
        match self {
            CheckResult::Success(a) => CheckResult::Success(f(a)),
            CheckResult::Failure(errs) => CheckResult::Failure(errs)
        }
    }

    fn map2<A, B, F>(f: F, r1: CheckResult<'x, A>, r2: CheckResult<'x, B>) -> Self
    where
        F: FnOnce(A, B) -> X
    {
        match r1 {
            CheckResult::Success(a) =>
                match r2 {
                    CheckResult::Success(b) => CheckResult::Success(f(a, b)),
                    CheckResult::Failure(errs) => CheckResult::Failure(errs)
                },
            CheckResult::Failure(errs1) =>
                match r2 {
                    CheckResult::Success(_) => CheckResult::Failure(errs1),
                    CheckResult::Failure(errs2) =>
                        CheckResult::Failure(
                            errs1
                                .into_iter()
                                .chain(errs2.into_iter())
                                .collect()
                        )
                }
        }
    }

    fn and_then<B, F>(self, f: F) -> CheckResult<'x, B>
    where
        F: FnOnce(X) -> CheckResult<'x, B>,
    {
        match self {
            CheckResult::Success(t) => f(t),
            CheckResult::Failure(e) => CheckResult::Failure(e)
        }
    }
}

pub struct InferContext {
    path: Path,
    scope: HashMap<Ident, Type>
}

impl InferContext {
    fn fail<'a, A>(&self, error: TypeError<'a>) -> CheckResult<'a, A> {
        CheckResult::fail(Error{ path: self.path.clone(), error })
    }

    fn assert_type<'a>(&self, expected: Type, actual: Type) -> CheckResult<'a, ()> {
        if expected == actual {
            CheckResult::succeed(())
        } else {
            CheckResult::fail(
                Error{
                    path: self.path.clone(),
                    error: TypeError::TypeMismatch{ expected, actual },
                }
            )
        }
    }
    fn assert_bool<'a>(&self, received: Type) -> CheckResult<'a, ()> {
        self.assert_type(Type::Bool, received)
    }

    fn assert_number<'a>(&self, received: Type) -> CheckResult<'a, ()> {
        self.assert_type(Type::Number, received)
    }

    fn assert_text<'a>(&self, received: Type) -> CheckResult<'a, ()> {
        self.assert_type(Type::Text, received)
    }

    fn assert_record<'a, 'c>(&self, actual: Type) -> CheckResult<'a, HashMap<Ident, Type>> {
        match actual {
            Type::Record(fields) => CheckResult::succeed(fields),
            _ => CheckResult::fail(
                Error{
                    path: self.path.clone(),
                    error: TypeError::ExpectedRecord{ got: actual }
                }
            )
        }
    }

    fn assert_list<'a>(&self, actual: Type) -> CheckResult<'a, Type> {
        match actual {
            Type::List(ty) => CheckResult::succeed(*ty),
            _ => CheckResult::fail(
                Error{
                    path: self.path.clone(),
                    error: TypeError::ExpectedList{got: actual},
                }
            ),
        }
    }

    fn with_seg<A, F>(&mut self, segment: Seg, cont: F) -> A
    where F: FnOnce(&mut InferContext) -> A {
        self.path.push(segment);
        let res = cont(self);
        self.path.pop();
        res
    }

    fn with_scope<A, F>(&mut self, name: &Ident, ty: Type, cont: F) -> A
    where F: FnOnce(&mut InferContext) -> A {
        match self.scope.insert(name.clone(), ty) {
            Some(_) => panic!("Variable was shadowed. Use a better data structure."),
            None => ()
        }
        let res = cont(self);
        self.scope.remove(&name);
        res
    }

    pub fn infer_rule<'a>(&mut self, rule: &'a Rule) -> CheckResult<'a, Type> {
        match rule {
            Rule::Empty => self.fail(TypeError::EmptyExpr),
            Rule::LitBool{value: _} => CheckResult::succeed(Type::Bool),
            Rule::LitNumber{value: _} => CheckResult::succeed(Type::Number),
            Rule::LitText{value: _} => CheckResult::succeed(Type::Text),
            Rule::Not{not} =>
                self.with_seg(
                    Seg::Not,
                    |this|
                    this.infer_rule(not)
                        .and_then(|ty| this.assert_bool(ty))
                        .map(|()| Type::Bool)
                ),
            Rule::If{condition, consequence} => {
                let cond_result = self.with_seg(
                    Seg::IfCond,
                    |this|
                    this.infer_rule(condition)
                        .and_then(|ty| this.assert_bool(ty))
                );
                let cons_result = self.with_seg(
                    Seg::IfCond,
                    |this| this.infer_rule(consequence)
                );
                CheckResult::map2(|_, a| a, cond_result, cons_result)
            },
            Rule::Chain{value} =>
                value
                .iter()
                .enumerate()
                .fold(
                    CheckResult::succeed(Type::Unit),
                    |acc, (ix, rule)| {
                        let res = self.with_seg(
                            Seg::ChainN(ix as i32),
                            |this| this.infer_rule(rule)
                        );
                        CheckResult::map2(|_, a| a, acc, res)
                    }
                ),
            Rule::VariableRef{value} =>
                match self.scope.get(value) {
                    Some(a) => CheckResult::succeed(a.clone()),
                    None => 
                        self.fail(
                            TypeError::Undefined{ident: value}
                        )
                },
            Rule::RecordRef{record, identifier} => {
                let record_res = self.with_seg(
                    Seg::RecordRef,
                    |this|
                    this.infer_rule(record)
                        .and_then(|ty| this.assert_record(ty))
                );
                record_res.and_then(
                    |fields|
                    match fields.get(identifier) {
                        None => self.fail(
                            TypeError::Undefined{ident: identifier}
                        ),
                        Some(ty) => CheckResult::succeed(ty.clone())
                    }
                )
            },
            Rule::Foreach{var, list, body} => {
                let list_res = self.with_seg(
                    Seg::ForeachList,
                    |this|
                    this.infer_rule(list)
                        .and_then(|list_ty| this.assert_list(list_ty))
                );
                list_res.and_then(
                    |ty|
                    self.with_seg(
                        Seg::ForeachBody,
                        |this| this.with_scope(
                            var, ty,
                            |this| this.infer_rule(body)
                        )
                    )
                )
            }
        }
    }
}

