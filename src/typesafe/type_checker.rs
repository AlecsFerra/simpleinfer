use crate::expr::{Expression, Id};
use crate::typesafe::substitution::Substitution;
use crate::typesafe::type_environment::TypeEnvironment;
use crate::typesafe::types::Type;
use crate::typesafe::types::Type::{Bool, Function, Int, Unknown};

macro_rules! propagate_res_res {
    ($cl: expr) => {
        match $cl {
            Err(e) => return Err(e),
            Ok(e) => e,
        }
    };
}

macro_rules! propagate_opt_res {
    ($cl: expr) => {
        match ($cl) {
            Some(e) => return Err(e),
            _ => (),
        }
    };
}

macro_rules! propagate_opt_opt {
    ($cl: expr) => {
        match ($cl) {
            Some(e) => return Some(e),
            _ => (),
        }
    };
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Unify(Type, Type),
    Recursion(u32, Type),
    Unknown(Id),
}

pub struct TypeChecker {
    unknown_suppl: u32,
    substitution: Substitution,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            unknown_suppl: 0,
            substitution: Substitution::new(),
        }
    }

    fn substitute(&self, ty: Type) -> Type {
        self.substitution.apply(ty)
    }

    fn add_substitution(&mut self, id: u32, ty: Type) {
        self.substitution.set(id, ty)
    }

    pub fn clear(&mut self) {
        self.unknown_suppl = 0;
        self.substitution.clear();
    }

    fn fresh_unknown(&mut self) -> Type {
        let unknown = Type::Unknown(self.unknown_suppl);
        self.unknown_suppl += 1;
        return unknown;
    }

    pub fn infer(&mut self, expr: &Expression) -> Result<Type, TypeError> {
        let infer = propagate_res_res!(self.infer_env(expr, &TypeEnvironment::new()));
        println!("{:?}", self.substitution);
        let subst = self.substitute(infer);
        Ok(subst)
    }

    fn infer_env(&mut self, expr: &Expression, env: &TypeEnvironment) -> Result<Type, TypeError> {
        match expr {
            Expression::IntLiteral(_) => Ok(Int),
            Expression::BoolLiteral(_) => Ok(Bool),
            Expression::StringLiteral(_) => Ok(Type::String),
            Expression::Variable(id) => env.find(id),
            Expression::Let(id, expr, body) => {
                let ty_expr = propagate_res_res!(self.infer_env(expr, env));
                let body_env = env.extend(id.clone(), ty_expr);
                self.infer_env(body, &body_env)
            }
            Expression::Lambda(arg, body) => {
                let ty_arg = self.fresh_unknown();
                let body_env = env.extend(arg.clone(), ty_arg.clone());
                let ty_res = propagate_res_res!(self.infer_env(body, &body_env));
                Ok(Function(Box::new(ty_arg), Box::new(ty_res)))
            }
            Expression::Application(lambda, argument) => {
                let ty_argument = propagate_res_res!(self.infer_env(argument, env));
                let ty_lambda = propagate_res_res!(self.infer_env(lambda, env));
                let ty_return = self.fresh_unknown();

                let ty_fun = Function(Box::from(ty_argument), Box::from(ty_return.clone()));
                propagate_opt_res!(self.unify(ty_lambda, ty_fun));
                Ok(ty_return)
            }
            Expression::If(cond, then_case, else_cae) => {
                let ty_cond = propagate_res_res!(self.infer_env(cond, env));
                let ty_then = propagate_res_res!(self.infer_env(then_case, env));
                let ty_else = propagate_res_res!(self.infer_env(else_cae, env));

                propagate_opt_res!(self.unify(ty_cond, Bool));
                propagate_opt_res!(self.unify(ty_then.clone(), ty_else));
                Ok(ty_then)
            }
        }
    }

    fn unify(&mut self, ty1: Type, ty2: Type) -> Option<TypeError> {
        let ty1 = self.substitute(ty1);
        let ty2 = self.substitute(ty2);
        match (ty1, ty2) {
            (ty1, ty2) if ty1 == ty2 => None,
            (Function(arg1, ret1), Function(arg2, ret2)) => {
                propagate_opt_opt!(self.unify(*arg1, *arg2));
                propagate_opt_opt!(self.unify(*ret1, *ret2));
                None
            }
            (Unknown(id), ty2) => {
                if ty2.unknowns().contains(&id) {
                    Some(TypeError::Recursion(id, ty2))
                } else {
                    self.add_substitution(id, ty2);
                    None
                }
            }
            (ty1, Unknown(id)) => {
                if ty1.unknowns().contains(&id) {
                    Some(TypeError::Recursion(id, ty1))
                } else {
                    self.add_substitution(id, ty1);
                    None
                }
            }
            (ty1, ty2) => Some(TypeError::Unify(ty1, ty2)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::expr::Expression::{
        Application, BoolLiteral, If, IntLiteral, Lambda, Let, StringLiteral, Variable,
    };

    use super::*;

    #[test]
    fn infer_int_literal() {
        let mut tc = TypeChecker::new();
        let expr = IntLiteral(42);

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Int);
    }

    #[test]
    fn infer_bool_literal() {
        let mut tc = TypeChecker::new();
        let expr = BoolLiteral(false);

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Bool);
    }

    #[test]
    fn infer_string_literal() {
        let mut tc = TypeChecker::new();
        let expr = StringLiteral(String::new());

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Type::String);
    }

    #[test]
    fn variable_found() {
        let mut tc = TypeChecker::new();
        let env = TypeEnvironment::new().extend(String::from("x"), Int);
        let expr = Variable(String::from("x"));

        let inferred = tc.infer_env(&expr, &env);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Int);
    }

    #[test]
    fn variable_not_found() {
        let mut tc = TypeChecker::new();
        let expr = Variable(String::from("x"));

        let inferred = tc.infer(&expr);
        assert!(inferred.is_err());
        assert_eq!(inferred.unwrap_err(), TypeError::Unknown(String::from("x")));
    }

    #[test]
    fn simple_let() {
        let mut tc = TypeChecker::new();
        let expr = Let(
            String::from("x"),
            Box::from(IntLiteral(12)),
            Box::from(BoolLiteral(false)),
        );

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Bool);
    }

    #[test]
    fn variable_let() {
        let mut tc = TypeChecker::new();
        let expr = Let(
            String::from("x"),
            Box::from(IntLiteral(12)),
            Box::from(Variable(String::from("x"))),
        );

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Int);
    }

    #[test]
    fn lambda() {
        let mut tc = TypeChecker::new();
        let expr = Lambda(
            String::from("x"),
            Box::from(Lambda(
                String::from("y"),
                Box::from(Variable(String::from("x"))),
            )),
        );

        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(
            inferred.unwrap(),
            Function(
                Box::from(Unknown(0)),
                Box::from(Function(Box::from(Unknown(1)), Box::from(Unknown(0))))
            )
        );
    }

    #[test]
    fn lambda_shadow() {
        let mut tc = TypeChecker::new();
        let expr = Lambda(
            String::from("x"),
            Box::from(Lambda(
                String::from("x"),
                Box::from(Variable(String::from("x"))),
            )),
        );
        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(
            inferred.unwrap(),
            Function(
                Box::from(Unknown(0)),
                Box::from(Function(Box::from(Unknown(1)), Box::from(Unknown(1))))
            )
        );
    }

    #[test]
    fn high_order() {
        let mut tc = TypeChecker::new();

        let flip_str = String::from("flip");
        let flip_var = Variable(flip_str.clone());

        let f_str = String::from("f");
        let f_var = Variable(f_str.clone());

        let x_str = String::from("x");
        let x_var = Variable(x_str.clone());

        let y_str = String::from("y");
        let y_var = Variable(y_str.clone());

        let const_str = String::from("const");
        let const_var = Variable(const_str.clone());

        let flip_fn = Lambda(
            f_str.clone(),
            Box::from(Lambda(
                x_str.clone(),
                Box::from(Lambda(
                    y_str.clone(),
                    Box::from(Application(
                        Box::from(Application(
                            Box::from(f_var.clone()),
                            Box::from(y_var.clone()),
                        )),
                        Box::from(x_var.clone()),
                    )),
                )),
            )),
        );
        let flip_infer = tc.infer(&flip_fn.clone());
        tc.clear();
        assert!(flip_infer.is_ok());
        assert_eq!(
            flip_infer.unwrap(),
            Function(
                Box::from(Function(
                    Box::from(Unknown(2)),
                    Box::from(Function(Box::from(Unknown(1)), Box::from(Unknown(4))))
                )),
                Box::from(Function(
                    Box::from(Unknown(1)),
                    Box::from(Function(Box::from(Unknown(2)), Box::from(Unknown(4))))
                )),
            )
        );

        let const_fn = Lambda(
            x_str.clone(),
            Box::from(Lambda(y_str.clone(), Box::from(x_var.clone()))),
        );
        let const_infer = tc.infer(&const_fn.clone());
        tc.clear();
        assert!(const_infer.is_ok());
        assert_eq!(
            const_infer.unwrap(),
            Function(
                Box::from(Unknown(0)),
                Box::from(Function(Box::from(Unknown(1)), Box::from(Unknown(0))))
            )
        );

        let expr = Let(
            flip_str.clone(),
            Box::from(flip_fn),
            Box::from(Let(
                const_str.clone(),
                Box::from(const_fn),
                Box::from(Application(
                    Box::from(Application(
                        Box::from(Application(
                            Box::from(flip_var.clone()),
                            Box::from(const_var.clone()),
                        )),
                        Box::from(IntLiteral(5)),
                    )),
                    Box::from(BoolLiteral(true)),
                )),
            )),
        );
        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Bool);
    }

    #[test]
    fn recursive_type() {}

    #[test]
    fn lambda_and_if() {
        let mut tc = TypeChecker::new();
        let expr = Let(
            String::from("x"),
            Box::from(Lambda(
                String::from("x"),
                Box::from(Variable(String::from("x"))),
            )),
            Box::from(If(
                Box::from(Application(
                    Box::from(Variable(String::from("x"))),
                    Box::from(BoolLiteral(true)),
                )),
                Box::from(IntLiteral(23)),
                Box::from(IntLiteral(43)),
            )),
        );
        let inferred = tc.infer(&expr);
        assert!(inferred.is_ok());
        assert_eq!(inferred.unwrap(), Int);
    }
}
