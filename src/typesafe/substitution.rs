use crate::typesafe::types::Type;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Substitution {
    substitutions: HashMap<u32, Type>,
}

impl Substitution {
    pub fn new() -> Self {
        Substitution {
            substitutions: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.substitutions.clear()
    }

    pub fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Unknown(id) => self
                .substitutions
                .get(&id)
                .map(|x| self.apply(x.clone()))
                .unwrap_or(ty)
                .clone(),
            Type::Function(arg, ret) => {
                Type::Function(Box::new(self.apply(*arg)), Box::new(self.apply(*ret)))
            }
            _ => ty,
        }
    }

    pub fn set(&mut self, id: u32, ty: Type) {
        self.substitutions.insert(id, ty);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::typesafe::types::Type::{Bool, Function, Int, Unknown};

    #[test]
    fn substitute_in_fn() {
        let mut subst = Substitution::new();
        subst.set(0, Function(Box::from(Unknown(9)), Box::from(Unknown(10))));
        subst.set(1, Function(Box::from(Unknown(11)), Box::from(Unknown(12))));

        let to_sub = Function(Box::from(Unknown(0)), Box::from(Unknown(1)));
        let substituted = subst.apply(to_sub);

        assert_eq!(
            substituted,
            Function(
                Box::from(Function(Box::from(Unknown(9)), Box::from(Unknown(10)))),
                Box::from(Function(Box::from(Unknown(11)), Box::from(Unknown(12))))
            )
        );

        subst.set(10, Int);
        subst.set(1, Bool);

        let substituted = subst.apply(substituted);
        assert_eq!(
            substituted,
            Function(
                Box::from(Function(Box::from(Unknown(9)), Box::from(Int))),
                Box::from(Function(Box::from(Unknown(11)), Box::from(Unknown(12))))
            )
        );
    }
}
