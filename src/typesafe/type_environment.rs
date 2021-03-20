use crate::expr::Id;
use crate::typesafe::type_checker::TypeError;
use crate::typesafe::types::Type;
use std::collections::HashMap;

pub struct TypeEnvironment {
    env: HashMap<Id, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn extend(&self, id: Id, ty: Type) -> TypeEnvironment {
        let mut new_env = self.env.clone();
        new_env.insert(id, ty);
        TypeEnvironment { env: new_env }
    }

    pub fn find(&self, id: &Id) -> Result<Type, TypeError> {
        self.env
            .get(id)
            .map(|x| x.clone())
            .ok_or(TypeError::Unknown(id.clone()))
    }
}
