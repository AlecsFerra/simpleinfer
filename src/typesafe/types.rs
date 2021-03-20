use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter, Result};

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Int,
    Bool,
    String,
    Function(Box<Type>, Box<Type>),
    Unknown(u32),
}

impl Type {
    pub fn unknowns(&self) -> HashSet<u32> {
        match self {
            Type::Function(arg, ret) => arg.unknowns().add(&ret.unknowns()),
            Type::Unknown(id) => HashSet::singleton(*id),
            _ => HashSet::new(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Function(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Type::Unknown(id) => write!(f, "u{}", id),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}

/*impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Debug::Result {
        Display::fmt(self, f)
    }
}*/

// FUCK RUST AND YOUR NONSENSE RULES
trait KoolSet {
    type Element;
    fn singleton(elem: Self::Element) -> Self;
    fn add(&self, other: &Self) -> Self;
}

impl KoolSet for HashSet<u32> {
    type Element = u32;

    fn singleton(singleton: Self::Element) -> Self {
        let mut this = HashSet::new();
        this.insert(singleton);
        this
    }

    fn add(&self, other: &Self) -> Self {
        let mut acc = HashSet::new();
        for it in self.iter() {
            acc.insert(*it);
        }
        for it in other.iter() {
            acc.insert(*it);
        }
        acc
    }
}
