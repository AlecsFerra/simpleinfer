pub type Id = String;

#[derive(Clone, Debug)]
pub enum Expression {
    IntLiteral(i32),
    BoolLiteral(bool),
    StringLiteral(String),
    Variable(Id),
    Let(Id, Box<Expression>, Box<Expression>),
    Lambda(Id, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}
