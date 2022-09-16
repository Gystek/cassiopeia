#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Delimiter {
    Bracket,
    Paren,
    Brace,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    Keyword(String),
    Identifier(String),
    Delimiter(Delimiter, bool),
    Operator(String),
}
