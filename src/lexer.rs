use crate::errors::Result;
use crate::tokens::{Delimiter, Token};
use crate::utils::Located;
use std::iter::Peekable;
use std::str::Chars;

macro_rules! setter {
    ($p:ident, $t:ty) => {
        pub fn $p(mut self, x: $t) -> Lexer<'a> {
            self.$p = x;

            self
        }
    };
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,

    file: &'a str,
    line: usize,
    col: usize,

    _line: usize,
    _col: usize,

    operators: &'a [&'a str],
    delimiters: &'a [(Delimiter, char, char)],
    keywords: &'a [&'a str],
}

const TERMINATORS: [char; 4] = [' ', '\n', '\t', '\r'];

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Self {
            input: input.chars().peekable(),

            file: "none",
            line: 1,
            col: 0,

            _line: 1,
            _col: 0,

            operators: &[],
            delimiters: &[],
            keywords: &[],
        }
    }
    setter!(file, &'a str);
    setter!(operators, &'a [&'a str]);
    setter!(delimiters, &'a [(Delimiter, char, char)]);
    setter!(keywords, &'a [&'a str]);
    fn peek(&mut self) -> Option<char> {
        self.input.peek().map(|x| *x)
    }

    fn pop(&mut self) -> Result<char> {
        let c = self.input.next().map_or(
            error!(
                self.file.to_string(),
                (self._line, self._col),
                (self.line, self.col),
                "unexpected end-of-file while lexing.",
            ),
            Ok,
        )?;

        self.col += 1;

        if c == '\n' {
            self.line += 1;
            self.col = 0;
        }

        Ok(c)
    }

    fn expect(&mut self, c: char) -> Result<char> {
        self.pop().and_then(|x| {
            if x == c {
                Ok(c)
            } else {
                error!(
                    self.file.to_string(),
                    (self._line, self._col),
                    (self.line, self.col),
                    "expected a `{}' character, but found a `{}' character.",
                    c,
                    x
                )
            }
        })
    }

    fn eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn is_next(&mut self, f: impl Fn(char) -> bool) -> bool {
        self.peek().map_or(false, f)
    }

    fn record(&mut self) {
        self._line = self.line;
        self._col = self.col;
    }

    fn number(&mut self) -> Result<Token> {
        let mut v_in = 0i64;
        let mut v_fl = 0f64;
        let mut x: i32 = 0;

        while self.is_next(|x| x.is_digit(10)) {
            v_in *= 10;
            v_in += self.pop().unwrap().to_string().parse::<i64>().unwrap();
        }

        if self.is_next(|x| x == ',') {
            v_fl = v_in as f64;
            x = 1;
            self.pop()?;
        }

        while self.is_next(|x| x.is_digit(10)) {
            v_fl += self.pop().unwrap().to_string().parse::<f64>().unwrap() * 10f64.powi(-x);
            x += 1;
        }

        Ok(if x > 0 {
            Token::Float(v_fl)
        } else {
            Token::Integer(v_in)
        })
    }

    fn operator(&mut self, mut operators: Vec<&'a str>) -> Result<Token> {
        let mut content = String::new();

        while self.is_next(|x| {
            !(TERMINATORS.contains(&x)
                || self
                    .delimiters
                    .iter()
                    .filter(|(_, a, b)| x == *b || x == *a)
                    .count()
                    > 0
                || x.is_alphanumeric())
        }) {
            content.push(self.pop()?);

            operators = operators
                .into_iter()
                .filter(|s| s.starts_with(&content))
                .collect();

            if operators.len() > 0 {
                if operators.len() == 1 {
                    break;
                }
            } else {
                return error!(
                    self.file.to_string(),
                    (self.line, self.col),
                    (self._line, self._col),
                    "invalid operator: `{}'",
                    content
                );
            }
        }

        Ok(Token::Operator(content))
    }

    fn identifier(&mut self) -> Result<Token> {
        let mut content = String::new();

        while self.is_next(|x| {
            !(self
                .operators
                .iter()
                .filter_map(|s| s.chars().nth(0))
                .find(|&c| c == x)
                .is_some()
                || self
                    .delimiters
                    .iter()
                    .filter(|(_, a, b)| x == *b || x == *a)
                    .count()
                    > 0
                || TERMINATORS.contains(&x))
        }) {
            content.push(self.pop()?);
        }

        Ok(self
            .keywords
            .iter()
            .find(|&x| x == &content)
            .map_or(Token::Identifier(content), |x| {
                Token::Keyword(x.to_string())
            }))
    }

    fn chr(&mut self) -> Result<Token> {
        self.expect('\'').and(
            self.pop()
                .and_then(|x| self.expect('\'').and(Ok(Token::Char(x)))),
        )
    }

    fn string(&mut self) -> Result<Token> {
        let mut content = String::new();

        self.expect('\"')?;
        while !self.is_next(|x| x == '"') {
            content.push(self.pop()?);
        }

        if self.eof() {
            error!(
                self.file.to_string(),
                (self._line, self._col),
                (self.line, self.col),
                "Missing double quote (`\"') to close the string"
            )
        } else {
            self.pop()?;
            Ok(Token::String(content))
        }
    }

    fn lex_one(&mut self) -> Result<Located<Token>> {
        self.record();

        let tok = if let Some(c) = self.peek() {
            if TERMINATORS.contains(&c) {
                self.pop()?;
                return self.lex_one();
            }

            let opm: Vec<&'a str> = self
                .operators
                .iter()
                .filter(|s| s.len() > 0 && s.chars().nth(0) == Some(c))
                .map(|&x| x)
                .collect();
            let delim = self
                .delimiters
                .iter()
                .filter(|(_, x, y)| *x == c || *y == c)
                .map(|(d, x, _)| (d, *x == c))
                .nth(0);

            match c {
                x if x.is_digit(10) => self.number(),
                _ if delim.is_some() => {
                    self.pop()?;
                    if let Some((d, b)) = delim {
                        Ok(Token::Delimiter(*d, b))
                    } else {
                        unreachable!()
                    }
                }
                x if x == '_' || x.is_alphabetic() => self.identifier(),
                _ if !opm.is_empty() => self.operator(opm),
                '"' => self.string(),
                '\'' => self.chr(),
                _ => errorm!(
                    self.file.to_string(),
                    self.line,
                    self.col,
                    "unexpected character at the beginning of token: `{}'",
                    c
                ),
            }
        } else {
            unreachable!() /* more on that later */
        }?;

        Ok(Located::new(tok)
            .file(self.file.to_string())
            .start(self._line, self._col)
            .end(self.line, self.col))
    }

    pub fn lex(mut self) -> Result<Vec<Located<Token>>> {
        let mut content = Vec::new();

        while !self.eof() {
            content.push(self.lex_one()?)
        }

        Ok(content)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::errors::Result;
    use crate::tokens::{Delimiter, Token};
    use crate::utils::Located;

    const FILE: &str = "CASSIOPEIA-TESTS";

    fn lexer<'a>(input: &'a str) -> Lexer<'a> {
        Lexer::new(input)
            .file(FILE)
            .operators(&["+", "+.", "-"])
            .delimiters(&[(Delimiter::Bracket, '[', ']'), (Delimiter::Paren, '(', ')')])
            .keywords(&["let", "fun"])
    }

    #[test]
    fn keywords() -> Result<()> {
        assert_eq!(
            lexer("let fun").lex()?,
            vec![
                Located::new(Token::Keyword("let".to_string()))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 3),
                Located::new(Token::Keyword("fun".to_string()))
                    .file(FILE.to_string())
                    .start(1, 4)
                    .end(1, 7)
            ]
        );

        Ok(())
    }

    #[test]
    fn operators() -> Result<()> {
        assert_eq!(
            lexer("+ +. -").lex()?,
            vec![
                Located::new(Token::Operator("+".to_string()))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 1),
                Located::new(Token::Operator("+.".to_string()))
                    .file(FILE.to_string())
                    .start(1, 2)
                    .end(1, 4),
                Located::new(Token::Operator("-".to_string()))
                    .file(FILE.to_string())
                    .start(1, 5)
                    .end(1, 6)
            ]
        );

        Ok(())
    }

    #[test]
    fn numbers() -> Result<()> {
        assert_eq!(
            lexer("12 3, 3,1415").lex()?,
            vec![
                Located::new(Token::Integer(12))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 2),
                Located::new(Token::Float(3.0))
                    .file(FILE.to_string())
                    .start(1, 3)
                    .end(1, 5),
                Located::new(Token::Float(3.1415))
                    .file(FILE.to_string())
                    .start(1, 6)
                    .end(1, 12)
            ]
        );

        Ok(())
    }

    #[test]
    fn chars() -> Result<()> {
        assert_eq!(
            lexer("'a' \"Fo\no\"").lex()?,
            vec![
                Located::new(Token::Char('a'))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 3),
                Located::new(Token::String("Fo\no".to_string()))
                    .file(FILE.to_string())
                    .start(1, 4)
                    .end(2, 2)
            ]
        );

        Ok(())
    }

    #[test]
    fn identifiers() -> Result<()> {
        assert_eq!(
            lexer("func bar").lex()?,
            vec![
                Located::new(Token::Identifier("func".to_string()))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 4),
                Located::new(Token::Identifier("bar".to_string()))
                    .file(FILE.to_string())
                    .start(1, 5)
                    .end(1, 8)
            ]
        );

        Ok(())
    }

    #[test]
    fn delimiters() -> Result<()> {
        assert_eq!(
            lexer("() []").lex()?,
            vec![
                Located::new(Token::Delimiter(Delimiter::Paren, true))
                    .file(FILE.to_string())
                    .start(1, 0)
                    .end(1, 1),
                Located::new(Token::Delimiter(Delimiter::Paren, false))
                    .file(FILE.to_string())
                    .start(1, 1)
                    .end(1, 2),
                Located::new(Token::Delimiter(Delimiter::Bracket, true))
                    .file(FILE.to_string())
                    .start(1, 3)
                    .end(1, 4),
                Located::new(Token::Delimiter(Delimiter::Bracket, false))
                    .file(FILE.to_string())
                    .start(1, 4)
                    .end(1, 5),
            ]
        );

        Ok(())
    }
}
