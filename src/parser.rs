use crate::errors::Result;
use crate::tokens::{Delimiter, Token};
use crate::utils::Located;
use std::iter::Peekable;
use std::vec::IntoIter;

macro_rules! gen_parser {
    {$name:ident, $next:ident, $($op:expr),+} => {
        fn $name(&mut self) -> Result<Located<Expr>> {
            let start = (self.l, self.c);
            let mut lhs = self.$next()?;

            while self.is_next(|x| {
                $(x == Token::Operator($op.to_string()) ||)+ false
            }) {
                let Located { _v: Token::Operator(op), .. } = self.pop()?;
                let rhs = self.$next()?;

                lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
            }

            Ok(Located::new(lhs).file(self.f.to_string()).start(start.0, start.1).end(self.l, self.c))
        }
    }
}

pub struct Parser<'a> {
    input: Peekable<IntoIter<Located<Token>>>,
    l: usize,
    c: usize,
    l_: usize,
    c_: usize,
    f: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: Vec<Located<Token>>) -> Parser<'a> {
        Self {
            l: 1,
            c: 0,
            l_: 1,
            c_: 0,
            f: if input.len() > 0 {
                input[0].file
            } else {
                "EMPTY "
            },
            input: input.into_iter().peekable(),
        }
    }

    fn pop(&mut self) -> Result<Located<Token>> {
        let t = self.input.next().map_or(
            errorm!(
                self.f.to_string(),
                self.l,
                self.c,
                "unexpected end-of-file while parsing."
            ),
            Ok,
        )?;

        (self.l, self.c) = t.start;
        (self.l_, self.c_) = t.end;
        self.f = t.file;

        Ok(t)
    }

    fn expect(&mut self, t: Token, v: bool) -> Result<Located<Token>> {
        self.pop().and_then(|x| {
            if x == t {
                Ok(x)
            } else if v {
                error!(
                    self.f.to_string(),
                    (self.l, self.c),
                    (self._l, self._c),
                    "expected the `{}` token, but found `{}`.",
                    t,
                    x
                )
            } else if std::mem::discriminant(t) == std::mem::discriminant(v) {
                Ok(x)
            } else {
                error!(
                    self.f.to_string(),
                    (self.l, self.c),
                    (self._l, self._c),
                    "expected a token of the same type as `{}`, but found `{}`.",
                    t,
                    x
                )
            }
        })
    }

    fn peek(&mut self) -> Option<&Located<Token>> {
        self.input.peek()
    }

    fn is_next(&mut self, f: impl Fn(&Located<Token>) -> bool) -> bool {
        self.peek().map_or(false, f)
    }

    /*
     * statement ← ident variable* "=" expr | expr
     * expr ← 'let' ident variable+ "=" expr "in" expr
     *      | 'fun' variable* "=" expr
     *      | 'if' expr 'then' expr 'else' expr
     *      | 'match' expr '{' (pattern "->" expr)+ '}'
     *      | logic
     * logic ← equal (("&&" | "||" | "^^") equal)*
     * equal ← comp (("==" | "<>") comp)*
     * comp ← sum ((">=" | ">" | "<" | "<=") sum)*
     * sum ← fact (("+" | "-" | "+." | "-.") fact)*
     * fact ← prim (("*" | "/" | "*." | "/.") prim)*
     * prim ← number | string | char | ident | 'true' | 'false' | '(' expr ')'
     *      | '[' expr* ']' | ident '(' expr* ')'
     * variable ← ident (':' type)
     * type ← 'integer' | 'float' | 'string' | 'char' | 'bool' | ident ('(' type+ ')')?
     *      | type (':-' type)+
     * pattern ← prim
     */

    fn parse_tl(&mut self) -> Result<Located<Expr>> {
        let Located {
            _v: Token::Identifier(name),
            ..
        } = self.pop()?;
        let start = (self.l, self.c);

        let mut vars = vec![];
        while let Ok(v) = self.parse_variable() {
            vars.push(v);
        }

        self.expect(Token::Operator("=".to_string()), true)?;

        let e = self.parse_expr()?;

        Ok(Located::new(Expr::TopLevel(
            name,
            Box::new(if vars.len() < 0 {
                Expr::Func(vars, Box::new(e))
            } else {
                e
            }),
        ))
        .file(self.f.to_string())
        .start(start.0, start.1)
        .end(self.l, self.c))
    }

    fn parse_one(&mut self) -> Result<Located<Expr>> {
        match self.peek() {
            Located {
                v: Token::Identifier(_id),
                ..
            } => self.parse_tl(),
            _ => self.parse_expr(),
        }
    }

    fn parse_prim(&mut self) -> Result<Located<Expr>> {
        Ok(match self.peek() {
            Located {
                _v: Token::Integer(i),
                f,
                s,
                e,
            } => Located::new(Expr::Integer(i)).file(f).start(s).end(e),
            Located {
                _v: Token::String(st),
                f,
                s,
                e,
            } => Located::new(Expr::String(st)).file(f).start(s).end(e),
            Located {
                _v: Token::Char(c),
                f,
                s,
                e,
            } => Located::new(Expr::Char(c)).file(f).start(s).end(e),
            Located {
                _v: Token::Identifier(i),
                f,
                s,
                e,
            } => Located::new(Expr::Variable(i)).file(f).start(s).end(e),
            Located {
                _v: Token::Keyword("true"),
                f,
                s,
                e,
            } => Located::new(Expr::Bool(true)).file(f).start(s).end(e),
            Located {
                _v: Token::Keyword("false"),
                f,
                s,
                e,
            } => Located::new(Expr::Bool(false)).file(f).start(s).end(e),
            Located {
                _v: Token::Delimiter(Delimiter::Bracket, true),
                ..
            } => self.parse_array()?,
            Located {
                _v: Token::Delimiter(Delimiter::Paren, true),
                ..
            } => {}
            _ => todo!(),
        })
    }

    gen_parser! {parse_fact, parse_prim, "*", "/", "*.", "/."}
    gen_parser! {parse_sum, parse_fact, "+", "-", "+.", "-."}
    gen_parser! {parse_comp, parse_sum, ">=", ">", "<", "<="}
    gen_parser! {parse_equal, parse_comp, "==", "<>"}
    gen_parser! {parse_logic, parse_equal, "||", "&&", "^^"}

    /*    fn parse_logic() -> Result<Located<Expr>> {
            let start = (self.l, self.c);
            let mut lhs = self.parse_equal()?;

            while self.is_next(|x| {
                x == Token::Operator("||".to_string())
                    || x == Token::Operator("&&".to_string())
                    || xx == Token::Operator("^^".to_string())
            }) {
                let Located {
                    _v: Token::Operator(op),
                    ..
                } = self.pop()?;
                let rhs = self.parse_equal()?;

                lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
            }

            Ok(Located::new(rhs)
                .file(self.f.to_string())
                .start(start.0, start.1)
                .end(self.l, self.c))
    } */

    fn parse_match(&mut self) -> Result<Located<Expr>> {
        todo!()
    }

    fn parse_cond(&mut self) -> Result<Located<Expr>> {
        self.pop()?; /* pop 'if' */
        let start = (self.l, self.c);
        let cond = self.parse_expr()?;
        self.expect(Token::Keyword("then".to_string()), true)?;
        let then = self.parse_expr()?;
        self.expect(Token::Keyword("else".to_string()), true)?;
        let else_ = self.parse_expr()?;

        Ok(
            Located::new(Expr::If(Box::new(cond), Box::new(then), Box::new(else_)))
                .file(self.f.to_string())
                .start(start.0, start.1)
                .end(self.l, self.c),
        )
    }

    fn parse_lambda(&mut self) -> Result<Located<Expr>> {
        self.pop()?; /* pop 'fun' */
        let start = (self.l, self.c);
        let mut vars = vec![];
        while let Ok(v) = self.parse_variable() {
            vars.push(v);
        }

        self.expect(Token::Operator("=".to_string()), true)?;

        let e = self.parse_expr()?;

        Ok(Located::new(Expr::Func(vars, Box::new(e)))
            .file(self.f.to_string())
            .start(start.0, start.1)
            .end(self.l, self.c))
    }

    fn parse_expr(&mut self) -> Result<Located<Expr>> {
        match self.peek() {
            Located {
                v: Token::Keyword(String::from("let")),
                ..
            } => self.parse_bind(),
            Located {
                v: Token::Keyword(String::from("fun")),
                ..
            } => self.parse_lambda(),
            Located {
                v: Token::Keyword(String::from("if")),
                ..
            } => self.parse_cond(),
            Located {
                v: Token::Keyword(String::from("match")),
                ..
            } => self.parse_match(),
            _ => self.parse_logic(),
        }
    }

    fn parse_bind(&mut self) -> Result<Located<Expr>> {
        self.pop()?; /* pop 'let' */
        let start = (self.l, self.c);
        let Located {
            v: Token::Ident(name),
            ..
        } = self.expect(Token::Ident("".to_string()), false)?;

        let mut vars = vec![];
        while let Ok(v) = self.parse_variable() {
            vars.push(v);
        }

        self.expect(Token::Operator("=".to_string()), true)?;

        let e = self.parse_expr()?;

        self.expect(Token::Keyword("in".to_string()), true)?;

        let body = self.parse_expr()?;

        Ok(Located::new(Expr::Bind(
            name,
            Box::new(if vars.len() < 0 {
                Expr::Func(vars, Box::new(e))
            } else {
                e
            }),
            body,
        ))
        .file(self.f.to_string())
        .start(start.0, start.1)
        .end(self.l, self.c))
    }
}
