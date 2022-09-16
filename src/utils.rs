#[derive(PartialEq, Debug, Clone)]
pub struct Located<T: PartialEq> {
    v: T,
    file: String,
    start: (usize, usize),
    end: (usize, usize),
}

impl<T: PartialEq> Located<T> {
    pub fn new(v: T) -> Located<T> {
        Self {
            v,
            file: String::new(),
            start: (0, 0),
            end: (0, 0),
        }
    }

    pub fn file(mut self, s: String) -> Located<T> {
        self.file = s;
        self
    }

    pub fn start(mut self, l: usize, c: usize) -> Located<T> {
        self.start = (l, c);
        self
    }

    pub fn end(mut self, l: usize, c: usize) -> Located<T> {
        self.end = (l, c);
        self
    }
    pub fn value(self) -> T {
        self.v
    }
}
