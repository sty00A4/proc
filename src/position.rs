use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Position(Range<usize>, Range<usize>);
impl Position {
    pub fn new(ln: Range<usize>, col: Range<usize>) -> Self {
        Self(ln, col)
    }
}