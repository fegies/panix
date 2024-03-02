pub struct Multipeek<S>
where
    S: Iterator,
{
    buf: [Option<S::Item>; 2],
    source: S,
}
impl<T, S> Multipeek<S>
where
    S: Iterator<Item = T>,
{
    pub fn new(source: S) -> Self {
        Self {
            buf: [None, None],
            source,
        }
    }
    pub fn peek(&mut self) -> Option<&T> {
        self.rotate_fill();
        Self::buffer_get(&mut self.buf[0], &mut self.source)
    }
    pub fn peek_2(&mut self) -> Option<&T> {
        self.rotate_fill();
        Self::buffer_get(&mut self.buf[1], &mut self.source)
    }

    fn buffer_get<'b>(buf: &'b mut Option<T>, iter: &mut S) -> Option<&'b T> {
        if let Some(val) = buf {
            Some(val)
        } else {
            let val = iter.next()?;
            Some(buf.insert(val))
        }
    }

    // rotate the buffers such that at least the first one is filled
    fn rotate_fill(&mut self) {
        if self.buf[0].is_none() {
            self.buf[0] = self.buf[1].take();
        }
    }
}

impl<S, T> Iterator for Multipeek<S>
where
    S: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let cached = self.buf.iter_mut().find_map(|e| e.take());
        cached.or_else(|| self.source.next())
    }
}
