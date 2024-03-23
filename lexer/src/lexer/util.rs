pub fn ends_with_unsecaped_backslash(input: &[u8]) -> bool {
    let number_of_trailing_slashes = input.iter().rev().take_while(|e| **e == b'\\').count();

    // if the final number of slashes is uneven, they will not escape each other
    number_of_trailing_slashes % 2 > 0
}

// pub struct LineSplitter<'a> {
//     input: &'a [u8],
// }
// impl<'a> LineSplitter<'a> {
//     pub fn new(input: &'a [u8]) -> Self {
//         Self { input }
//     }
// }

// impl<'a> Iterator for LineSplitter<'a> {
//     type Item = &'a [u8];

//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(line_idx) = memchr::memchr(b'\n', self.input) {
//             let rv = &self.input[..(line_idx + 1)];
//             self.input = &self.input[(line_idx + 1)..];
//             Some(rv)
//         } else {
//             if self.input.is_empty() {
//                 None
//             } else {
//                 let rv = self.input;
//                 self.input = &[];
//                 Some(rv)
//             }
//         }
//     }
// }
