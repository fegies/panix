use std::cmp::Ordering;

use super::*;

impl<'a> NixStringContent<'a> {
    pub fn get_literal(&self) -> Option<&'a str> {
        match self {
            NixStringContent::Known(KnownNixStringContent::Empty) => Some(""),
            NixStringContent::Known(KnownNixStringContent::Literal(lit)) => Some(lit),
            _ => None,
        }
    }
}

impl<'a> NixString<'a> {
    pub fn from_literal(lit: &'a str, position: SourcePosition) -> Self {
        Self {
            position,
            content: NixStringContent::Known(KnownNixStringContent::Literal(lit)),
        }
    }
}

impl<'a> AsRef<NixStringContent<'a>> for NixString<'a> {
    fn as_ref(&self) -> &NixStringContent<'a> {
        &self.content
    }
}
impl core::fmt::Debug for NixString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}:{}:{:?}",
            self.position.line, self.position.column, self.content
        ))
    }
}

impl core::fmt::Debug for KnownNixStringContent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => arg0.fmt(f),
            Self::Composite(arg0) => {
                let composed = arg0.iter().cloned().collect::<String>();
                composed.fmt(f)
            }
            Self::Empty => "".fmt(f),
        }
    }
}

impl core::fmt::Debug for NixStringContent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Known(arg0) => arg0.fmt(f),
            Self::Interpolated(vals) => f
                .debug_struct("Interpolation")
                .field("entries", vals)
                .finish(),
        }
    }
}

impl<'a> KnownNixStringContent<'a> {
    fn as_segments(&self) -> &[&'a str] {
        match self {
            KnownNixStringContent::Literal(lit) => core::slice::from_ref(lit),
            KnownNixStringContent::Composite(components) => &components,
            KnownNixStringContent::Empty => &[],
        }
    }
}

impl Eq for KnownNixStringContent<'_> {}

impl PartialEq for KnownNixStringContent<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for KnownNixStringContent<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for KnownNixStringContent<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_slices(self.as_segments(), other.as_segments())
    }
}

fn compare_slices(left: &[&str], right: &[&str]) -> Ordering {
    // fast path form the common case that there are two 1-element sequences
    if let ([left], [right]) = (left, right) {
        return left.cmp(right);
    }

    let mut left = left.into_iter().filter(|s| !s.is_empty());
    let mut right = right.into_iter().filter(|s| !s.is_empty());

    // now walk through the two sequences, always comparing the shorter
    // initial piece to the longer one, and then advancing that seq.
    // in this way we should get the same result as if we had
    // concatenated the strings first

    let (mut cur_left, mut cur_right) = match (left.next(), right.next()) {
        (None, None) => return Ordering::Equal,
        (None, Some(_)) => return Ordering::Less,
        (Some(_), None) => return Ordering::Greater,
        (Some(l), Some(r)) => (*l, *r),
    };

    loop {
        let left_len = cur_left.len();
        let right_len = cur_right.len();
        if left_len < right_len {
            match cur_left.cmp(&&cur_right[..left_len]) {
                Ordering::Equal => {
                    cur_left = if let Some(l) = left.next() {
                        l
                    } else {
                        return Ordering::Less;
                    };
                    cur_right = &cur_right[left_len..];
                }
                o => return o,
            }
        } else if left_len > right_len {
            match cur_left[..right_len].cmp(&cur_right) {
                Ordering::Equal => {
                    cur_right = if let Some(r) = right.next() {
                        r
                    } else {
                        return Ordering::Greater;
                    };
                    cur_left = &cur_left[right_len..];
                }
                o => return o,
            }
        } else {
            // equal length
            match cur_left.cmp(cur_right) {
                Ordering::Equal => {
                    (cur_left, cur_right) = match (left.next(), right.next()) {
                        (None, None) => return Ordering::Equal,
                        (None, Some(_)) => return Ordering::Less,
                        (Some(_), None) => return Ordering::Greater,
                        (Some(l), Some(r)) => (*l, *r),
                    }
                }
                o => return o,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_compare_slices() {
        assert_eq!(Ordering::Equal, compare_slices(&["foo"], &["foo"]));
        assert_eq!(Ordering::Equal, compare_slices(&["foo"], &["fo", "o"]));
        assert_eq!(
            Ordering::Equal,
            compare_slices(&["", "foo"], &["fo", "o", ""])
        );

        assert_eq!(
            Ordering::Less,
            compare_slices(&["fo", "o", "o"], &["foooo"])
        );
        assert_eq!(
            Ordering::Greater,
            compare_slices(&["f", "o", "o", "o"], &["foo"])
        );
    }
}
