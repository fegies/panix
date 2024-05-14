use super::*;

impl<'a> NixString<'a> {
    pub fn get_literal(&self) -> Option<&'a str> {
        match self {
            NixString::Literal(l) => Some(l),
            NixString::Composite(_) => None,
            NixString::Interpolated(_) => None,
            NixString::Empty => Some(""),
        }
    }
}

impl core::fmt::Debug for NixString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => arg0.fmt(f),
            Self::Composite(arg0) => {
                f.write_char('"')?;
                for s in arg0 {
                    f.write_str(s)?;
                }
                f.write_char('"')?;
                Ok(())
            }
            Self::Interpolated(vals) => f
                .debug_struct("Interpolation")
                .field("entries", vals)
                .finish(),
            Self::Empty => "".fmt(f),
        }
    }
}
