use super::*;

impl<'a> NixStringContent<'a> {
    pub fn get_literal(&self) -> Option<&'a str> {
        match self {
            NixStringContent::Literal(l) => Some(l),
            NixStringContent::Composite(_) => None,
            NixStringContent::Interpolated(_) => None,
            NixStringContent::Empty => Some(""),
        }
    }
}

impl<'a> NixString<'a> {
    pub fn from_literal(lit: &'a str, position: SourcePosition) -> Self {
        Self {
            position,
            content: NixStringContent::Literal(lit),
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
        self.content.fmt(f)
    }
}

impl core::fmt::Debug for NixStringContent<'_> {
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
