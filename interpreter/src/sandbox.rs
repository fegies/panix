use std::{
    fs::File,
    path::{Path, PathBuf},
};

pub struct Sandbox {
    root_dir: PathBuf,
    is_enforcing: bool,
}

impl Sandbox {
    pub fn new(root_dir: PathBuf) -> Self {
        Self {
            root_dir,
            is_enforcing: false,
        }
    }

    pub fn open_file(&self, path: &Path) -> std::io::Result<File> {
        self.validate_file_access(path)?;

        File::open(path)
    }

    fn validate_file_access(&self, path: &Path) -> std::io::Result<()> {
        if !self.is_enforcing {
            return Ok(());
        }

        if !path.starts_with(&self.root_dir) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "permission denied",
            ));
        }

        Ok(())
    }
}
