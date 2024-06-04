use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use crate::errors::*;
use crate::AbsolutePath;
use crate::AbsolutePathBuf;
use crate::CombinedPathBuf;

/// An absolute path from a [`CombinedPathBuf`], resolved against the cwd.
///
/// This is mostly used in `clap` and `serde` structs so that an extra resolution step
/// is not required. That is why it is only a PathBuf variant.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
pub struct ResolvedAbsolutePathBuf(AbsolutePathBuf);

impl ResolvedAbsolutePathBuf {
    /// Attempt to create an instance of [`ResolvedAbsolutePathBuf`].
    ///
    /// This will fail if the provided path is relative, or if, when normalizing, the path would
    /// traverse beyond the root of the filesystem.
    pub fn try_new<P: Into<PathBuf> + ?Sized>(path: P) -> Result<Self, AbsolutePathBufNewError> {
        let combined = CombinedPathBuf::try_new(path)?;

        Ok(Self(combined.try_into_absolute_in_cwd()?))
    }

    /// Create an [`crate::RelativePath`] per [`crate::RelativePath::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
        Self::try_new(path).expect("an absolute path")
    }

    /// Get a new [`AbsolutePath`] referencing the internal Path object.
    pub fn as_absolute_path(&self) -> &AbsolutePath {
        &self.0
    }
}

impl FromStr for ResolvedAbsolutePathBuf {
    type Err = AbsolutePathBufNewError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ResolvedAbsolutePathBuf::try_new(s)
    }
}

impl AsRef<Path> for ResolvedAbsolutePathBuf {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<AbsolutePath> for ResolvedAbsolutePathBuf {
    fn as_ref(&self) -> &AbsolutePath {
        &self.0
    }
}

impl Deref for ResolvedAbsolutePathBuf {
    type Target = AbsolutePath;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for ResolvedAbsolutePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display().fmt(f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for ResolvedAbsolutePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for ResolvedAbsolutePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let path = PathBuf::deserialize(deserializer)?;
        ResolvedAbsolutePathBuf::try_new(path).map_err(|e| D::Error::custom(format!("{}", e)))
    }
}

#[cfg(test)]
mod test {

    use crate::errors::*;
    use crate::ResolvedAbsolutePathBuf;

    #[test]
    fn path_buf_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        assert_eq!(
            cwd.join("foo.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new("foo.txt")?.as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/quz.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new("foo/bar/baz/../quz.txt")?.as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new("./foo/bar/baz/./quz.txt")?.as_path()
        );

        assert_eq!(
            cwd.join("foo.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new(cwd.join("foo.txt").as_path())?.as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/quz.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new(cwd.join("foo/bar/baz/../quz.txt").as_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz.txt").as_path(),
            ResolvedAbsolutePathBuf::try_new(cwd.join("./foo/bar/baz/./quz.txt").as_path())?
                .as_path()
        );

        let parent_dirs = "../".repeat(cwd.components().count());
        let past_root_path = cwd.join("foo").join(parent_dirs).join("../../bar.txt");
        assert_eq!(
            AbsolutePathBufNewError::NormalizationFailed(NormalizationFailed(
                past_root_path.display().to_string()
            )),
            ResolvedAbsolutePathBuf::try_new(past_root_path.as_path()).unwrap_err()
        );

        Ok(())
    }
}

#[cfg(all(test, feature = "serde"))]
mod serde_tests {

    use crate::ResolvedAbsolutePathBuf;

    #[test]
    fn path_buf_serializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let p = ResolvedAbsolutePathBuf::try_new(&cwd)?;
        assert_eq!(format!("\"{}\"", cwd.display()), serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_deserializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let serialized_absolute = format!("\"{}/foo/./bar/../baz\"", cwd.display());
        let serialized_relative = "\"foo/./bar/../baz\"".to_owned();
        let serialized_traversal = format!(
            "\"{}\"",
            cwd.join("../".repeat(cwd.components().count())).display()
        );

        let expected = ResolvedAbsolutePathBuf::try_new(cwd.join("foo/baz"))?;
        assert_eq!(
            expected,
            serde_json::from_str::<ResolvedAbsolutePathBuf>(&serialized_absolute)?
        );
        assert_eq!(
            expected,
            serde_json::from_str::<ResolvedAbsolutePathBuf>(&serialized_relative)?
        );
        assert!(serde_json::from_str::<ResolvedAbsolutePathBuf>(&serialized_traversal).is_err());
        Ok(())
    }
}
