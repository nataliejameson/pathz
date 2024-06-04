use std::fmt::Debug;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use ref_cast::RefCast;

use crate::common::HasParent;
use crate::errors::JoinedAbsolute;
use crate::errors::NotRelative;
use crate::errors::*;
use crate::AbsolutePath;
use crate::AbsolutePathBuf;
use crate::RelativePath;

/// A relative path that does not traverse upward, and does not have '.' as a component.
#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, RefCast)]
#[cfg_attr(feature = "diesel", derive(diesel::expression::AsExpression))]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text, not_sized))]
#[repr(transparent)]
pub struct ForwardPath(Path);

impl ForwardPath {
    /// Attempt to create an instance of [`ForwardPath`].
    ///
    /// This will fail if the provided path is absolute, or if any component is '..'
    pub fn try_new<P: AsRef<Path> + ?Sized>(path: &P) -> Result<&Self, ForwardPathNewError> {
        let p = path.as_ref();
        if p.is_absolute() {
            Err(NotRelative(p.display().to_string()).into())
        } else {
            let mut components_count = 0;
            // This normalizes in a way I don't want it to, so just do some counting of slashes.
            // That *should* handle '//' and '/./', and './foo' will be handled by the component
            // check
            // I would make this more robust, but I don't have a win machine handy to test on...
            for c in p.components() {
                if c.as_os_str() == "." || c.as_os_str() == ".." {
                    return Err(WasNotNormalized(p.display().to_string()).into());
                }
                components_count += 1;
            }
            let slashes = p
                .as_os_str()
                .as_encoded_bytes()
                .iter()
                .filter(|x| **x == std::path::MAIN_SEPARATOR as u8)
                .count();
            if components_count != 0 && (slashes + 1) != components_count {
                return Err(WasNotNormalized(p.display().to_string()).into());
            }
            Ok(Self::ref_cast(path.as_ref()))
        }
    }

    /// Create an [`ForwardPath`] per [`ForwardPath::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: AsRef<Path> + ?Sized>(path: &P) -> &Self {
        Self::try_new(path).expect("a normalized relative path")
    }

    /// Much like new_unchecked, but we don't actually even do any verification.
    ///
    /// This is *only* for internal crate use where we have already verified the contents
    /// of the internal [`Path`] object.
    fn internal_new_unchecked<P: AsRef<Path> + ?Sized>(path: &P) -> &Self {
        Self::ref_cast(path.as_ref())
    }

    /// Return a zero-cost [`RelativePath`]
    pub fn as_relative(&self) -> &RelativePath {
        RelativePath::internal_new_unchecked(&self.0)
    }

    /// Get a reference to the internal Path object.
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative, and must be normalizable without any '..' components.
    pub fn join<P: AsRef<Path>>(&self, path: P) -> Result<ForwardPathBuf, ForwardJoinError> {
        ForwardPathBuf::internal_join(&self.0, path.as_ref())
    }

    /// Join this to an [`AbsolutePath`], normalizing the joined path.
    ///
    /// This can only fail the normalization causes traversal beyond the filesystem root.
    pub fn try_into_absolute(
        &self,
        abs: &AbsolutePath,
    ) -> Result<AbsolutePathBuf, NormalizationFailed> {
        abs.join_relative(self.as_relative())
    }

    /// Like `Path::to_string_lossy()`, but returns an owned string.
    pub fn to_lossy_string(&self) -> String {
        self.0.to_string_lossy().to_string()
    }
}

impl HasParent for ForwardPath {
    type PathType = ForwardPath;

    fn parent(&self) -> Option<&Self::PathType> {
        self.0.parent().map(ForwardPath::internal_new_unchecked)
    }
}

impl AsRef<Path> for ForwardPath {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl AsRef<ForwardPath> for ForwardPath {
    fn as_ref(&self) -> &ForwardPath {
        self
    }
}

impl Deref for ForwardPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for ForwardPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.display(), f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for ForwardPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for ForwardPath
where
    DB: diesel::backend::Backend,
    str: diesel::serialize::ToSql<diesel::sql_types::Text, DB>,
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, DB>,
    ) -> diesel::serialize::Result {
        self.0.to_str().expect("paths should be utf8").to_sql(out)
    }
}

/// The "owned" analog for [`ForwardPath`]. This is normalized on creation, and may not start with '..'
#[derive(Debug, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::expression::AsExpression, diesel::FromSqlRow)
)]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text))]
pub struct ForwardPathBuf(PathBuf);

impl ForwardPathBuf {
    /// Attempt to create an instance of [`ForwardPathBuf`].
    ///
    /// This will fail if the provided path is absolute or if ".." is the first component of
    /// the path after normalizaton.
    pub fn try_new<P: Into<PathBuf> + ?Sized>(path: P) -> Result<Self, ForwardPathBufNewError> {
        let p = path.into();
        if p.is_absolute() {
            Err(NotRelative(p.display().to_string()).into())
        } else {
            let needs_normalization = p
                .components()
                .any(|c| c.as_os_str() == "." || c.as_os_str() == "..");
            if !needs_normalization {
                Ok(Self(p))
            } else {
                let mut new_pb = Vec::with_capacity(p.components().count());
                for c in p.components() {
                    match c.as_os_str() {
                        x if x == "." => {}
                        x if x == ".." => match new_pb.pop() {
                            Some(y) if y == x => {
                                new_pb.push(y);
                                new_pb.push(x);
                            }
                            Some(_) => {}
                            None => {
                                new_pb.push(x);
                            }
                        },
                        x => {
                            new_pb.push(x);
                        }
                    }
                }

                match new_pb.first() {
                    Some(x) if *x == ".." => {
                        Err(NormalizationFailed(p.display().to_string()).into())
                    }
                    _ => Ok(Self(PathBuf::from_iter(new_pb))),
                }
            }
        }
    }

    /// Create an [`ForwardPathBuf`] per [`ForwardPathBuf::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
        Self::try_new(path).expect("a relative path")
    }

    /// Much like new_unchecked, but we don't actually even do any verification.
    ///
    /// This is *only* for internal crate use where we have already verified the contents
    /// of the internal [`PathBuf`] object.
    fn internal_new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
        Self(path.into())
    }

    /// Get a reference to the internal Path object.
    pub fn as_path(&self) -> &Path {
        self.0.as_path()
    }

    /// Get a new [`RelativePath`] referencing the internal Path object.
    pub fn as_relative_path(&self) -> &RelativePath {
        RelativePath::internal_new_unchecked(self.0.as_path())
    }

    /// Get a new [`ForwardPath`] referencing the internal Path object.
    pub fn as_forward_path(&self) -> &ForwardPath {
        ForwardPath::internal_new_unchecked(self.0.as_path())
    }

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative.
    pub fn join<P: AsRef<Path> + ?Sized>(&self, path: &P) -> Result<Self, ForwardJoinError> {
        Self::internal_join(&self.0, path.as_ref())
    }

    fn internal_join(self_path: &Path, path: &Path) -> Result<Self, ForwardJoinError> {
        match ForwardPathBuf::try_new(path) {
            Ok(pb) => Ok(Self::internal_new_unchecked(self_path.join(pb.0))),
            Err(ForwardPathBufNewError::NormalizationFailed(e)) => Err(e.into()),
            Err(ForwardPathBufNewError::NotRelative(_)) => Err(JoinedAbsolute(
                self_path.display().to_string(),
                path.display().to_string(),
            )
            .into()),
        }
    }

    /// Join this to an [`AbsolutePath`], normalizing the joined path.
    ///
    /// This can only fail the normalization causes traversal beyond the filesystem root.
    pub fn try_into_absolute(
        &self,
        abs: &AbsolutePath,
    ) -> Result<AbsolutePathBuf, NormalizationFailed> {
        abs.join_relative(self.as_relative_path())
    }

    /// Like `Path::to_string_lossy()`, but returns an owned string.
    pub fn to_lossy_string(&self) -> String {
        self.0.to_string_lossy().to_string()
    }
}

impl From<&ForwardPath> for ForwardPathBuf {
    fn from(rp: &ForwardPath) -> Self {
        ForwardPathBuf::internal_new_unchecked(&rp.0)
    }
}

impl TryFrom<PathBuf> for ForwardPathBuf {
    type Error = ForwardPathBufNewError;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        ForwardPathBuf::try_new(value)
    }
}

impl FromStr for ForwardPathBuf {
    type Err = ForwardPathBufNewError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ForwardPathBuf::try_new(s)
    }
}

impl AsRef<ForwardPath> for ForwardPathBuf {
    fn as_ref(&self) -> &ForwardPath {
        ForwardPath::internal_new_unchecked(&self.0)
    }
}

impl AsRef<Path> for ForwardPathBuf {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl Deref for ForwardPathBuf {
    type Target = ForwardPath;

    fn deref(&self) -> &Self::Target {
        ForwardPath::internal_new_unchecked(&self.0)
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for ForwardPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.display(), f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for ForwardPathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for ForwardPathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let path = PathBuf::deserialize(deserializer)?;
        ForwardPathBuf::try_new(path).map_err(|e| Error::custom(format!("{}", e)))
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for ForwardPathBuf
where
    DB: diesel::backend::Backend,
    str: diesel::serialize::ToSql<diesel::sql_types::Text, DB>,
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, DB>,
    ) -> diesel::serialize::Result {
        self.0.to_str().expect("paths should be utf8").to_sql(out)
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::deserialize::FromSql<diesel::sql_types::Text, DB> for ForwardPathBuf
where
    DB: diesel::backend::Backend,
    String: diesel::deserialize::FromSql<diesel::sql_types::Text, DB>,
{
    fn from_sql(
        bytes: <DB as diesel::backend::Backend>::RawValue<'_>,
    ) -> diesel::deserialize::Result<Self> {
        String::from_sql(bytes).and_then(|s| Ok(ForwardPathBuf::try_new(s)?))
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use crate::common::HasParent;
    use crate::errors::*;
    use crate::AbsolutePath;
    use crate::AbsolutePathBuf;
    use crate::ForwardPath;
    use crate::ForwardPathBuf;

    #[test]
    fn path_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        assert_eq!(
            Path::new("foo.txt"),
            ForwardPath::try_new("foo.txt")?.as_path()
        );
        assert_eq!(
            Path::new("foo/bar/baz.txt"),
            ForwardPath::try_new("foo/bar/baz.txt")?.as_path()
        );
        assert_eq!(
            ForwardPathNewError::NotRelative(NotRelative(
                cwd.join("foo.txt").display().to_string()
            )),
            ForwardPath::try_new(cwd.join("foo.txt").as_path()).unwrap_err()
        );
        assert_eq!(
            ForwardPathNewError::WasNotNormalized(WasNotNormalized("./foo/bar.txt".to_owned())),
            ForwardPath::try_new("./foo/bar.txt").unwrap_err()
        );
        assert_eq!(
            ForwardPathNewError::WasNotNormalized(WasNotNormalized("foo/./bar.txt".to_owned())),
            ForwardPath::try_new("foo/./bar.txt").unwrap_err()
        );
        assert_eq!(
            ForwardPathNewError::WasNotNormalized(WasNotNormalized("foo/bar.txt/.".to_owned())),
            ForwardPath::try_new("foo/bar.txt/.").unwrap_err()
        );
        assert_eq!(
            ForwardPathNewError::WasNotNormalized(WasNotNormalized("foo//bar.txt".to_owned())),
            ForwardPath::try_new("foo//bar.txt").unwrap_err()
        );
        assert_eq!(
            ForwardPathNewError::WasNotNormalized(WasNotNormalized("foo/../bar.txt".to_owned())),
            ForwardPath::try_new("foo/../bar.txt").unwrap_err()
        );
        Ok(())
    }

    #[test]
    fn path_join() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        assert_eq!(
            Path::new("foo/bar"),
            ForwardPath::try_new("foo")?.join("bar")?.as_path()
        );
        assert_eq!(
            Path::new("foo/baz/quz.txt"),
            ForwardPath::try_new("foo")?
                .join("bar/../baz/quz.txt")?
                .as_path()
        );
        assert_eq!(
            ForwardJoinError::JoinedAbsolute(JoinedAbsolute(
                "foo".to_owned(),
                cwd.join("foo.txt").display().to_string()
            )),
            ForwardPath::try_new("foo")?
                .join(cwd.join("foo.txt"))
                .unwrap_err()
        );
        assert_eq!(
            ForwardJoinError::NormalizationFailed(NormalizationFailed("../bar/baz.txt".to_owned())),
            ForwardPath::try_new("foo")?
                .join("../bar/baz.txt")
                .unwrap_err()
        );

        Ok(())
    }

    #[test]
    fn path_try_into_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let foo_bar = cwd.join("foo/bar");

        let original = AbsolutePath::try_new(foo_bar.as_path())?;

        assert_eq!(
            cwd.join("foo/bar/baz").as_path(),
            ForwardPath::try_new("baz")?
                .try_into_absolute(original)?
                .as_path()
        );

        Ok(())
    }

    #[test]
    fn path_buf_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        assert_eq!(
            Path::new("foo.txt"),
            ForwardPathBuf::try_new("foo.txt")?.as_path()
        );
        assert_eq!(
            Path::new("foo/baz.txt"),
            ForwardPathBuf::try_new("foo/bar/../baz.txt")?.as_path()
        );
        assert_eq!(
            Path::new("foo/bar/baz.txt"),
            ForwardPathBuf::try_new("foo/bar/./baz.txt")?.as_path(),
        );
        assert_eq!(
            Path::new("foo/bar/baz.txt"),
            ForwardPathBuf::try_new("./foo/bar/./baz.txt")?.as_path(),
        );
        assert_eq!(
            ForwardPathBufNewError::NotRelative(NotRelative(
                cwd.join("foo.txt").display().to_string()
            )),
            ForwardPathBuf::try_new(cwd.join("foo.txt")).unwrap_err()
        );
        assert_eq!(
            ForwardPathBufNewError::NormalizationFailed(NormalizationFailed(
                "foo/../../bar/baz.txt".to_owned()
            )),
            ForwardPathBuf::try_new("foo/../../bar/baz.txt").unwrap_err(),
        );

        Ok(())
    }

    #[test]
    fn path_buf_try_into_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        let original = AbsolutePathBuf::try_new(cwd.join("foo/bar"))?;

        assert_eq!(
            cwd.join("foo/bar/baz").as_path(),
            ForwardPathBuf::try_new("baz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );

        Ok(())
    }

    #[test]
    fn path_creates_parent_dirs() -> anyhow::Result<()> {
        let temp = tempfile::tempdir()?;
        let existing = AbsolutePathBuf::try_new(temp.path().canonicalize()?.join("foo/bar"))?;
        let not_existing = AbsolutePathBuf::try_new(temp.path().canonicalize()?.join("bar/baz"))?;
        let cwd = AbsolutePathBuf::current_dir();

        let relative_existing = existing.relative_to(&cwd)?;
        let relative_not_existing = not_existing.relative_to(&cwd)?;

        let existing_file = relative_existing.join("quz")?;
        let not_existing_file = relative_not_existing.join("quz")?;

        std::fs::create_dir_all(&existing)?;

        existing_file.as_relative_path().ensure_parent_exists()?;
        not_existing_file
            .as_relative_path()
            .ensure_parent_exists()?;

        assert!(existing.is_dir());
        assert!(not_existing.is_dir());
        assert!(relative_existing.is_dir());
        assert!(relative_not_existing.is_dir());
        Ok(())
    }

    #[test]
    fn path_buf_creates_parent_dirs() -> anyhow::Result<()> {
        let temp = tempfile::tempdir()?;
        let existing = AbsolutePathBuf::try_new(temp.path().canonicalize()?.join("foo/bar"))?;
        let not_existing = AbsolutePathBuf::try_new(temp.path().canonicalize()?.join("bar/baz"))?;
        let cwd = AbsolutePathBuf::current_dir();

        let relative_existing = existing.relative_to(&cwd)?;
        let relative_not_existing = not_existing.relative_to(&cwd)?;

        let existing_file = relative_existing.join("quz")?;
        let not_existing_file = relative_not_existing.join("quz")?;

        std::fs::create_dir_all(&existing)?;

        existing_file.ensure_parent_exists()?;
        not_existing_file.ensure_parent_exists()?;

        assert!(existing.is_dir());
        assert!(not_existing.is_dir());
        assert!(relative_existing.is_dir());
        assert!(relative_not_existing.is_dir());
        Ok(())
    }
}

#[cfg(all(test, feature = "serde"))]
mod serde_tests {
    use crate::ForwardPath;
    use crate::ForwardPathBuf;

    #[test]
    fn path_serializes() -> anyhow::Result<()> {
        let p = ForwardPath::try_new("foo/bar")?;
        assert_eq!("\"foo/bar\"", serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_serializes() -> anyhow::Result<()> {
        let p = ForwardPathBuf::try_new("foo/./bar")?;
        assert_eq!("\"foo/./bar\"", serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_deserializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let serialized_good = "\"foo/./bar/../baz\"";
        let serialized_absolute = format!("\"{}\"", cwd.display());

        let expected = ForwardPathBuf::try_new("foo/./bar/../baz")?;
        assert_eq!(
            expected,
            serde_json::from_str::<ForwardPathBuf>(serialized_good)?
        );
        assert!(serde_json::from_str::<ForwardPathBuf>(&serialized_absolute).is_err());
        Ok(())
    }
}

#[cfg(all(test, feature = "diesel"))]
mod test_diesel {
    use diesel::RunQueryDsl;

    use crate::diesel::QueryDsl;
    use crate::diesel_helpers::create_table;
    use crate::diesel_helpers::insert_values;
    use crate::ForwardPath;
    use crate::ForwardPathBuf;

    #[derive(Queryable, Insertable, Clone, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFile {
        id: i32,
        x: ForwardPathBuf,
        y: Option<ForwardPathBuf>,
    }

    #[derive(Insertable, Clone, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFileLog {
        id: i32,
        x: &'static ForwardPath,
        y: Option<&'static ForwardPath>,
    }

    #[test]
    fn path_to_sql() -> anyhow::Result<()> {
        let mut connection = create_table()?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = vec![
            TestFile {
                id: 1,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: Some(ForwardPathBuf::try_new("bar/baz.txt")?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFileLog {
                    id: 1,
                    x: ForwardPath::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFileLog {
                    id: 2,
                    x: ForwardPath::try_new("foo/bar.txt")?,
                    y: Some(ForwardPath::try_new("bar/baz.txt")?),
                },
            ])
            .execute(&mut connection)?;

        let rows = test_files.load::<TestFile>(&mut connection)?;
        assert_eq!(expected, rows);

        Ok(())
    }

    #[test]
    fn path_buf_to_sql() -> anyhow::Result<()> {
        let mut connection = create_table()?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = vec![
            TestFile {
                id: 1,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: Some(ForwardPathBuf::try_new("bar/baz.txt")?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFile {
                    id: 1,
                    x: ForwardPathBuf::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFile {
                    id: 2,
                    x: ForwardPathBuf::try_new("foo/bar.txt")?,
                    y: Some(ForwardPathBuf::try_new("bar/baz.txt")?),
                },
            ])
            .execute(&mut connection)?;

        let rows = test_files.load::<TestFile>(&mut connection)?;
        assert_eq!(expected, rows);

        Ok(())
    }

    #[test]
    fn path_buf_from_sql() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let abs_foo_bar = cwd.join("foo/bar.txt");
        let abs_bar_baz = cwd.join("bar/baz.txt");
        let abs_foo_bar_str = abs_foo_bar.display().to_string();
        let abs_bar_baz_str = abs_bar_baz.display().to_string();

        let mut connection = create_table()?;

        insert_values(
            &mut connection,
            &[
                (1, "foo/bar.txt", None),
                (2, &abs_foo_bar_str, None),
                (3, "foo/bar.txt", Some("bar/baz.txt")),
                (4, "foo/bar.txt", Some(&abs_bar_baz_str)),
            ],
        )?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = [
            TestFile {
                id: 1,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 3,
                x: ForwardPathBuf::try_new("foo/bar.txt")?,
                y: Some(ForwardPathBuf::try_new("bar/baz.txt")?),
            },
        ];

        assert_eq!(expected[0], test_files.find(1).first(&mut connection)?);
        assert!(test_files
            .find(2)
            .first::<TestFile>(&mut connection)
            .is_err());
        assert_eq!(expected[1], test_files.find(3).first(&mut connection)?);
        assert!(test_files
            .find(4)
            .first::<TestFile>(&mut connection)
            .is_err());

        Ok(())
    }
}
