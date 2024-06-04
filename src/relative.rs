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

/// A relative path. This is not normalized until joined to an absolute path.
#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, RefCast)]
#[cfg_attr(feature = "diesel", derive(diesel::expression::AsExpression))]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text, not_sized))]
#[repr(transparent)]
pub struct RelativePath(Path);

impl RelativePath {
    /// Attempt to create an instance of [`RelativePath`].
    ///
    /// This will fail if the provided path is absolute.
    pub fn try_new<P: AsRef<Path> + ?Sized>(path: &P) -> Result<&Self, NotRelative> {
        let p = path.as_ref();
        if p.is_absolute() {
            Err(NotRelative(p.display().to_string()))
        } else {
            Ok(Self::ref_cast(path.as_ref()))
        }
    }

    /// Create an [`RelativePath`] per [`RelativePath::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: AsRef<Path> + ?Sized>(path: &P) -> &Self {
        Self::try_new(path).expect("an absolute path")
    }

    /// Much like new_unchecked, but we don't actually even do any verification.
    ///
    /// This is *only* for internal crate use where we have already verified the contents
    /// of the internal [`Path`] object.
    pub(crate) fn internal_new_unchecked<P: AsRef<Path> + ?Sized>(path: &P) -> &Self {
        Self::ref_cast(path.as_ref())
    }

    /// Get a reference to the internal Path object.
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative.
    pub fn join<P: AsRef<Path>>(&self, path: P) -> Result<RelativePathBuf, JoinedAbsolute> {
        let p = path.as_ref();
        if p.is_absolute() {
            Err(JoinedAbsolute(
                self.0.display().to_string(),
                p.display().to_string(),
            ))
        } else {
            Ok(RelativePathBuf::try_new(self.0.join(p))
                .expect("Already verified both pieces are relative"))
        }
    }

    /// Join this to an [`AbsolutePath`], normalizing the joined path.
    ///
    /// This can only fail the normalization causes traversal beyond the filesystem root.
    pub fn try_into_absolute(
        &self,
        abs: &AbsolutePath,
    ) -> Result<AbsolutePathBuf, NormalizationFailed> {
        abs.join_relative(self)
    }

    /// Like `Path::to_string_lossy()`, but returns an owned string.
    pub fn to_lossy_string(&self) -> String {
        self.0.to_string_lossy().to_string()
    }
}

impl HasParent for RelativePath {
    type PathType = RelativePath;

    fn parent(&self) -> Option<&Self::PathType> {
        self.0.parent().map(RelativePath::internal_new_unchecked)
    }
}

impl AsRef<Path> for RelativePath {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl AsRef<RelativePath> for RelativePath {
    fn as_ref(&self) -> &RelativePath {
        self
    }
}

impl Deref for RelativePath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for RelativePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.display(), f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for RelativePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for RelativePath
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

/// The "owned" analog for [`RelativePath`]. This is not normalized until joined to an absolute path.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::expression::AsExpression, diesel::FromSqlRow)
)]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text))]
pub struct RelativePathBuf(PathBuf);

impl RelativePathBuf {
    /// Attempt to create an instance of [`RelativePathBuf`].
    ///
    /// This will fail if the provided path is absolute.
    pub fn try_new<P: Into<PathBuf> + ?Sized>(path: P) -> Result<Self, NotRelative> {
        let p = path.into();
        if p.is_absolute() {
            Err(NotRelative(p.display().to_string()))
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

                Ok(Self(PathBuf::from_iter(new_pb)))
            }
        }
    }

    /// Create a [`RelativePathBuf`] per [`RelativePathBuf::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
        Self::try_new(path).expect("a relative path")
    }

    /// Much like new_unchecked, but we don't actually even do any verification.
    ///
    /// This is *only* for internal crate use where we have already verified the contents
    /// of the internal [`Path`] object.
    pub(crate) fn internal_new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
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

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative.
    pub fn join<P: AsRef<Path> + ?Sized>(&self, path: &P) -> Result<Self, JoinedAbsolute> {
        let p = path.as_ref();
        if p.is_absolute() {
            Err(JoinedAbsolute(
                self.0.display().to_string(),
                p.display().to_string(),
            ))
        } else {
            Ok(Self::try_new(self.0.join(p)).expect("Already verified both pieces were relative"))
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

impl From<&RelativePath> for RelativePathBuf {
    fn from(rp: &RelativePath) -> Self {
        RelativePathBuf::internal_new_unchecked(&rp.0)
    }
}

impl TryFrom<PathBuf> for RelativePathBuf {
    type Error = NotRelative;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        RelativePathBuf::try_new(value)
    }
}

impl FromStr for RelativePathBuf {
    type Err = NotRelative;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        RelativePathBuf::try_new(s)
    }
}

impl AsRef<RelativePath> for RelativePathBuf {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::internal_new_unchecked(&self.0)
    }
}

impl AsRef<Path> for RelativePathBuf {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl Deref for RelativePathBuf {
    type Target = RelativePath;

    fn deref(&self) -> &Self::Target {
        RelativePath::internal_new_unchecked(&self.0)
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for RelativePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.display(), f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for RelativePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for RelativePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let path = PathBuf::deserialize(deserializer)?;
        RelativePathBuf::try_new(path).map_err(|e| D::Error::custom(format!("{}", e)))
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for RelativePathBuf
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
impl<DB> diesel::deserialize::FromSql<diesel::sql_types::Text, DB> for RelativePathBuf
where
    DB: diesel::backend::Backend,
    String: diesel::deserialize::FromSql<diesel::sql_types::Text, DB>,
{
    fn from_sql(
        bytes: <DB as diesel::backend::Backend>::RawValue<'_>,
    ) -> diesel::deserialize::Result<Self> {
        String::from_sql(bytes).and_then(|s| Ok(RelativePathBuf::try_new(s)?))
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use crate::common::HasParent;
    use crate::errors::*;
    use crate::AbsolutePath;
    use crate::AbsolutePathBuf;
    use crate::RelativePath;
    use crate::RelativePathBuf;

    #[test]
    fn path_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        assert_eq!(
            Path::new("foo.txt"),
            RelativePath::try_new("foo.txt")?.as_path()
        );
        assert_eq!(
            Path::new("foo/../bar/../../baz/./quz.txt"),
            RelativePath::try_new("foo/../bar/../../baz/./quz.txt")?.as_path()
        );

        assert_eq!(
            NotRelative(cwd.join("foo.txt").display().to_string()),
            RelativePath::try_new(cwd.join("foo.txt").as_path()).unwrap_err()
        );
        Ok(())
    }

    #[test]
    fn path_join() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        assert_eq!(
            Path::new("foo/bar"),
            RelativePath::try_new("foo")?.join("bar")?.as_path()
        );
        assert_eq!(
            Path::new("../baz/quz.txt"),
            RelativePath::try_new("foo")?
                .join("../bar/../../baz/./quz.txt")?
                .as_path()
        );

        assert_eq!(
            JoinedAbsolute("foo".to_owned(), cwd.join("foo.txt").display().to_string()),
            RelativePath::try_new("foo")?
                .join(cwd.join("foo.txt"))
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
            RelativePath::try_new("baz")?
                .try_into_absolute(original)?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo").as_path(),
            RelativePath::try_new("../")?
                .try_into_absolute(original)?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz").as_path(),
            RelativePath::try_new("baz/./quz")?
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
            RelativePathBuf::try_new("foo.txt")?.as_path()
        );
        assert_eq!(
            Path::new("../baz/quz.txt"),
            RelativePathBuf::try_new("foo/../bar/../../baz/./quz.txt")?.as_path()
        );
        assert_eq!(
            Path::new("../../bar"),
            RelativePathBuf::try_new("../../foo/../bar")?.as_path()
        );

        assert_eq!(
            NotRelative(cwd.join("foo.txt").display().to_string()),
            RelativePathBuf::try_new(cwd.join("foo.txt")).unwrap_err()
        );

        Ok(())
    }

    #[test]
    fn path_buf_try_into_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        let original = AbsolutePathBuf::try_new(cwd.join("foo/bar"))?;

        assert_eq!(
            cwd.join("foo/bar/baz").as_path(),
            RelativePathBuf::try_new("baz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo").as_path(),
            RelativePathBuf::try_new("../")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz").as_path(),
            RelativePathBuf::try_new("baz/./quz")?
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
    use crate::RelativePath;
    use crate::RelativePathBuf;

    #[test]
    fn path_serializes() -> anyhow::Result<()> {
        let p = RelativePath::try_new("foo/./bar")?;
        assert_eq!("\"foo/./bar\"", serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_serializes() -> anyhow::Result<()> {
        let p = RelativePathBuf::try_new("foo/./bar")?;
        assert_eq!("\"foo/./bar\"", serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_deserializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let serialized_good = "\"foo/./bar/../baz\"";
        let serialized_absolute = format!("\"{}\"", cwd.display());

        let expected = RelativePathBuf::try_new("foo/./bar/../baz")?;
        assert_eq!(
            expected,
            serde_json::from_str::<RelativePathBuf>(serialized_good)?
        );
        assert!(serde_json::from_str::<RelativePathBuf>(&serialized_absolute).is_err());
        Ok(())
    }
}

#[cfg(all(test, feature = "diesel"))]
mod test_diesel {
    use diesel::RunQueryDsl;

    use crate::diesel::QueryDsl;
    use crate::diesel_helpers::create_table;
    use crate::diesel_helpers::insert_values;
    use crate::RelativePath;
    use crate::RelativePathBuf;

    #[derive(Queryable, Insertable, Clone, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFile {
        id: i32,
        x: RelativePathBuf,
        y: Option<RelativePathBuf>,
    }

    #[derive(Insertable, Clone, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFileLog {
        id: i32,
        x: &'static RelativePath,
        y: Option<&'static RelativePath>,
    }

    #[test]
    fn path_to_sql() -> anyhow::Result<()> {
        let mut connection = create_table()?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = vec![
            TestFile {
                id: 1,
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: Some(RelativePathBuf::try_new("bar/baz.txt")?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFileLog {
                    id: 1,
                    x: RelativePath::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFileLog {
                    id: 2,
                    x: RelativePath::try_new("foo/bar.txt")?,
                    y: Some(RelativePath::try_new("bar/baz.txt")?),
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
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: Some(RelativePathBuf::try_new("bar/baz.txt")?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFile {
                    id: 1,
                    x: RelativePathBuf::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFile {
                    id: 2,
                    x: RelativePathBuf::try_new("foo/bar.txt")?,
                    y: Some(RelativePathBuf::try_new("bar/baz.txt")?),
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
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 3,
                x: RelativePathBuf::try_new("foo/bar.txt")?,
                y: Some(RelativePathBuf::try_new("bar/baz.txt")?),
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
