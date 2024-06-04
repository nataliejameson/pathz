use std::fmt::Debug;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use ref_cast::RefCast;

use crate::common::HasParent;
use crate::errors::*;
use crate::AbsolutePath;
use crate::AbsolutePathBuf;
use crate::RelativePath;
use crate::RelativePathBuf;

/// A path that is either Absolute or Relative, but strongly typed either way.
#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, RefCast)]
#[cfg_attr(feature = "diesel", derive(diesel::expression::AsExpression))]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text, not_sized))]
#[repr(transparent)]
pub struct CombinedPath(Path);

impl CombinedPath {
    pub fn try_new<P: AsRef<Path> + ?Sized>(path: &P) -> Result<&Self, WasNotNormalized> {
        let p = path.as_ref();
        if p.is_absolute() {
            Ok(Self::ref_cast(AbsolutePath::try_new(path).map_err(
                |e| match e {
                    AbsolutePathNewError::WasNotNormalized(e) => e,
                    AbsolutePathNewError::NotAbsolute(_) => {
                        std::unreachable!()
                    }
                },
            )?))
        } else {
            Ok(Self::ref_cast(
                RelativePath::try_new(path).expect("already verified was relative"),
            ))
        }
    }

    /// Create an [`CombinedPath`] per [`CombinedPath::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: AsRef<Path> + ?Sized>(path: &P) -> &Self {
        Self::try_new(path).expect("an absolute path")
    }

    /// Get a reference to the internal Path object.
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative.
    pub fn join<P: AsRef<Path>>(&self, path: P) -> Result<CombinedPathBuf, CombinedJoinError> {
        if self.0.is_absolute() {
            Ok(AbsolutePath::new_unchecked(&self.0).join(path)?.into())
        } else {
            Ok(RelativePath::new_unchecked(&self.0).join(path)?.into())
        }
    }

    /// Resolve this into an [`AbsolutePathBuf`] by either converting the AbsolutePath, or joining
    /// the RelativePath to `resolve_against`
    pub fn try_into_absolute(
        &self,
        resolve_against: &AbsolutePath,
    ) -> Result<AbsolutePathBuf, NormalizationFailed> {
        if self.0.is_absolute() {
            Ok(AbsolutePath::new_unchecked(&self.0).into())
        } else {
            Ok(RelativePath::new_unchecked(&self.0).try_into_absolute(resolve_against)?)
        }
    }

    /// Helper to resolve this path against the cwd.
    pub fn try_into_absolute_in_cwd(&self) -> Result<AbsolutePathBuf, NormalizationFailed> {
        self.try_into_absolute(&AbsolutePathBuf::current_dir())
    }

    pub fn is_relative(&self) -> bool {
        self.0.is_relative()
    }

    pub fn is_absolute(&self) -> bool {
        self.0.is_absolute()
    }

    pub fn to_lossy_string(&self) -> String {
        self.0.to_string_lossy().to_string()
    }
}

impl HasParent for CombinedPath {
    type PathType = CombinedPath;

    fn parent(&self) -> Option<&Self::PathType> {
        self.0.parent().map(CombinedPath::new_unchecked)
    }
}

impl AsRef<Path> for CombinedPath {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl AsRef<CombinedPath> for CombinedPath {
    fn as_ref(&self) -> &CombinedPath {
        self
    }
}

impl Deref for CombinedPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for CombinedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_relative() {
            std::fmt::Display::fmt(&RelativePath::try_new(&self.0).unwrap().display(), f)
        } else {
            std::fmt::Display::fmt(&AbsolutePath::try_new(&self.0).unwrap().display(), f)
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for CombinedPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for CombinedPath
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

/// The owned version of [`CombinedPathBuf`]
///
/// Very useful for things like command line arguments or config file values.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Ord, PartialOrd)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::expression::AsExpression, diesel::FromSqlRow)
)]
#[cfg_attr(feature="diesel", diesel(sql_type = diesel::sql_types::Text))]
pub enum CombinedPathBuf {
    Relative(RelativePathBuf),
    Absolute(AbsolutePathBuf),
}

impl CombinedPathBuf {
    pub fn try_new<P: Into<PathBuf> + ?Sized>(path: P) -> Result<Self, NormalizationFailed> {
        let p = path.into();
        if p.is_absolute() {
            Ok(CombinedPathBuf::Absolute(
                AbsolutePathBuf::try_new(p).map_err(|e| match e {
                    AbsolutePathBufNewError::NormalizationFailed(e) => e,
                    AbsolutePathBufNewError::NotAbsolute(_) => {
                        std::unreachable!()
                    }
                })?,
            ))
        } else {
            Ok(CombinedPathBuf::Relative(
                RelativePathBuf::try_new(p).expect("already verified was relative"),
            ))
        }
    }

    /// Create a [`CombinedPathBuf`] per [`CombinedPathBuf::try_new`] that panics on an invalid path.
    ///
    /// This is mostly used for paths that are known ahead of time (e.g. static strings) to be
    /// valid.
    pub fn new_unchecked<P: Into<PathBuf> + ?Sized>(path: P) -> Self {
        Self::try_new(path).expect("a combined path")
    }

    /// Get a reference to the internal Path object.
    pub fn as_path(&self) -> &Path {
        match self {
            CombinedPathBuf::Relative(r) => r.as_path(),
            CombinedPathBuf::Absolute(a) => a.as_path(),
        }
    }

    /// Attempt to join to a path.
    ///
    /// The provided path must be relative.
    pub fn join<P: AsRef<Path>>(&self, path: P) -> Result<CombinedPathBuf, CombinedJoinError> {
        match self {
            CombinedPathBuf::Relative(r) => Ok(r.join(&path)?.into()),
            CombinedPathBuf::Absolute(a) => Ok(a.join(&path)?.into()),
        }
    }

    /// Resolve this into an [`AbsolutePathBuf`] by either converting the AbsolutePath, or joining
    /// the RelativePath to `resolve_against`
    pub fn try_into_absolute(
        &self,
        resolve_against: &AbsolutePath,
    ) -> Result<AbsolutePathBuf, NormalizationFailed> {
        match self {
            CombinedPathBuf::Relative(r) => r.try_into_absolute(resolve_against),
            CombinedPathBuf::Absolute(a) => Ok(a.clone()),
        }
    }

    /// Helper to resolve this path against the cwd.
    pub fn try_into_absolute_in_cwd(&self) -> Result<AbsolutePathBuf, NormalizationFailed> {
        let cwd = std::env::current_dir().expect("there to be a cwd");
        let abs_cwd = AbsolutePath::new_unchecked(&cwd);
        self.try_into_absolute(abs_cwd)
    }

    pub fn is_relative(&self) -> bool {
        match self {
            CombinedPathBuf::Relative(_) => true,
            CombinedPathBuf::Absolute(_) => false,
        }
    }

    pub fn is_absolute(&self) -> bool {
        match self {
            CombinedPathBuf::Relative(_) => false,
            CombinedPathBuf::Absolute(_) => true,
        }
    }

    /// Like `Path::to_string_lossy()`, but returns an owned string.
    pub fn to_lossy_string(&self) -> String {
        match self {
            CombinedPathBuf::Relative(r) => r.to_lossy_string(),
            CombinedPathBuf::Absolute(a) => a.to_lossy_string(),
        }
    }
}

impl From<&CombinedPath> for CombinedPathBuf {
    fn from(c: &CombinedPath) -> Self {
        if c.0.is_absolute() {
            CombinedPathBuf::Absolute(AbsolutePathBuf::new_unchecked(&c.0))
        } else {
            CombinedPathBuf::Relative(RelativePathBuf::new_unchecked(&c.0))
        }
    }
}

impl From<&RelativePath> for CombinedPathBuf {
    fn from(p: &RelativePath) -> Self {
        CombinedPathBuf::Relative(p.into())
    }
}

impl From<&AbsolutePath> for CombinedPathBuf {
    fn from(p: &AbsolutePath) -> Self {
        CombinedPathBuf::Absolute(p.into())
    }
}

impl From<RelativePathBuf> for CombinedPathBuf {
    fn from(p: RelativePathBuf) -> Self {
        CombinedPathBuf::Relative(p)
    }
}

impl From<AbsolutePathBuf> for CombinedPathBuf {
    fn from(p: AbsolutePathBuf) -> Self {
        CombinedPathBuf::Absolute(p)
    }
}

impl TryFrom<PathBuf> for CombinedPathBuf {
    type Error = NormalizationFailed;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        CombinedPathBuf::try_new(value)
    }
}

impl FromStr for CombinedPathBuf {
    type Err = NormalizationFailed;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CombinedPathBuf::try_new(s)
    }
}

impl AsRef<Path> for CombinedPathBuf {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl Deref for CombinedPathBuf {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

#[cfg(feature = "display")]
impl std::fmt::Display for CombinedPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CombinedPathBuf::Relative(r) => std::fmt::Display::fmt(&r.display(), f),
            CombinedPathBuf::Absolute(a) => std::fmt::Display::fmt(&a.display(), f),
        }
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for CombinedPathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            CombinedPathBuf::Relative(r) => r.serialize(serializer),
            CombinedPathBuf::Absolute(a) => a.serialize(serializer),
        }
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for CombinedPathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let path = PathBuf::deserialize(deserializer)?;
        CombinedPathBuf::try_new(path).map_err(|e| D::Error::custom(format!("{}", e)))
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::serialize::ToSql<diesel::sql_types::Text, DB> for CombinedPathBuf
where
    DB: diesel::backend::Backend,
    str: diesel::serialize::ToSql<diesel::sql_types::Text, DB>,
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, DB>,
    ) -> diesel::serialize::Result {
        match self {
            CombinedPathBuf::Relative(r) => {
                diesel::serialize::ToSql::<diesel::sql_types::Text, DB>::to_sql(r, out)
            }
            CombinedPathBuf::Absolute(a) => {
                diesel::serialize::ToSql::<diesel::sql_types::Text, DB>::to_sql(a, out)
            }
        }
    }
}

#[cfg(feature = "diesel")]
impl<DB> diesel::deserialize::FromSql<diesel::sql_types::Text, DB> for CombinedPathBuf
where
    DB: diesel::backend::Backend,
    String: diesel::deserialize::FromSql<diesel::sql_types::Text, DB>,
{
    fn from_sql(
        bytes: <DB as diesel::backend::Backend>::RawValue<'_>,
    ) -> diesel::deserialize::Result<Self> {
        String::from_sql(bytes).and_then(|s| Ok(CombinedPathBuf::try_new(s)?))
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;
    use std::path::PathBuf;

    use crate::combined::CombinedPath;
    use crate::combined::CombinedPathBuf;
    use crate::errors::*;
    use crate::AbsolutePathBuf;

    #[test]
    fn path_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        let relative = CombinedPath::try_new("foo.txt")?;
        let absolute = CombinedPath::try_new(&cwd)?;

        assert!(relative.is_relative());
        assert_eq!(Path::new("foo.txt"), relative.as_path());

        assert!(absolute.is_absolute());
        assert_eq!(cwd.as_path(), absolute.as_path());

        assert_eq!(
            WasNotNormalized(cwd.join("foo/../../bar.txt").display().to_string()),
            CombinedPath::try_new(cwd.join("foo/../../bar.txt").as_path()).unwrap_err()
        );
        Ok(())
    }

    #[test]
    fn is_relative_is_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        assert!(CombinedPath::try_new(&cwd)?.is_absolute());
        assert!(!CombinedPath::try_new("foo/bar")?.is_absolute());
        assert!(CombinedPathBuf::try_new(&cwd)?.is_absolute());
        assert!(!CombinedPathBuf::try_new("foo/bar")?.is_absolute());

        assert!(!CombinedPath::try_new(&cwd)?.is_relative());
        assert!(CombinedPath::try_new("foo/bar")?.is_relative());
        assert!(!CombinedPathBuf::try_new(&cwd)?.is_relative());
        assert!(CombinedPathBuf::try_new("foo/bar")?.is_relative());

        Ok(())
    }

    #[test]
    fn path_try_into_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let foo_bar = cwd.join("foo/bar");

        let original = AbsolutePathBuf::try_new(foo_bar.as_path())?;

        assert_eq!(
            cwd.join("foo/bar/baz").as_path(),
            CombinedPath::try_new("baz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo").as_path(),
            CombinedPath::try_new("../")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz").as_path(),
            CombinedPath::try_new("baz/./quz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.as_path(),
            CombinedPath::try_new(cwd.as_path())?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );

        let traversal = PathBuf::from("../".repeat(cwd.components().count() + 5));
        assert_eq!(
            NormalizationFailed(original.as_path().join(&traversal).display().to_string()),
            CombinedPath::try_new(&traversal)?
                .try_into_absolute(original.as_absolute_path())
                .unwrap_err()
        );

        Ok(())
    }

    #[test]
    fn path_buf_try_new() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;

        let relative = CombinedPathBuf::try_new("foo.txt")?;
        let absolute = CombinedPathBuf::try_new(&cwd)?;

        assert!(relative.is_relative());
        assert_eq!(Path::new("foo.txt"), relative.as_path());

        assert!(absolute.is_absolute());
        assert_eq!(cwd.as_path(), absolute.as_path());

        let traversal = cwd.join("../".repeat(cwd.components().count() + 5));
        assert_eq!(
            NormalizationFailed(traversal.display().to_string()),
            CombinedPathBuf::try_new(&traversal).unwrap_err()
        );
        Ok(())
    }

    #[test]
    fn path_buf_try_into_absolute() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let foo_bar = cwd.join("foo/bar");

        let original = AbsolutePathBuf::try_new(foo_bar.as_path())?;

        assert_eq!(
            cwd.join("foo/bar/baz").as_path(),
            CombinedPathBuf::try_new("baz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo").as_path(),
            CombinedPathBuf::try_new("../")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.join("foo/bar/baz/quz").as_path(),
            CombinedPathBuf::try_new("baz/./quz")?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );
        assert_eq!(
            cwd.as_path(),
            CombinedPathBuf::try_new(cwd.as_path())?
                .try_into_absolute(original.as_absolute_path())?
                .as_path()
        );

        Ok(())
    }
}

#[cfg(all(test, feature = "serde"))]
mod test_serde {
    use crate::combined::CombinedPath;
    use crate::combined::CombinedPathBuf;

    #[test]
    fn path_serializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let p = CombinedPath::try_new("foo/./bar")?;
        assert_eq!("\"foo/./bar\"", serde_json::to_string(&p)?);

        let p = CombinedPath::try_new(&cwd)?;
        assert_eq!(format!("\"{}\"", cwd.display()), serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_serializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let p = CombinedPathBuf::try_new("foo/./bar")?;
        assert_eq!("\"foo/./bar\"", serde_json::to_string(&p)?);

        let p = CombinedPathBuf::try_new(&cwd)?;
        assert_eq!(format!("\"{}\"", cwd.display()), serde_json::to_string(&p)?);
        Ok(())
    }

    #[test]
    fn path_buf_deserializes() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let serialized_relative = "\"foo/./bar/../baz\"";
        let serialized_absolute = format!("\"{}/foo/../bar/./quz\"", cwd.display());
        let serialized_bad = format!(
            "\"{}/{}\"",
            cwd.display(),
            "../".repeat(cwd.components().count() + 1)
        );

        let expected_relative = CombinedPathBuf::try_new("foo/./bar/../baz")?;
        let expected_absolute = CombinedPathBuf::try_new(cwd.join("bar/quz"))?;
        assert_eq!(
            expected_relative,
            serde_json::from_str::<CombinedPathBuf>(serialized_relative)?
        );
        assert_eq!(
            expected_absolute,
            serde_json::from_str::<CombinedPathBuf>(&serialized_absolute)?
        );
        assert!(serde_json::from_str::<CombinedPathBuf>(&serialized_bad).is_err());
        Ok(())
    }
}

#[cfg(all(test, feature = "diesel"))]
mod test_diesel {
    use diesel::RunQueryDsl;

    use crate::diesel::QueryDsl;
    use crate::diesel_helpers::create_table;
    use crate::diesel_helpers::insert_values;
    use crate::CombinedPath;
    use crate::CombinedPathBuf;

    #[derive(Queryable, Insertable, Clone, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFile {
        id: i32,
        x: CombinedPathBuf,
        y: Option<CombinedPathBuf>,
    }

    #[derive(Insertable, Debug, Eq, PartialEq)]
    #[diesel(table_name = crate::diesel_helpers::schema::test_files)]
    struct TestFileLog<'a> {
        id: i32,
        x: &'a CombinedPath,
        y: Option<&'a CombinedPath>,
    }

    #[test]
    fn path_to_sql() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let abs_foo_bar = cwd.join("foo/bar.txt");
        let abs_bar_baz = cwd.join("bar/baz.txt");

        let mut connection = create_table()?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = vec![
            TestFile {
                id: 1,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: Some(CombinedPathBuf::try_new("bar/baz.txt")?),
            },
            TestFile {
                id: 3,
                x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                y: None,
            },
            TestFile {
                id: 4,
                x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                y: Some(CombinedPathBuf::try_new(&abs_bar_baz)?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFileLog {
                    id: 1,
                    x: CombinedPath::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFileLog {
                    id: 2,
                    x: CombinedPath::try_new("foo/bar.txt")?,
                    y: Some(CombinedPath::try_new("bar/baz.txt")?),
                },
                &TestFileLog {
                    id: 3,
                    x: CombinedPath::try_new(&abs_foo_bar)?,
                    y: None,
                },
                &TestFileLog {
                    id: 4,
                    x: CombinedPath::try_new(&abs_foo_bar)?,
                    y: Some(CombinedPath::try_new(&abs_bar_baz)?),
                },
            ])
            .execute(&mut connection)?;

        let rows = test_files.load::<TestFile>(&mut connection)?;
        assert_eq!(expected, rows);

        Ok(())
    }

    #[test]
    fn path_buf_to_sql() -> anyhow::Result<()> {
        let cwd = std::env::current_dir()?;
        let abs_foo_bar = cwd.join("foo/bar.txt");
        let abs_bar_baz = cwd.join("bar/baz.txt");

        let mut connection = create_table()?;

        use crate::diesel_helpers::schema::test_files::dsl::*;

        let expected = vec![
            TestFile {
                id: 1,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: Some(CombinedPathBuf::try_new("bar/baz.txt")?),
            },
            TestFile {
                id: 3,
                x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                y: None,
            },
            TestFile {
                id: 4,
                x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                y: Some(CombinedPathBuf::try_new(&abs_bar_baz)?),
            },
        ];

        diesel::insert_into(test_files)
            .values(vec![
                &TestFile {
                    id: 1,
                    x: CombinedPathBuf::try_new("foo/bar.txt")?,
                    y: None,
                },
                &TestFile {
                    id: 2,
                    x: CombinedPathBuf::try_new("foo/bar.txt")?,
                    y: Some(CombinedPathBuf::try_new("bar/baz.txt")?),
                },
                &TestFile {
                    id: 3,
                    x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                    y: None,
                },
                &TestFile {
                    id: 4,
                    x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                    y: Some(CombinedPathBuf::try_new(&abs_bar_baz)?),
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
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: None,
            },
            TestFile {
                id: 2,
                x: CombinedPathBuf::try_new(&abs_foo_bar)?,
                y: None,
            },
            TestFile {
                id: 3,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: Some(CombinedPathBuf::try_new("bar/baz.txt")?),
            },
            TestFile {
                id: 4,
                x: CombinedPathBuf::try_new("foo/bar.txt")?,
                y: Some(CombinedPathBuf::try_new(&abs_bar_baz)?),
            },
        ];

        assert_eq!(expected[0], test_files.find(1).first(&mut connection)?);
        assert_eq!(expected[1], test_files.find(2).first(&mut connection)?);
        assert_eq!(expected[2], test_files.find(3).first(&mut connection)?);
        assert_eq!(expected[3], test_files.find(4).first(&mut connection)?);

        Ok(())
    }
}
