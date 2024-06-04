//! The various ways that path operations can fail.
//!
//! This often is something simple like joining absolute paths to relative ones,
//! but for operations that can fail in multiple ways, there are some combined
//! types.

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("Attempted to join `{}` to non-relative path `{}`", .0, .1)]
pub struct JoinedAbsolute(pub String, pub String);

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("`{}` must be normalized, but contained '.' or '..'", .0)]
pub struct WasNotNormalized(pub String);

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("`{}` could not be normalized", .0)]
pub struct NormalizationFailed(pub String);

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("`{}` was not an absolute path", .0)]
pub struct NotAbsolute(pub String);

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("`{}` was not a relative path", .0)]
pub struct NotRelative(pub String);

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum AbsolutePathNewError {
    #[error(transparent)]
    WasNotNormalized(WasNotNormalized),
    #[error(transparent)]
    NotAbsolute(NotAbsolute),
}

impl From<WasNotNormalized> for AbsolutePathNewError {
    fn from(e: WasNotNormalized) -> Self {
        AbsolutePathNewError::WasNotNormalized(e)
    }
}
impl From<NotAbsolute> for AbsolutePathNewError {
    fn from(e: NotAbsolute) -> Self {
        AbsolutePathNewError::NotAbsolute(e)
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum AbsolutePathBufNewError {
    #[error(transparent)]
    NormalizationFailed(NormalizationFailed),
    #[error(transparent)]
    NotAbsolute(NotAbsolute),
}

impl From<NormalizationFailed> for AbsolutePathBufNewError {
    fn from(e: NormalizationFailed) -> Self {
        AbsolutePathBufNewError::NormalizationFailed(e)
    }
}
impl From<NotAbsolute> for AbsolutePathBufNewError {
    fn from(e: NotAbsolute) -> Self {
        AbsolutePathBufNewError::NotAbsolute(e)
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum AbsoluteJoinError {
    #[error(transparent)]
    NormalizationFailed(NormalizationFailed),
    #[error(transparent)]
    JoinedAbsolute(JoinedAbsolute),
}

impl From<NormalizationFailed> for AbsoluteJoinError {
    fn from(e: NormalizationFailed) -> Self {
        AbsoluteJoinError::NormalizationFailed(e)
    }
}
impl From<JoinedAbsolute> for AbsoluteJoinError {
    fn from(e: JoinedAbsolute) -> Self {
        AbsoluteJoinError::JoinedAbsolute(e)
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum CombinedJoinError {
    #[error(transparent)]
    NormalizationFailed(NormalizationFailed),
    #[error(transparent)]
    JoinedAbsolute(JoinedAbsolute),
}

impl From<NormalizationFailed> for CombinedJoinError {
    fn from(e: NormalizationFailed) -> Self {
        CombinedJoinError::NormalizationFailed(e)
    }
}
impl From<JoinedAbsolute> for CombinedJoinError {
    fn from(e: JoinedAbsolute) -> Self {
        CombinedJoinError::JoinedAbsolute(e)
    }
}

impl From<AbsoluteJoinError> for CombinedJoinError {
    fn from(e: AbsoluteJoinError) -> Self {
        match e {
            AbsoluteJoinError::NormalizationFailed(e) => Self::NormalizationFailed(e),
            AbsoluteJoinError::JoinedAbsolute(e) => Self::JoinedAbsolute(e),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum ForwardPathNewError {
    #[error(transparent)]
    WasNotNormalized(WasNotNormalized),
    #[error(transparent)]
    NotRelative(NotRelative),
}

impl From<WasNotNormalized> for ForwardPathNewError {
    fn from(e: WasNotNormalized) -> Self {
        ForwardPathNewError::WasNotNormalized(e)
    }
}
impl From<NotRelative> for ForwardPathNewError {
    fn from(e: NotRelative) -> Self {
        ForwardPathNewError::NotRelative(e)
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum ForwardPathBufNewError {
    #[error(transparent)]
    NormalizationFailed(NormalizationFailed),
    #[error(transparent)]
    NotRelative(NotRelative),
}

impl From<NormalizationFailed> for ForwardPathBufNewError {
    fn from(e: NormalizationFailed) -> Self {
        ForwardPathBufNewError::NormalizationFailed(e)
    }
}
impl From<NotRelative> for ForwardPathBufNewError {
    fn from(e: NotRelative) -> Self {
        ForwardPathBufNewError::NotRelative(e)
    }
}

#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
pub enum ForwardJoinError {
    #[error(transparent)]
    NormalizationFailed(NormalizationFailed),
    #[error(transparent)]
    JoinedAbsolute(JoinedAbsolute),
}

impl From<NormalizationFailed> for ForwardJoinError {
    fn from(e: NormalizationFailed) -> Self {
        ForwardJoinError::NormalizationFailed(e)
    }
}
impl From<JoinedAbsolute> for ForwardJoinError {
    fn from(e: JoinedAbsolute) -> Self {
        ForwardJoinError::JoinedAbsolute(e)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum RelativeToError {
    #[error("Provided paths are identical, and cannot be relativized")]
    PathsAreIdentical,
}
