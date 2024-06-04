use std::path::Path;

/// Common traits that all derived PathBuf/Path types should have
/// and types / functions that can operate on all of the derived
/// path types in this crate.

pub trait HasParent: AsRef<Self::PathType> + AsRef<Path> {
    /// The reference type for this type. e.g. AbsolutePath for AbsolutePathBuf
    type PathType: HasParent + ?Sized;

    /// Get a reference to the parent directory, if one exists.
    fn parent(&self) -> Option<&Self::PathType>;

    /// Returns an iterator over all of this path's parents
    fn parents(&self) -> ParentIterator<Self::PathType> {
        ParentIterator {
            current: self.parent(),
        }
    }

    /// Returns an iterator over both this path, and all of its parents.
    fn parents_and_self(&self) -> ParentIterator<Self::PathType> {
        ParentIterator {
            current: Some(self.as_ref()),
        }
    }

    /// Ensures that the parent path, if there is one, exists.
    fn ensure_parent_exists(&self) -> std::io::Result<()> {
        crate::create_parent_dir(self)
    }
}

/// Iterator that starts at 'current' and iterates over parent paths until there are no more.
pub struct ParentIterator<'a, T: HasParent + ?Sized> {
    current: Option<&'a T>,
}

impl<'a, T: ?Sized + HasParent<PathType = T>> Iterator for ParentIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.current;
        if let Some(path) = ret {
            self.current = path.parent();
        }
        ret
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use gazebo::prelude::SliceExt;

    use crate::common::HasParent;
    use crate::AbsolutePath;
    use crate::AbsolutePathBuf;

    #[test]
    fn upward_traversal_works() {
        let cwd = std::env::current_dir().unwrap();
        let foo_bar_baz = cwd.join("foo/bar/baz");
        let path = AbsolutePath::new_unchecked(&foo_bar_baz);

        let mut expected = vec![
            AbsolutePathBuf::new_unchecked(cwd.join("foo/bar/baz")),
            AbsolutePathBuf::new_unchecked(cwd.join("foo/bar")),
            AbsolutePathBuf::new_unchecked(cwd.join("foo")),
        ];
        for i in (0..cwd.iter().count()).rev() {
            expected.push(AbsolutePathBuf::new_unchecked(PathBuf::from_iter(
                cwd.components().take(i + 1),
            )));
        }

        let parents: Vec<&AbsolutePath> = path.parents().collect();
        let parents_and_self: Vec<&AbsolutePath> = path.parents_and_self().collect();

        assert_eq!(parents_and_self, expected.map(|p| p.as_absolute_path()));
        assert_eq!(parents, expected[1..].map(|p| p.as_absolute_path()));
    }
}
