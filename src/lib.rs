#![deny(clippy::all)]

mod absolute;
mod combined;
pub mod common;
pub mod errors;
mod forward;
mod relative;
mod resolved_absolute;

use std::path::Path;

pub use absolute::AbsolutePath;
pub use absolute::AbsolutePathBuf;
pub use combined::CombinedPath;
pub use combined::CombinedPathBuf;
pub use forward::ForwardPath;
pub use forward::ForwardPathBuf;
pub use relative::RelativePath;
pub use relative::RelativePathBuf;
pub use resolved_absolute::ResolvedAbsolutePathBuf;

/// If the path has a parent, create that parent directory and all of its parent dirs
/// using [`std::fs::create_dir_all()`]
fn create_parent_dir<P: AsRef<Path>>(p: P) -> std::io::Result<()> {
    if let Some(parent) = p.as_ref().parent() {
        std::fs::create_dir_all(parent)
    } else {
        Ok(())
    }
}

#[cfg(all(test, feature = "diesel"))]
#[macro_use]
extern crate diesel;

#[cfg(all(test, feature = "diesel"))]
pub(crate) mod diesel_helpers {
    use diesel::sql_query;
    use diesel::Connection;
    use diesel::RunQueryDsl;
    use diesel::SqliteConnection;

    pub(crate) mod schema {
        table! {
            test_files (id) {
                id -> Integer,
                x -> Text,
                y -> Nullable<Text>,
            }
        }
    }

    pub(crate) fn create_table() -> anyhow::Result<SqliteConnection> {
        let mut connection = diesel::sqlite::SqliteConnection::establish(":memory:")?;
        diesel::sql_query(
            "CREATE TABLE test_files (id PRIMARY KEY NOT NULL, x TEXT NOT NULL, y TEXT NULL)",
        )
        .execute(&mut connection)?;
        Ok(connection)
    }

    pub(crate) fn insert_values(
        connection: &mut SqliteConnection,
        values: &[(i32, &str, Option<&str>)],
    ) -> anyhow::Result<()> {
        for (pk, x, y) in values {
            let y = match y {
                None => "NULL".to_owned(),
                Some(value) => {
                    format!("\"{}\"", value)
                }
            };
            sql_query(format!(
                "INSERT INTO test_files (id, x, y) VALUES ({}, \"{}\", {});",
                pk, x, y
            ))
            .execute(connection)?;
        }
        Ok(())
    }
}
