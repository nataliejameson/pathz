# pathz

Simple way to semantically require relative or absolute paths.

## tl;dr;

There are four main classes in this crate that have various constraints that are validated by their constructors. See `try_new` on each of these classes.

Each set of constraints has a `*Path` and `*PathBuf` variant, mirroring what is in the standard lib:

- `RelativePath` / `RelativePathBuf`: These paths must not start with `/`
- `AbsolutePath` / `AbsolutePathBuf`: These paths must start with a `/` and be a complete path.
- `CombinedPath` / `CombinedPathBuf`: These paths may be either a relative or absolute one.
- `ForwardPath`  / `ForwardPathBuf` : These paths are relative paths that are normalized. '.' and '..' are not permitted.

## Adding to a project

```toml
# Cargo.toml
[dependencies]
pathz = { git = "https://github.com/nataliejameson/pathz", tag = "0.1.0" }
```

## Path / PathBuf variants

Much like `std::path::Path` and `std::path::PathBuf`, each path type has an owned, and unowned variant.

The `Path` variants only validate on construction; they otherwise are a transparent wrapper around a `std::path::Path`.
For some times, this can fail if the input path is not normalized, as we can't do any coalescing on construction.

The `PathBuf` variants validate on construction, and sometimes will also make strings that would fail validation on
`Path` types pass. e.g. removing `..` in the middle of a `ForwardPathBuf` (though not at the beginning!)

## Extra features

If the `display` feature is enabled, a human readable `Display` impl is made available for each type.
If the `serde` feature is enabled, a serialization / deserialization impl is made available that also validates path constraints on deserialization.
If the `diesel` feature is enabled, a field type is added that allows serialization and deserialization in Diesel (`ToSql`/`FromSql` impls are provided)

## Random notes

For `CombinedPath`, `CombinedPath::try_into_absolute()` and `CombinedPath::try_into_absolute_in_cwd()` can be used to either return an absolute path, or resolve it against an absolute path if the inner data is a relative path.

Because `FromStr` is implemented, things like `CombinedPathBuf` can be used by `clap`.

Like `Path`, the `*Path` implementations are ~zero size containers over the inner `Path` reference object. It's similar in the case of `PathBuf` variants.

These types all can deref into Path like structs, so pretty much any stdlib functions should work with them.

Many of these types are convertible, and can be converted for ~free. See e.g. `ForwardPath::as_relative()`
