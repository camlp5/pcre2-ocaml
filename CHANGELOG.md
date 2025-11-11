# Changelog

## [8.0.4] - 2025-10-19

### Added

- Provide a `make test` convenience target and generate
  `lib/compile_commands.json` to streamline running the suite and IDE
  integration.
- Introduce GitHub automation (Dependabot updates plus linting, formatting, and
  pre-commit workflows) mirroring the original `pcre-ocaml` project.

### Changed

- Reorganize the repository to match the upstream layout, moving library sources
  to `lib/`, refreshing documentation, and updating example descriptions.

### Fixed

- Ensure the `Pcre2.BadPattern` exception allocates short OCaml strings to avoid
  trailing garbage or embedded NUL bytes in error messages.
- Correct the offset-vector pointer cast in `pcre2_stubs.c` to remove undefined
  behavior when copying match offsets.

## [8.0.3] - 2025-02-15

### Fixed

- Make `caml_alloc_some` static to prevent clashes with the `pcre` package when
  targeting OCaml 4.08-4.11.

## [8.0.2] - 2024-12-26

### Added

- Restore compatibility with OCaml 4.08 through 4.11. Thanks to @nojb.

## [8.0.1] - 2024-12-20

### Changed

- Merge the legacy `pcre-ocaml` changes into `pcre2-ocaml`.

### Fixed

- Correct `full_split` so that non-capturing groups are identified properly.

## [7.5.3] - 2024-12-23

### Fixed

- Address the `full_split` regression. Thanks to @mmottl.

## [7.5.2] - 2023-09-06

### Added

- Introduce the initial unit test that exercises `full_split`.

### Fixed

- Resolve the `full_split` bug affecting split classification.

## [7.5.1] - 2023-09-01

### Added

- Create the initial `pcre2-ocaml` bindings derived from the original
  `pcre-ocaml` project.
