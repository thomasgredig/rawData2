# Copilot instructions for rawdata2

## Build and test

This repository is an R package (`rawData2.Rproj`) using testthat edition 3.

- Run the full test suite:
  `R -q -e 'testthat::test_local()'`
- Run one test file:
  `R -q -e 'testthat::test_file("tests/testthat/test-raw-find.R")'`
- Run a focused test by name:
  `R -q -e 'testthat::test_local(filter = "raw_update")'`
- Build the package:
  `R CMD build .`
- Run the package check:
  `R CMD check .`

The package depends on DBI, RSQLite, cli, digest, dplyr, rprojroot, stats,
tools, and utils; testthat (version 3 or newer) is a suggested dependency.
There is no configured lint command. The RStudio project is configured to use
devtools for package operations and roxygen2 for `rd`, `collate`, and
`namespace` generation.

## Architecture

- This is an R package whose public API is the exported `raw_*` functions in
  `R/`; roxygen comments generate the corresponding entries in `NAMESPACE` and
  `man/`.
- The package maintains a persistent `.rawdata2` data area. Its main files are
  `RAW_paths.csv` (registered search roots and metadata), `RAW_files.csv`
  (the file catalogue), `extensions.txt` (scan extensions), `RAW_log.txt`
  (optional lookup audit log), and `RAW_database.sqlite` (the separate SQLite
  store initialized by `raw_initDB()`).
- Root/data-location resolution is centralized in `find_raw_root.R`. For an R
  package it uses `tools::R_user_dir()` for package data; outside a package it
  falls back to the user home directory. Functions such as `raw_paths_file()`
  should be used instead of reconstructing the storage path.
- `raw_init()` bootstraps the data area, handles legacy catalogue migration,
  imports compatible shared registers, and can mark existing paths
  unsearchable. Path metadata and normalization are handled by `raw_path.R`.
- `raw_update()` is the catalogue pipeline: it registers any supplied paths,
  scans only searchable directories for configured extensions, hashes files
  with SHA-256, records file sizes, preserves IDs across renames/moves when
  content matches, removes duplicate content entries, and writes
  `RAW_files.csv`.
- A catalogue record has a numeric `ID` and a shorter `ID2` derived from the
  first seven hexadecimal characters of the SHA-256 digest using the package's
  base64 helper. `raw_file_by_id()` first checks the recorded path, then
  searches existing searchable roots; when multiple candidates are possible it
  uses file size and SHA-256 to disambiguate.
- Register export/import is CSV-based. Imported registers are validated for
  required columns and renamed after successful import so they are not
  repeatedly consumed. Legacy records without hashes remain supported and are
  reconciled during later updates.
- Tests live under `tests/testthat/` and generally create temporary source
  directories and files, initialize the package state, exercise the public
  API, and clean up temporary files. Do not rely on a developer's real RAW
  directories or catalogue.

## Repository conventions

- Keep public functions and their roxygen documentation in `R/`, with
  implementation helpers unexported unless they are part of the documented
  API. Regenerate `NAMESPACE` and `.Rd` files with roxygen rather than editing
  generated files by hand.
- Use the existing storage constants in `R/CONST.R` for `.rawdata2` filenames
  and catalogue columns. Read and write catalogues through the existing
  helpers (`raw_paths_read()`, `raw_files_read()`, `raw_path_write()`, and
  `rawFilesSave()`) so older catalogue formats continue to be upgraded.
- Preserve compatibility behavior: older path files may lack `remote`, older
  file catalogues may lack `filesize`, and legacy data may lack SHA-256/`ID2`.
  Existing records should be normalized or migrated rather than rejected
  solely because they use an older schema.
- Path operations should account for normalized paths, duplicate roots, and
  parent/child roots. Only paths marked `searchable == TRUE` are scanned or
  searched for file recovery; `remote` is metadata and does not by itself
  control scanning.
- File discovery is extension-configured and case-insensitive. Use
  `raw_extensions_read()` and `raw_extensions_append()` rather than embedding
  extension lists in scanning code.
- User-facing functions follow the package's established signaling: warnings
  for missing files, paths, or ambiguous/invalid data; messages for
  initialization and catalogue progress; and `NA_character_`, empty results,
  or `NULL` only where the existing API documents those outcomes.
- Tests use `test_that()` with `expect_*` assertions and should isolate
  filesystem state with `tempfile()`/`dir.create()`. When testing catalogue
  changes, verify both the stable identifier behavior and the persisted
  record/path behavior.
