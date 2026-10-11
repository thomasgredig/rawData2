# rawdata2 0.3.5

* remove duplicates that have not ID2 but same filename and same ID

# rawdata2 0.3.4

* add constants for file structure
* handle case of two files with same name and file size but different content, so will return correct path
* clean paths not available any longer in `raw_paths_read()`.

# rawdata2 0.3.3

* for duplicate files keep the one with the lowest ID (for example if legacy imported without sha256 through `raw_init()`)

# rawdata2 0.3.2

* remove `raw_find_ID()`, as it is identical to `raw_file_by_id()`
* remove duplicate IDs, if SHA256 is NA and filesize and filename and ID are the same
* add `raw_export_legacy_rawData()` as part of `raw_init()` to import legacy data
* add progress bar for `raw_update()`

# rawdata2 0.3.1

* `raw_getID()` generates the ID2 value for one or multiple files.

# rawdata2 0.3.0

* add SQLite functionality with `raw_initDB()`
* add `remote` path metadata to `raw_path_append()` and `raw_paths_read()`;
  paths are remote by default and can be marked local with `remote = FALSE`
* TODO: fix issue with path stripping for secondary paths
* allow `raw_find()` to match filenames containing two literal search strings


# rawdata2 0.2.2

* configure additional `raw_update()` file extensions in
  `.rawdata2/config.txt` with `raw_extensions_append()`
* record `raw_file_by_id()` requests in `.rawdata2/RAW_log.txt`
* add `raw_id2_by_date()` to retrieve unique requested ID2 values by date

# rawdata2 0.2.1

* load files with certain file extensions only
* add `found` parameter to `raw_list()` to list found or missing files
* save the size in bytes of each found file in the RAW file catalogue
* add `fastScan` to `raw_update()` to reuse checksums for unchanged-size files

# rawdata2 0.2.0

* add import and export of RAWdata register
* add `raw_info()` for information about an ID
* add `raw_find()` to search for IDs from a list of filenames (partial)
* return full filename based on ID or ID2
* update README.md with workflow

# rawdata2 0.1.1

* implement 6-character Base64 alternative ID as `ID2`
* add a processing dot for `raw_update`

# rawdata2 0.1.0

* update tests
* add columns to paths to include GIT username and date
* `raw_path_trim()` removes any invalid paths

# rawData2 0.0.1

* init `.rawdata2` directory
* append paths with `raw_path_append()`
* read paths with `raw_paths_read()`
* update SHA256 for all files with `raw_update()`
* reads files `raw_files_read()`
* modify update, so that files that change their names are recognized
