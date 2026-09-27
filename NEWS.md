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
