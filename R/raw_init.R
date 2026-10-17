#' Initializes the .rawdata2 directory
#'
#' @return The normalized path to the initialized `.rawdata2` directory.
#' @param make_paths_unsearchable marks all paths as unsearchable
#' @export
raw_init <- function(make_paths_unsearchable = FALSE) {
  root_dir <- find_raw_root()

  if (is.na(root_dir)) {
    root_dir = getwd()
    message("No dataset root found, using current directory: ", root_dir)
  } else {
    message("Dataset root: ", root_dir)
  }

  rawdata_dir <- file.path(root_dir, RAWDATA_FOLDER)

  if (!dir.exists(rawdata_dir)) {
    dir.create(rawdata_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", rawdata_dir)

    # check for legacy .rawdata2 directory elsewhere and copy files as needed
    path_legacy = getLegacyRoot()
    rawdata_legacy <- file.path(path_legacy, RAWDATA_FOLDER_LEGACY)
    if (dir.exists(rawdata_legacy)) {
      files_to_move = c(FILE_PATHS, FILE_FILES, FILE_EXT, FILE_LOG, FILE_DB)
      for(f in files_to_move) {
        fname = file.path(rawdata_legacy, f)
        if (file.exists(f)) {
          message("Moving legacy file: ", fname)
          file.rename(from = fname, to = file.path(rawdata_dir,f))
        }
      }
    }
  } else {
    message("Directory already exists: ", rawdata_dir)
    d_paths = raw_paths_read()
    message("Found ", nrow(d_paths), " paths.")
    d_files <- raw_files_read()
    message("Found ", nrow(d_files), " files")
  }

  # create a register from rawData as needed
  raw_export_legacy_rawData(root_dir)
  raw_import_register()

  # makes all paths unsearchable
  if (make_paths_unsearchable) {
    df <- raw_paths_read()
    if (nrow(df)>0) {
      df$searchable = FALSE
      raw_path_write(df)
    }
  }

  invisible(normalizePath(rawdata_dir, mustWork = TRUE))
}
