#' Initializes the .rawdata2 directory
#'
#' @export
raw_init <- function() {
  root_dir <- find_raw_root()

  if (is.na(root_dir)) {
    root_dir = getwd()
    message("No dataset root found, using current directory: ", root_dir)
  } else {
    message("Dataset root: ", root_dir)
  }

  rawdata_dir <- file.path(root_dir, ".rawdata2")

  if (!dir.exists(rawdata_dir)) {
    dir.create(rawdata_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", rawdata_dir)
  } else {
    message("Directory already exists: ", rawdata_dir)
    d_paths = raw_paths_read()
    message("Found ", nrow(d_paths), " paths.")
    d_files <- raw_files_read()
    message("Found ", nrow(d_files), " files")
  }

  raw_import_register()

  normalizePath(rawdata_dir, mustWork = TRUE)
}

