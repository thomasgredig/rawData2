raw_default_extensions <- c(
  "tiff", "jpg", "jpeg", "png", "ibw", "ras", "rasx", "txt", "csv",
  "bin", "xlsx", "docx", "asc", "nid"
)

#' Read the file extensions used by `raw_update()`
#'
#' @return A character vector of file extensions without leading dots.
#' @noRd
raw_extensions_read <- function() {
  config_file <- file.path(dirname(raw_paths_file()), "config.txt")

  if (!file.exists(config_file)) {
    writeLines(raw_default_extensions, config_file)
    return(raw_default_extensions)
  }

  extensions <- trimws(readLines(config_file, warn = FALSE))
  extensions <- extensions[
    nzchar(extensions) & !startsWith(extensions, "#")
  ]
  extensions <- tolower(sub("^\\.", "", extensions))
  extensions <- unique(extensions[nzchar(extensions)])

  if (length(extensions) == 0L) {
    warning(
      "No file extensions are configured in .rawdata2/config.txt; ",
      "raw_update() will not find files."
    )
  }

  extensions
}

#' Add file extensions used by `raw_update()`
#'
#' @param extensions Character vector of extensions, with or without leading
#'   dots.
#' @return Invisibly, the complete configured extension vector.
#' @export
raw_extensions_append <- function(extensions) {
  if (length(extensions) == 0L ||
      !is.character(extensions) ||
      anyNA(extensions) ||
      any(!nzchar(trimws(extensions)))) {
    stop("'extensions' must be a non-empty character vector.")
  }

  extensions <- tolower(sub("^\\.", "", trimws(extensions)))
  if (any(!grepl("^[[:alnum:]_-]+$", extensions))) {
    stop("Extensions must contain only letters, numbers, '_' or '-'.")
  }

  configured <- raw_extensions_read()
  configured <- unique(c(configured, extensions))
  config_file <- file.path(dirname(raw_paths_file()), "config.txt")
  writeLines(configured, config_file)

  invisible(configured)
}
