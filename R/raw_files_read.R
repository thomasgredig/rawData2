#' Read the RAW file catalogue
#' @param path Directory containing the RAW file catalogue.
#' @return A data frame containing the RAW file catalogue. If the catalogue
#'   does not exist, an empty data frame with the catalogue columns is returned.
#' @importFrom utils read.csv
#' @export
raw_files_read <- function(path = dirname(raw_paths_file())) {
  raw_files_file <- file.path(
    path,
    FILE_FILES
  )

  if (!file.exists(raw_files_file)) {
    message("RAW file list not found: ", raw_files_file)
    return(data.frame(
      ID = numeric(),
      ID2 = character(),
      file = character(),
      sha256 = character(),
      found = logical(),
      filesize = numeric(),
      crc = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  col_classes <- c(
    ID = "numeric",
    ID2 = "character",
    file = "character",
    sha256 = "character",
    found = "logical",
    filesize = "numeric",
    crc = "numeric"
  )
  header <- read.csv(
    raw_files_file,
    stringsAsFactors = FALSE,
    nrows = 0
  )

  raw_files <- read.csv(
    raw_files_file,
    stringsAsFactors = FALSE,
    colClasses = col_classes[names(col_classes) %in% names(header)]
  )

  if (!"filesize" %in% names(raw_files)) {
    raw_files$filesize <- NA_real_
  }
  if (!"crc" %in% names(raw_files)) {
    raw_files$crc <- NA_real_
  }

  raw_files <- raw_files[, c(
    "ID", "ID2", "file", "sha256", "found", "filesize", "crc",
    setdiff(names(raw_files),
            c("ID", "ID2", "file", "sha256", "found", "filesize", "crc"))
  ), drop = FALSE]

  raw_files
}
