#' Read the RAW file catalogue
#' @return A data frame containing the RAW file catalogue. If the catalogue
#'   does not exist, an empty data frame with the catalogue columns is returned.
#' @importFrom utils read.csv
#' @export
raw_files_read <- function() {
  raw_files_file <- file.path(
    dirname(raw_paths_file()),
    "RAW_files.csv"
  )

  if (!file.exists(raw_files_file)) {
    return(data.frame(
      file = character(),
      sha256 = character(),
      filesize = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  raw_files <- read.csv(
    raw_files_file,
    stringsAsFactors = FALSE,
    colClasses = c(file = "character", sha256 = "character",
                   filesize = "numeric")
  )

  if (!"filesize" %in% names(raw_files)) {
    raw_files$filesize <- NA_real_
  }

  raw_files
}
