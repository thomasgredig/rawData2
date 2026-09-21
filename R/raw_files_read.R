#' Read the RAW file catalogue
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
      stringsAsFactors = FALSE
    ))
  }

  read.csv(
    raw_files_file,
    stringsAsFactors = FALSE,
    colClasses = c(
      file = "character",
      sha256 = "character"
    )
  )
}
