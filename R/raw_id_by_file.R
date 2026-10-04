#' Retrieve RAW file IDs by filename or partial filename
#' @param filename A single complete or partial filename to match.
#' @param ignore.case Logical; should matching ignore case?
#' @param found_only Logical; if `TRUE`, return only files currently marked as
#'   found.
#' @return An integer vector of matching numeric RAW IDs. A zero-length vector
#'   is returned when no files match.
#' @export
raw_id_by_file <- function(filename,
                           ignore.case = FALSE,
                           found_only = FALSE) {
  if (length(filename) != 1 || !is.character(filename)) {
    stop("'filename' must be a single character string.")
  }

  raw_files <- raw_files_read()

  if (nrow(raw_files) == 0) {
    return(integer())
  }

  search_files <- raw_files$file
  if (ignore.case) {
    filename <- tolower(filename)
    search_files <- tolower(search_files)
  }

  matches <- grepl(
    filename,
    search_files,
    fixed = TRUE
  )

  if (found_only && "found" %in% names(raw_files)) {
    matches <- matches & raw_files$found
  }

  IDs <- raw_files$ID2[matches]
  na_ids <- is.na(IDs)
  IDs[na_ids] <-  paste0("00000",raw_files$ID[matches][na_ids])

  if (length(IDs) == 0) {
    warning("No RAW file matched: ", filename)
    return(character())
  }

  IDs
}




#' Retrieve file and SHA
#' @param ID A numeric RAW file identifier.
#' @return A one-row data frame containing the matching RAW file record, or
#'   `NULL` when no record matches.
#' @export
raw_file_record_by_id <- function(ID) {
  raw_files <- raw_files_read()

  result <- raw_files[raw_files$ID == ID, , drop = FALSE]

  if (nrow(result) == 0) {
    warning("No RAW file found with ID: ", ID)
    return(NULL)
  }

  result
}




