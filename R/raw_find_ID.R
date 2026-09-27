#' Retrieve a RAW filename by ID2
#' @param ID2 A base64-style RAW file identifier.
#' @return The matching filename, resolved against the registered RAW paths
#'   when possible, or `NA_character_` when the catalogue is empty. If the ID
#'   is not found, a warning is issued and `NA_character_` is returned.
#' @export
raw_find_ID <- function(ID2) {
  raw_files <- raw_files_read()

  if (nrow(raw_files) == 0) {
    return(NA_character_)
  }

  match_index = which(raw_files$ID2 == ID2)

  if (length(match_index)==0) {
    warning("No RAW file found with ID: ", ID2)
    return(NA_character_)
  }

  filename = raw_files$file[match_index]
  fullname <- filename

  raw_paths <- raw_paths_read()
  for(p in raw_paths$path) {
    fullname = file.path(p, filename)
    if (file.exists(fullname)) break
  }
  fullname
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

  matches <- grepl(
    filename,
    raw_files$file,
    fixed = TRUE,
    ignore.case = ignore.case
  )

  if (found_only && "found" %in% names(raw_files)) {
    matches <- matches & raw_files$found
  }

  IDs <- raw_files$ID[matches]

  if (length(IDs) == 0) {
    warning("No RAW file matched: ", filename)
    return(integer())
  }

  as.integer(IDs)
}
