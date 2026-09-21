#' Retrieve a RAW filename by ID
#' @export
raw_file_by_id <- function(ID) {
  if (length(ID) != 1 || is.na(ID)) {
    stop("'ID' must be a single non-missing value.")
  }

  raw_files <- raw_files_read()

  if (nrow(raw_files) == 0) {
    return(NA_character_)
  }

  match_index <- match(as.integer(ID), as.integer(raw_files$ID))

  if (is.na(match_index)) {
    warning("No RAW file found with ID: ", ID)
    return(NA_character_)
  }

  raw_files$file[match_index]
}


#' Retrieve file and SHA
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
