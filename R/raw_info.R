#' Information about the file
#' @param ID A numeric catalogue ID or a base64-style `ID2` value or integer `ID`.
#' @return A list containing the numeric ID, base64-style ID, filename, found
#'   status, and resolved full filename.
#' @export
raw_info <-function(ID) {
  idx <- raw_idxByID(ID)
  d <- raw_files_read()[idx,]
  full_filename <- raw_file_by_id(d$ID)
  list(
    ID = d$ID,
    ID2 = d$ID2,
    filename = basename(d$file),
    filesize = d$filesize,
    sha256 = d$sha256,
    found = d$found,
    fullname = full_filename
  )
}


#' Returns index in raw_files_read()
#' @param ID A numeric catalogue ID or a base64-style `ID2` value
#' @return The matching row index, or `NA_character_` when no catalogue entry
#'   matches.
#' @noRd
raw_idxByID <- function(ID) {
  if (length(ID) != 1L || is.na(ID)) {
    stop("'ID' must be a single non-missing value.")
  }

  raw_files <- raw_files_read()

  if (nrow(raw_files) == 0L) {
    return(NA_character_)
  }

  # Match either numeric ID or base64-style ID2.
  if (is.numeric(ID)) {
    match_index <- match(ID, raw_files$ID)
  } else {
    ID <- as.character(ID)

    # First try ID2.
    match_index <- match(ID, as.character(raw_files$ID2))

    # Also allow a numeric ID supplied as character text.
    if (is.na(match_index) && grepl("^[0-9]+$", ID)) {
      match_index <- match(as.integer(ID), raw_files$ID)
    }
  }

  if (is.na(match_index)) {
    warning("No RAW file found with ID: ", ID)
    return(NA_character_)
  }

  match_index
}
