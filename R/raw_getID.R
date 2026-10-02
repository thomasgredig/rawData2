#' Finds
#' @export
raw_getID <- function(filename) {
  if (!is.character(filename) || length(filename) == 0L) {
    stop("'filename' must be a non-empty character vector.")
  }

  results <- lapply(filename, function(f) {
    found <- file.exists(f)

    if (found) {
      sha256 <- digest::digest(
        f,
        algo = "sha256",
        file = TRUE
      )

      ID2 <- as.character(
        base64(strtoi(substr(sha256, 1L, 7L), base = 16L))
      )

      filesize <- file.size(f)

    } else {
      warning("File not found: ", f)

      sha256 <- NA_character_
      ID2 <- NA_character_
      filesize <- NA_real_
    }

    data.frame(
      ID = NA_integer_,
      ID2 = ID2,
      file = basename(f),
      sha256 = sha256,
      filesize = filesize,
      found = found,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
