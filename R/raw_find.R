#' Find IDs from partial filenames
#' @param filename_partial A character vector of complete or partial filenames
#'   to match.
#' @param filename_partial2 An optional single complete or partial filename
#'   string. When supplied, only files matching both search strings are
#'   returned.
#' @param type Reserved for compatibility; currently ignored.
#' @return An integer vector of matching numeric RAW IDs. A zero-length vector
#'   is returned when no filenames match.
#' @export
raw_find <- function(
    filename_partial,
    filename_partial2 = NULL,
    type = "SHA256"
) {
  ID_list <- c()

  IDs2 <- NULL
  if (!is.null(filename_partial2)) {
    IDs2 <- raw_id_by_file(filename_partial2)
  }

  for(fname in filename_partial) {
    ID <- raw_id_by_file(fname)
    if (!is.null(IDs2)) {
      ID <- intersect(ID, IDs2)
    }
    ID_list = c(ID_list, ID)
  }

  ID_list
}
