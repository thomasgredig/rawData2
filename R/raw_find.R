#' Find IDs from partial filenames
#' @param filename_partial A character vector of complete or partial filenames
#'   to match.
#' @param type Reserved for compatibility; currently ignored.
#' @return An integer vector of matching numeric RAW IDs. A zero-length vector
#'   is returned when no filenames match.
#' @export
raw_find <-function(filename_partial, type="SHA256"){
  ID_list <- c()
  for(fname in filename_partial) {
    ID <- raw_id_by_file(fname)
    ID_list = c(ID_list, ID)
  }
  ID_list
}
