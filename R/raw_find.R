#' Find IDs from partial filenames
#' @export
raw_find <-function(filename_partial, type="SHA256"){
  ID_list <- c()
  for(fname in filename_partial) {
    ID <- raw_id_by_file(fname)
    ID_list = c(ID_list, ID)
  }
  ID_list
}
