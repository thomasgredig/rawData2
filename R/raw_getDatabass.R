#' returns name of database
#' @export
raw_getDatabase <- function() {
  file_database = raw_get_SQL_database()
  if (!file.exists(file_database)) {
    raw_initDB()
  }
  file_database
}
