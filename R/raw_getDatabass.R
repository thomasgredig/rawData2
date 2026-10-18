#' Return the path to the RAW SQLite database
#'
#' @return The path to the configured SQLite database file.
#' @export
raw_getDatabase <- function() {
  file_database = raw_get_SQL_database()
  if (!file.exists(file_database)) {
    raw_initDB()
  }
  file_database
}
