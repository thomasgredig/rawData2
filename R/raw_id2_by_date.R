#' Retrieve unique RAW ID2 values requested on a date
#'
#' @param date A single `Date` value or a date string in `YYYY-MM-DD` format.
#' @return A list of unique `ID2` values requested on `date`. An empty list is
#'   returned when no request log exists or no requests match.
#' @export
raw_id2_by_date <- function(date = Sys.Date()) {
  if (inherits(date, "Date")) {
    if (length(date) != 1L || is.na(date)) {
      stop("'date' must be a single non-missing date.")
    }
    date <- as.character(date)
  } else if (is.character(date) &&
             length(date) == 1L &&
             !is.na(date) &&
             grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date)) {
    parsed_date <- as.Date(date)
    if (is.na(parsed_date) || as.character(parsed_date) != date) {
      stop("'date' must be a valid date in 'YYYY-MM-DD' format.")
    }
  } else {
    stop("'date' must be a single Date or 'YYYY-MM-DD' value.")
  }

  log_file <- file.path(dirname(raw_paths_file()), "RAW_file_by_id.txt")
  if (!file.exists(log_file)) {
    return(list())
  }

  requests <- read.delim(
    log_file,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = character()
  )

  required_columns <- c("ID2", "timestamp")
  if (!all(required_columns %in% names(requests))) {
    stop(
      "RAW_file_by_id.txt is missing required column(s): ",
      paste(setdiff(required_columns, names(requests)), collapse = ", ")
    )
  }

  requested_id2 <- requests$ID2[
    substr(requests$timestamp, 1L, 10L) == date &
      !is.na(requests$ID2) &
      nzchar(requests$ID2)
  ]

  as.list(unique(requested_id2))
}
