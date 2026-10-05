#' Lists all available files
#' @param nLEN Maximum number of trailing characters to show for each filename.
#' @param found Logical; if `TRUE`, return files currently marked as found. If
#'   `FALSE`, return files that are not currently found.
#' @return A data frame containing the ID, ID2, final directory component, and
#'   shortened filename for the files selected by `found`.
#' @importFrom dplyr select mutate filter
#' @importFrom stats na.omit
#' @export
raw_list <- function(IDlist = NULL, nLEN = 40, found = TRUE) {
  if (length(found) != 1L || !is.logical(found) || is.na(found)) {
    stop("'found' must be a single TRUE or FALSE value.")
  }

  IDlist <- na.omit(IDlist)

  d <- raw_files_read()
  if (!is.null(IDlist)) {
    if (is.numeric(IDlist)) {
      # Numeric IDs: c(1, 3, 100, 222)
      d <- filter(d, ID %in% IDlist)

    } else if (is.character(IDlist)) {
      # Base64-style IDs: c("rewA3", "YT33z")
      valid_id2 <- grepl("^[A-Za-z0-9.?]+$", IDlist)

      if (!all(valid_id2)) {
        stop("Character IDlist contains invalid ID2 values.")
      }

      d <- filter(d, ID2 %in% IDlist)

    } else {
      stop("'IDlist' must be numeric or character.")
    }
  }

  d[d$found %in% found, , drop = FALSE] |>
    mutate(filen=basename(file)) |>
    mutate(filename = substr(filen,nchar(filen)-nLEN+1, nchar(filen))) |>
    mutate(lastpath = basename(dirname(file))) |>
    select(ID,ID2,filesize,lastpath,filename)
}
