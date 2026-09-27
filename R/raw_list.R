#' Lists all available files
#' @param nLEN Maximum number of trailing characters to show for each filename.
#' @param found Logical; if `TRUE`, return files currently marked as found. If
#'   `FALSE`, return files that are not currently found.
#' @return A data frame containing the ID, ID2, final directory component, and
#'   shortened filename for the files selected by `found`.
#' @importFrom dplyr select mutate
#' @export
raw_list <- function(nLEN = 40, found = TRUE) {
  if (length(found) != 1L || !is.logical(found) || is.na(found)) {
    stop("'found' must be a single TRUE or FALSE value.")
  }

  d <- raw_files_read()
  d[d$found %in% found, , drop = FALSE] |>
    mutate(filen=basename(file)) |>
    mutate(filename = substr(filen,nchar(filen)-nLEN+1, nchar(filen))) |>
    mutate(lastpath = basename(dirname(file))) |>
    dplyr::select(ID,ID2,filesize,lastpath,filename)
}
