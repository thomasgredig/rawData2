#' Lists all available files
#' @param nLEN Maximum number of trailing characters to show for each filename.
#' @return A data frame containing the ID, ID2, final directory component, and
#'   shortened filename for files currently marked as found.
#' @importFrom dplyr select mutate
#' @export
raw_list <- function(nLEN=40) {
  d <- raw_files_read()
  d[d$found,] |>
    mutate(filen=basename(file)) |>
    mutate(filename = substr(filen,nchar(filen)-nLEN+1, nchar(filen))) |>
    mutate(lastpath = basename(dirname(file))) |>
    select(ID,ID2,lastpath,filename)
}
