#' Lists all available files
#' @param nLEN maximum number of characters for file
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
