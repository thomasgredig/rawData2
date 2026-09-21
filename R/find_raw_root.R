#' Finds the directory with .rawdata2
#'
#' Either it finds the .rawdata2 directory or the nearest .git directory
#' @export
find_raw_root <- function(path = getwd()) {
  path <- normalizePath(path, mustWork = TRUE)

  repeat {
    if (dir.exists(file.path(path, ".rawdata2")) ||
        dir.exists(file.path(path, ".git"))) {
      return(path)
    }

    parent <- dirname(path)
    if (parent == path) return(NA_character_)
    path <- parent
  }
}

