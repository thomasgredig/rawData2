#' Saves all relevant data to the package
#' @importFrom usethis use_data
#' @export
raw_save2package <- function() {
  # check whether there is data
  root_dir <- find_raw_root()
  if (!dir.exists(root_dir)) {
    stop("No root directory found; initialize pacakge with `raw_init()` first.")
  }

  # check if there is a package
  pkgname <- tryCatch(
    basename(rprojroot::find_root(rprojroot::is_r_package)),
    error = function(e) NULL
  )
  if (is.null(pkgname)) {
    stop("R package not identified; create a new project and use type `R package`, then run this to save the RAW data to your package.")
  }

  raw_paths <- raw_paths_read()
  raw_files <- raw_files_read()
  raw_config <- raw_config_read()
  usethis::use_data(raw_paths,
                    raw_files,
                    raw_config, internal=FALSE, overwrite=TRUE)
}

#' @export
raw_config_read <- function() {
  ext <- raw_extensions_read()

  list(
    version = packageVersion("rawdata2"),
    raw_extensions = ext
  )
}
