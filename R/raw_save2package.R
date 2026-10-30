#' Saves all relevant data to the package
#' @importFrom usethis use_data
#' @export
raw_export2package <- function() {
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

  raw_files |> mutate(filename = basename(file)) |> select(ID,ID2,crc,filesize,filename) -> d
  write.csv(d, "data/RAW.csv", row.names = FALSE)
}

#' @export
raw_config_read <- function() {
  ext <- raw_extensions_read()

  list(
    version = packageVersion("rawdata2"),
    raw_extensions = ext
  )
}
