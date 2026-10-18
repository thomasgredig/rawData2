#' Status of the RAW files
#' @export
raw_status <- function() {
  root <- find_raw_root()
  if (dir.exists(root)) {
    message("Root directory:    ", root)
  } else {
    message("Root directory not found: ", root)
  }

  df_path <- raw_paths_read()
  message("Paths found:       ", nrow(df_path))

  df_files <- raw_files_read()
  if(nrow(df_files)>0) {
    message("Files in database: ",nrow(df_files))

    df_files_found <- raw_list()
    perc = signif(nrow(df_files_found)/nrow(df_files)*100,3)
    message("Files found:       ", nrow(df_files_found), " (",perc,"%)")
  }


  q <- raw_extensions_read()
  message("File extensions:  ", length(q)," (", paste(q, collapse=","), ")")
}
