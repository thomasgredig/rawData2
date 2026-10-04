#' Exports legacy rawData if available
#' @export
raw_export_legacy_rawData <- function(root_dir) {
  folder_data = file.path(root_dir, "data")
  rawBase_file = file.path(folder_data, "rawBase.csv")
  if (!file.exists(rawBase_file)) {
    rawBase_file = file.path(folder_data, "rawBase_index.csv")
  }
  if (file.exists(rawBase_file)) {
    # load data
    df_legacy <- read.csv(rawBase_file)
    # > names(df)
    # [1] "ID"       "size"     "missing"  "filename"
    n = nrow(df_legacy)
    df <- data.frame(
      ID = df_legacy$ID,
      ID2 = rep(NA_character_, n),
      file = df_legacy$filename,
      sha256 = rep(NA_character_, n),
      found = !df_legacy$missing,
      filesize = df_legacy$size,
      stringsAsFactors = FALSE
    )

    # save
    target_file = paste0(".rawdata2_" ,"legacy" ,".csv")
    fname =file.path(root_dir, target_file)
    message("Writing legacy file: ", fname)
    write.csv(df, row.names = FALSE,file = fname)
  }
}

