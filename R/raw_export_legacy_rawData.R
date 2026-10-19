#' Exports legacy rawData if available
#' @description
#' This is called from raw_init() and checks whether there is
#' a legacy file system.
#' @param root_dir Directory containing the legacy `data` directory.
#' @return Invisibly, `NULL`; a compatible legacy register is written when
#'   one is found.
#' @noRd
raw_export_legacy_rawData <- function(root_dir) {
  folder_data = file.path(root_dir, "data")
  rawBase_file = file.path(folder_data, "rawBase.csv")
  if (!file.exists(rawBase_file)) {
    rawBase_file = file.path(folder_data, "rawBase_index.csv")
  }
  if (file.exists(rawBase_file)) {
    # load data
    df_legacy <- read.csv(rawBase_file)
    crc_column <- names(df_legacy)[
      tolower(names(df_legacy)) == "crc"
    ][1]
    n = nrow(df_legacy)
    df <- data.frame(
      ID = df_legacy$ID,
      ID2 = rep(NA_character_, n),
      file = df_legacy$filename,
      sha256 = rep(NA_character_, n),
      found = !df_legacy$missing,
      filesize = df_legacy$size,
      crc = if (!is.na(crc_column)) {
        as.numeric(df_legacy[[crc_column]])
      } else {
        rep(NA_real_, n)
      },
      stringsAsFactors = FALSE
    )

    # save
    target_file = paste0(".rawdata2_" ,"legacy" ,".csv")
    fname =file.path(root_dir, target_file)
    message("Writing legacy file: ", fname)
    write.csv(df, row.names = FALSE,file = fname)
  }
}
