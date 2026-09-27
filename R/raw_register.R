#' exports the RAWdata register to the target_path
#' @param target_path A directory in which to write the exported register.
#' @return The path to the exported register, or `NA_character_` when
#'   `target_path` does not exist.
#' @export
raw_export_register <- function(target_path) {
  if (!dir.exists(target_path)) {
    warning("Target directory does not exist: ", target_path)
    return(NA_character_)
  }
  username <- Sys.info()[["user"]]
  target_file = paste0(".rawdata2_" ,username ,".csv")

  df <- raw_files_read()
  fname = normalizePath( file.path(target_path, target_file) )
  write.csv(df, row.names = FALSE,
            file = fname)

  fname
}

#' imports the RAWdata register
#' @param recursive Logical; should registered paths be searched recursively?
#' @return `NULL`. The catalogue is updated as compatible register files are
#'   imported.
#' @export
raw_import_register <- function(recursive=FALSE) {
  df <- raw_paths_read()
  path_list = c(".", df$path)
  for(path in path_list) {
    if (dir.exists(path)) {
      # local directory is only searched, but not recursively
      recur <- recursive && !identical(path, ".")
      files <- dir(path, pattern=".rawdata2_",
                   full.names = TRUE,
                   all.files = TRUE, # needed to find dot files
                   recursive=recur)
      if (length(files)>0) {
        for(file in files) {
          message("Importing: ", basename(files))
          old_files <- raw_files_read()
          new_files <- read.csv(file)
          if (check_rawdata_format(new_files)) {
            all_files <- raw_merge(old_files, new_files)
            rawFilesSave(all_files)
          } else {
            message("Incompatible file: ", file)
          }

        }
      }
    }
  }
}

#' @noRd
#' @param df1 The first RAW file catalogue data frame.
#' @param df2 The second RAW file catalogue data frame.
#' @return A merged catalogue with duplicate `ID2` entries removed.
raw_merge <- function(df1, df2) {
  df <- rbind(df1,df2)
  df <- df[!duplicated(df$ID2),]

  # this should never occur, but collisions are possible
  if (length(which(duplicated(df$ID2)==TRUE))>0) {
    stop("ID2 has collision, cannot merge RAWdata registers.")
  }

  # however, the ID can collide more often
  if (length(which(duplicated(df$ID)==TRUE))>0) {
    warning("ID collision(s).")
  }

  df
}

#' @noRd
#' @param df A data frame to validate.
#' @return Invisibly, `TRUE` when all required catalogue columns are present.
check_rawdata_format <- function(df) {
  required_cols <- c("ID", "ID2", "file", "sha256", "found")

  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(TRUE)
}
