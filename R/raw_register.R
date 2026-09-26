#' exports the RAWdata register to the target_path
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
#' @export
raw_import_register <- function(recursive=FALSE) {
  df <- raw_paths_read()
  for(path in df$path) {
    if (dir.exists(path)) {
      files <- dir(path, pattern=".rawdata2_",
                   full.names = TRUE, all.files = TRUE,
                   recursive=recursive)
      if (length(files)>0) {
        for(file in files) {
          message("Importing: ", basename(files))
          old_files <- raw_files_read()
          new_files <- read.csv(file)
          all_files <- raw_merge(old_files, new_files)
          rawFilesSave(all_files)
        }
      }
    }
  }
}

#' @noRd
raw_merge <- function(df1, df2) {
  df1 = d1
  df2 = d2
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
