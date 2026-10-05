#' Retrieve a RAW filename by ID
#' @param ID A numeric catalogue ID or a base64-style `ID2` value.
#' @return The normalized path to the matching file, or `NA_character_` when
#'   the file cannot be found.
#' @details Each request is appended to `RAW_log.txt` in the
#'   `.rawdata2` directory with its `ID`, `ID2`, timestamp, and system user.
#' @export
raw_file_by_id <- function(ID, log=FALSE) {
  match_index <- raw_idxByID(ID)

  raw_files <- raw_files_read()
  if (log) raw_file_by_id_append_log(ID, match_index, raw_files)

  filename <- raw_files$file[match_index]
  db_size <- raw_files$filesize[match_index]
  db_sha256 <- raw_files$sha256[match_index]

  if (file.exists(filename)) {
    f <- normalizePath(filename, winslash = "/", mustWork = FALSE)
    return(f)
  }

  raw_paths <- raw_paths_read()

  if (nrow(raw_paths) == 0L || !"path" %in% names(raw_paths)) {
    warning("No RAW search paths are registered.")
    return(NA_character_)
  }

  # Restrict the search to paths marked searchable, if that column exists.
  if ("searchable" %in% names(raw_paths)) {
    raw_paths <- raw_paths[
      !is.na(raw_paths$searchable) & raw_paths$searchable,
      ,
      drop = FALSE
    ]
  }

  # Only search directories that currently exist.
  raw_paths <- raw_paths[
    !is.na(raw_paths$path) & dir.exists(raw_paths$path),
    ,
    drop = FALSE
  ]

  if (nrow(raw_paths) == 0L) {
    warning("None of the registered RAW paths exist.")
    return(NA_character_)
  }

  candidate_paths <- file.path(raw_paths$path, filename)
  found <- file.exists(candidate_paths)

  if (!any(found)) {
    warning("File not found in any registered RAW path: ", filename,"\n",
            "Consider adding paths with raw_path_append() and/or run raw_update() again.")
    return(NA_character_)
  }

  matches <- candidate_paths[found]
  path_match = 1L # default
  if (length(matches) > 1L) {
    # need to check sha256 as two files could have the same name, same size, but different sha256
    actual_size = file.size(candidate_paths[found])
    which(actual_size == db_size) -> path_match
    if (length(path_match) > 1L) {
      # still found multiple files with same name and size, need to read file and check sha256
      actual_sha256 <- vapply(
        candidate_paths,
        digest::digest,
        character(1),
        algo = "sha256",
        file = TRUE
      )
      idx <- which(actual_sha256 == db_sha256)
      path_match <- path_match[idx]
    }
  }

  normalizePath(matches[path_match], winslash = "/", mustWork = FALSE)
}


#' Append a raw_file_by_id request to the audit log
#' @noRd
raw_file_by_id_append_log <- function(ID, match_index, raw_files) {
  requested_id <- NA_character_
  requested_id2 <- NA_character_

  if (!is.na(match_index)) {
    requested_id <- as.character(raw_files$ID[match_index])
    requested_id2 <- as.character(raw_files$ID2[match_index])
  } else if (is.numeric(ID) ||
             (is.character(ID) && grepl("^[0-9]+$", ID))) {
    requested_id <- as.character(ID)
  } else {
    requested_id2 <- as.character(ID)
  }

  log_file <- file.path(dirname(raw_paths_file()), "RAW_log.txt")
  if (!file.exists(log_file)) {
    cat("ID\tID2\ttimestamp\tuser\n", file = log_file)
  }

  cat(
    paste(
      ifelse(is.na(requested_id), "", requested_id),
      ifelse(is.na(requested_id2), "", requested_id2),
      format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3%z"),
      ifelse(is.na(Sys.info()[["user"]]), "", Sys.info()[["user"]]),
      sep = "\t"
    ),
    "\n",
    file = log_file,
    append = TRUE
  )

  invisible(log_file)
}
