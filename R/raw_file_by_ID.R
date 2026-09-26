#' Retrieve a RAW filename by ID
#' @param ID could be either numeric ID or base64 ID2
#' @export
raw_file_by_id <- function(ID) {
  match_index <- raw_idxByID(ID)

  raw_files <- raw_files_read()
  filename <- raw_files$file[match_index]
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
    warning("File not found in any registered RAW path: ", filename)
    return(NA_character_)
  }

  matches <- candidate_paths[found]

  if (length(matches) > 1L) {
    warning(
      "File found in multiple RAW paths; returning the first match: ",
      filename
    )
  }

  normalizePath(matches[1L], winslash = "/", mustWork = FALSE)
}

