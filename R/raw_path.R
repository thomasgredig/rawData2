#' Data file with RAW paths
#' @return The path to the `RAW_paths.csv` file. The `.rawdata2` directory is
#'   created when necessary.
#' @importFrom utils read.csv
#' @export
raw_paths_file <- function() {
  root_dir <- find_raw_root()

  if (is.na(root_dir)) {
    stop("No root directory for .rawdata2 is found; create a GIT or make a folder `.rawdata2` in the root directory.")
  }

  rawdata_dir <- file.path(root_dir, ".rawdata2")

  if (!dir.exists(rawdata_dir)) {
    dir.create(rawdata_dir, recursive = TRUE)
  }

  file.path(rawdata_dir, "RAW_paths.csv")
}


#' Reads the RAW paths
#' @return A data frame of registered RAW paths and their metadata, including
#'   the logical `remote` column. If the catalogue does not exist, an empty
#'   data frame is returned.
#' @export
raw_paths_read <- function() {
  file <- raw_paths_file()

  if (!file.exists(file)) {
    return(data.frame(
      path = character(),
      searchable = logical(),
      remote = logical(),
      stringsAsFactors = FALSE
    ))
  }

  paths_df <- read.csv(
    file,
    stringsAsFactors = FALSE
  )

  # Treat paths from older catalogues as remote by default.
  if (!"remote" %in% names(paths_df)) {
    paths_df$remote <- TRUE
  }

  raw_paths_reduce(paths_df)
}

#' Appends a RAW path
#' @param path A directory path to register.
#' @param searchable Logical; should files beneath this path be searched by
#'   catalogue updates?
#' @param remote Logical; whether the registered path is remote. Defaults to
#'   `TRUE`.
#' @return Invisibly, the complete data frame of registered RAW paths after
#'   duplicates and redundant child paths have been removed.
#' @export
raw_path_append <- function(path, searchable = TRUE, remote = TRUE) {
  if (length(path) != 1 || !is.character(path)) {
    stop("'path' must be a single character string.")
  }

  if (length(searchable) != 1 || !is.logical(searchable)) {
    stop("'searchable' must be a single TRUE or FALSE value.")
  }

  if (length(remote) != 1 || !is.logical(remote) || is.na(remote)) {
    stop("'remote' must be a single TRUE or FALSE value.")
  }

  file <- raw_paths_file()
  existing_paths <- raw_paths_read()

  # Ensure older path files have the current columns.
  if (nrow(existing_paths) == 0) {
    existing_paths <- data.frame(
      ID = integer(),
      path = character(),
      searchable = logical(),
      remote = logical(),
      user = character(),
      date = character(),
      stringsAsFactors = FALSE
    )
  } else {
    if (!"ID" %in% names(existing_paths)) {
      existing_paths$ID <- seq_len(nrow(existing_paths))
    }

    if (!"user" %in% names(existing_paths)) {
      existing_paths$user <- NA_character_
    }

    if (!"date" %in% names(existing_paths)) {
      existing_paths$date <- NA_character_
    }

    if (!"remote" %in% names(existing_paths)) {
      existing_paths$remote <- TRUE
    }

    existing_paths <- existing_paths[, c(
      "ID", "path", "searchable", "remote", "user", "date"
    )]
  }

  # Assign the next available ID.
  if (nrow(existing_paths) == 0) {
    new_ID <- 1L
  } else {
    new_ID <- max(existing_paths$ID, na.rm = TRUE) + 1L
  }

  new_row <- data.frame(
    ID = new_ID,
    path = path,
    searchable = searchable,
    remote = remote,
    user = git_username(),
    date = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )

  all_paths <- rbind(existing_paths, new_row)

  # Remove duplicate paths and paths covered by parent directories.
  all_paths <- raw_paths_reduce(all_paths)

  # Keep the requested column order.
  all_paths <- all_paths[, c(
    "ID", "path", "searchable", "remote", "user", "date"
  )]

  write.csv(
    all_paths,
    file = file,
    row.names = FALSE,
    quote = TRUE
  )

  invisible(all_paths)
}



#' Remove duplicate and redundant RAW paths
#' @param paths_df A data frame containing a `path` column and optional path
#'   metadata columns.
#' @return A data frame with duplicate paths and paths nested under another
#'   registered path removed.
#' @noRd
raw_paths_reduce <- function(paths_df) {
  if (nrow(paths_df) == 0) {
    return(paths_df)
  }

  # Normalize paths so trailing slashes and relative paths do not create
  # separate entries.
  paths_df$path <- normalizePath(
    paths_df$path,
    winslash = .Platform$file.sep,
    mustWork = FALSE
  )

  # Remove exact duplicate paths, keeping the first record.
  paths_df <- paths_df[!duplicated(paths_df$path), , drop = FALSE]

  path_contains <- function(parent, child) {
    if (parent == child) {
      return(TRUE)
    }

    separator <- .Platform$file.sep
    prefix <- if (endsWith(parent, separator)) {
      parent
    } else {
      paste0(parent, separator)
    }

    startsWith(child, prefix)
  }

  # A path is redundant if another path is its parent.
  keep <- vapply(seq_len(nrow(paths_df)), function(i) {
    !any(vapply(seq_len(nrow(paths_df)), function(j) {
      i != j && path_contains(paths_df$path[j], paths_df$path[i])
    }, logical(1)))
  }, logical(1))

  paths_df[keep, , drop = FALSE]
}


#' Removes path that do not exist; use with care
#' @return Invisibly, the character vector of paths read before nonexistent
#'   paths were removed from the catalogue.
#' @export
raw_path_trim <- function() {
  d <- raw_paths_read()

  all_paths = d$path
  if (!is.character(all_paths)) {
    stop("raw_paths_read() must return a character vector of paths.")
  }

  found <- dir.exists(all_paths)
  removed_paths <- all_paths[!found]


  write.csv(
    d[found,],
    file =  raw_paths_file(),
    row.names = FALSE,
    quote = TRUE
  )

  if (length(removed_paths) > 0L) {
    message("Removed ", length(removed_paths), " missing path(s).")
  }

  invisible(all_paths)
}
