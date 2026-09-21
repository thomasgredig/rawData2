#' Data file with RAW paths
#' @export
raw_paths_file <- function() {
  root_dir <- find_raw_root()

  if (is.na(root_dir)) {
    stop("No DataLad dataset root found.")
  }

  rawdata_dir <- file.path(root_dir, ".rawdata2")

  if (!dir.exists(rawdata_dir)) {
    dir.create(rawdata_dir, recursive = TRUE)
  }

  file.path(rawdata_dir, "RAW_paths.csv")
}


#' Reads the RAW paths
#' @export
raw_paths_read <- function() {
  file <- raw_paths_file()

  if (!file.exists(file)) {
    return(data.frame(
      path = character(),
      searchable = logical(),
      stringsAsFactors = FALSE
    ))
  }

  paths_df <- read.csv(
    file,
    stringsAsFactors = FALSE,
    colClasses = c("character", "logical")
  )

  raw_paths_reduce(paths_df)
}

#' Appends a RAW path
#' @export
raw_path_append <- function(path, searchable = TRUE) {
  if (length(path) != 1 || !is.character(path)) {
    stop("'path' must be a single character string.")
  }

  if (length(searchable) != 1 || !is.logical(searchable)) {
    stop("'searchable' must be a single TRUE or FALSE value.")
  }

  new_row <- data.frame(
    path = path,
    searchable = searchable,
    stringsAsFactors = FALSE
  )

  file <- raw_paths_file()

  existing_paths <- raw_paths_read()
  all_paths <- rbind(existing_paths, new_row)

  # Remove duplicates and paths covered by parent directories.
  all_paths <- raw_paths_reduce(all_paths)

  # Rewrite the CSV because adding a parent may remove existing children.
  write.csv(
    all_paths,
    file = file,
    row.names = FALSE,
    quote = TRUE
  )

  invisible(all_paths)
}


#' Remove duplicate and redundant RAW paths
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
