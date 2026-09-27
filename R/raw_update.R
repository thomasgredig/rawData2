#' Update the RAW file catalogue with SHA-256 checksums and file sizes
#' @param paths A character vector of paths to register before updating the
#'   catalogue.
#' @param fastScan Logical; if `TRUE`, reuse a registered SHA-256 checksum
#'   when the file has the same registered path and filesize. Files without a
#'   matching path and filesize are still hashed.
#' @return Invisibly, the updated RAW file catalogue as a data frame. The
#'   `filesize` column contains the size of each found file in bytes and is
#'   `NA` for files that are not currently found.
#' @importFrom digest digest
#' @importFrom utils write.csv
#' @export
raw_update <- function(paths = c(), fastScan = TRUE) {
  if (length(fastScan) != 1L || !is.logical(fastScan) || is.na(fastScan)) {
    stop("'fastScan' must be a single TRUE or FALSE value.")
  }

  # Update paths
  for(path in paths) {
    raw_path_append(path)
  }
  # Read all paths to be searched.
  paths <- raw_paths_read()
  # Read the previous catalogue.
  old <- raw_files_read()

  # helper function to remove paths
  strip_directories <- function(f, p) {
    f <- unlist(f, use.names = FALSE)
    p <- unlist(p, use.names = FALSE)

    clean <- function(x) {
      x <- gsub("\\\\", "/", x)   # normalize Windows paths
      sub("/+$", "", x)           # remove trailing slash
    }

    f <- clean(f)
    p <- clean(p)

    # Match the longest directory prefixes first
    p <- p[order(nchar(p), decreasing = TRUE)]

    vapply(f, function(file) {
      matches <- startsWith(file, p) &
        (nchar(file) == nchar(p) |
           substr(file, nchar(p) + 1, nchar(p) + 1) == "/")

      if (any(matches)) {
        sub("^/", "", substring(file, nchar(p[which(matches)[1]]) + 1))
      } else {
        file
      }
    }, character(1))
  }

  # create a DF if nothing exists
  if (nrow(old) == 0) {
    old <- data.frame(
      ID = integer(),
      ID2 = character(),
      file = character(),
      sha256 = character(),
      filesize = numeric(),
      found = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    if (!"filesize" %in% names(old)) {
      old$filesize <- NA_real_
    }

    # Assume that previous files are not found until rediscovered.
    old$found <- FALSE
  }

  # Search only paths marked as searchable.
  search_paths <- paths$path[paths$searchable %in% TRUE]

  files <- character()

  if (length(search_paths) > 0) {
    files <- unlist(
      lapply(search_paths, function(path) {
        if (!dir.exists(path)) {
          warning("Directory does not exist: ", path)
          return(character())
        }

        list.files(
          path = path,
          pattern = "\\.(tiff|jpg|jpeg|png|ibw|ras|rasx|txt|csv|bin|xlsx|docx|asc|nid|[0-9]+)$",
          recursive = TRUE,
          full.names = TRUE,
          include.dirs = FALSE,
          all.files = TRUE,
          ignore.case = TRUE
        )
      }),
      use.names = FALSE
    )
  }

  # Keep only regular files.
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- unique(normalizePath(files, mustWork = TRUE))

  if (length(files) > 0) {
    relative_files <- strip_directories(files, paths$path)
    file_sizes <- as.numeric(file.size(files))
    previous_index <- match(relative_files, old$file)

    can_reuse <- fastScan &
      !is.na(previous_index) &
      !is.na(old$filesize[previous_index]) &
      old$filesize[previous_index] == file_sizes

    checksums <- vapply(seq_along(files), function(i) {
      if (can_reuse[i]) {
        old$sha256[previous_index[i]]
      } else {
        digest(
          files[[i]],
          algo = "sha256",
          file = TRUE
        )
      }
    }, character(1))

    current <- data.frame(
      file = relative_files,
      filesize = file_sizes,
      sha256 = checksums,
      stringsAsFactors = FALSE
    )

    # A content hash identifies one catalogue entry.
    current <- current[
      !duplicated(current$sha256),
      ,
      drop = FALSE
    ]

    # Find which current files match previous content.
    old_index <- match(current$sha256, old$sha256)

    matched <- !is.na(old_index)

    # Existing content: retain ID, update path, mark as found.
    if (any(matched)) {
      old$found[old_index[matched]] <- TRUE
      old$file[old_index[matched]] <- current$file[matched]
      old$filesize[old_index[matched]] <- current$filesize[matched]
    }


    # New content: assign new IDs.
    if (any(!matched)) {
      if (nrow(old) == 0 || all(is.na(old$ID))) {
        next_id <- 7L
      } else {
        next_id <- max(old$ID, na.rm = TRUE) + 1L
      }

      new_files <- current[!matched, , drop = FALSE]
      IDs <- seq.int(
        from = next_id,
        length.out = nrow(new_files)
      )

      new_rows <- data.frame(
        ID = IDs,
        ID2 = base64(strtoi(substr(new_files$sha256, 1, 7),16L)),
        file = new_files$file,
        sha256 = new_files$sha256,
        filesize = new_files$filesize,
        found = TRUE,
        stringsAsFactors = FALSE
      )

      old <- rbind(old, new_rows)
    }
  }

  raw_files_file <- rawFilesSave(old)

  message("Updated RAW file catalogue: ", raw_files_file)
  message("Number of catalogue entries: ", nrow(old))
  message("Number of files currently found: ", sum(old$found))

  invisible(old)
}

#' @noRd
#' @param df_files A data frame containing RAW file catalogue records.
#' @return The path to the saved `RAW_files.csv` catalogue.
rawFilesSave <- function(df_files) {
  raw_files_file <- file.path(
    dirname(raw_paths_file()),
    "RAW_files.csv"
  )

  write.csv(
    df_files,
    raw_files_file,
    row.names = FALSE,
    quote = TRUE
  )

  raw_files_file
}
