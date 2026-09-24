#' Update the RAW file catalogue with SHA-256 checksums
#'
#' @importFrom digest digest
#' @importFrom utils write.csv
#' @export
raw_update <- function(verbose=TRUE) {
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
      found = logical(),
      stringsAsFactors = FALSE
    )
  } else {
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
          recursive = TRUE,
          full.names = TRUE,
          include.dirs = FALSE
        )
      }),
      use.names = FALSE
    )
  }

  # Keep only regular files.
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- unique(normalizePath(files, mustWork = TRUE))

  if (length(files) > 0) {
    current <- data.frame(
      file = files,
      sha256 = vapply(
        files,
        function(f) {
          digest(
            f,
            algo = "sha256",
            file = TRUE
          )
        },
        character(1)
      ),
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
        found = TRUE,
        stringsAsFactors = FALSE
      )

      old <- rbind(old, new_rows)
      old$file <- strip_directories(old$file, paths$path)
    }
  }


  raw_files_file <- file.path(
    dirname(raw_paths_file()),
    "RAW_files.csv"
  )

  ## create

  write.csv(
    old,
    raw_files_file,
    row.names = FALSE,
    quote = TRUE
  )

  message("Updated RAW file catalogue: ", raw_files_file)
  message("Number of catalogue entries: ", nrow(old))
  message("Number of files currently found: ", sum(old$found))

  invisible(old)
}
