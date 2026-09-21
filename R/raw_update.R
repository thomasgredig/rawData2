#' Update the RAW file catalogue with SHA-256 checksums
#'
#' @importFrom digest digest
#' @importFrom utils write.csv
#' @export
raw_update <- function() {
  paths <- raw_paths_read()

  # Read the previous catalogue.
  old <- raw_files_read()

  if (nrow(old) == 0) {
    old <- data.frame(
      ID = integer(),
      file = character(),
      sha256 = character(),
      found = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    # Support older RAW_files.csv files without a `found` column.
    if (!"ID" %in% names(old)) {
      old$ID <- seq_len(nrow(old))
    }

    if (!"found" %in% names(old)) {
      old$found <- FALSE
    }

    old <- old[, c("ID", "file", "sha256", "found")]
    old$ID <- as.integer(old$ID)
    old$file <- as.character(old$file)
    old$sha256 <- as.character(old$sha256)
    old$found <- as.logical(old$found)

    # Avoid duplicate catalogue entries for the same content.
    old <- old[!duplicated(old$sha256), , drop = FALSE]

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
        next_id <- 1L
      } else {
        next_id <- max(old$ID, na.rm = TRUE) + 1L
      }

      new_files <- current[!matched, , drop = FALSE]

      new_rows <- data.frame(
        ID = seq.int(
          from = next_id,
          length.out = nrow(new_files)
        ),
        file = new_files$file,
        sha256 = new_files$sha256,
        found = TRUE,
        stringsAsFactors = FALSE
      )

      old <- rbind(old, new_rows)
    }
  }

  raw_files_file <- file.path(
    dirname(raw_paths_file()),
    "RAW_files.csv"
  )

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
