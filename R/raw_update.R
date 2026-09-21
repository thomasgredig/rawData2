#' Update the RAW file catalogue with SHA-256 checksums
#' @export
raw_update <- function() {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required. Install it with install.packages('digest').")
  }

  paths <- raw_paths_read()

  # Search only paths marked as searchable.
  search_paths <- paths$path[paths$searchable %in% TRUE]

  if (length(search_paths) == 0) {
    codes <- data.frame(
      file = character(),
      sha256 = character(),
      stringsAsFactors = FALSE
    )
  } else {
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

    # Keep only regular files and remove duplicates.
    files <- files[file.exists(files) & !dir.exists(files)]
    files <- unique(normalizePath(files, mustWork = TRUE))

    codes <- data.frame(
      file = files,
      sha256 = vapply(
        files,
        function(f) {
          digest::digest(
            f,
            algo = "sha256",
            file = TRUE
          )
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  raw_files_file <- file.path(
    dirname(raw_paths_file()),
    "RAW_files.csv"
  )

  write.csv(
    codes,
    raw_files_file,
    row.names = FALSE,
    quote = TRUE
  )

  message("Updated RAW file catalogue: ", raw_files_file)

  invisible(codes)
}
