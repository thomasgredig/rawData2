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
#' @importFrom dplyr distinct select mutate filter n row_number arrange group_by ungroup if_else left_join
#' @importFrom utils write.csv setTxtProgressBar txtProgressBar
#' @export
raw_update <- function(paths = c(), fastScan = TRUE, verify=FALSE) {
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

  if (verify) {
    # find same ID, filename, and file size
    old <- old |>
      mutate(filename = basename(file)) |>
      dplyr::group_by(ID, filesize, filename) |>
      dplyr::filter(!(dplyr::n() > 1 & is.na(ID2))) |>
      dplyr::ungroup() |>
      select(!filename)
    ##
    old <- old |>
      mutate(filename = basename(file)) |>
      dplyr::group_by(ID, filename) |>
      dplyr::filter(!(dplyr::n() > 1 & is.na(ID2))) |>
      dplyr::ungroup() |>
      select(!filename)
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


  # merge duplicate files that have the same name + size, but no sha256
  lookup <- old |>
    filter(!is.na(sha256)) |>
    mutate(file = basename(file)) |>
    select(file, filesize,
           sha256_new = sha256,
           ID2_new = ID2) |>
    distinct(file, filesize, .keep_all = TRUE)

  old <- old |>
    mutate(file = basename(file)) |>
    left_join(lookup, by = c("file", "filesize")) |>
    mutate(
      needs_update = !is.na(found) & !found & is.na(sha256),
      sha256 = if_else(needs_update, sha256_new, sha256),
      ID2    = if_else(needs_update, ID2_new, ID2)
    ) |>
    select(-sha256_new, -ID2_new, -needs_update)

  old <- old |>
    arrange(ID) |>
    group_by(sha256) |>
    filter(is.na(sha256) | row_number() == 1) |>
    ungroup()

  # Search only paths marked as searchable.
  search_paths <- paths$path[paths$searchable %in% TRUE]

  files <- character()

  if (length(search_paths) > 0) {
    extensions <- raw_extensions_read()
    extension_pattern <- if (length(extensions) > 0L) {
      paste0("\\.(", paste(extensions, collapse = "|"), "|[0-9]+)$")
    } else {
      "^$"
    }

    files <- unlist(
      lapply(search_paths, function(path) {
        if (!dir.exists(path)) {
          warning("Directory does not exist: ", path)
          return(character())
        }

        list.files(
          path = path,
          pattern = extension_pattern,
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
    # relative_files <- strip_directories(files, paths$path)
    relative_files <- files
    file_sizes <- as.numeric(file.size(files))
    previous_index <- match(relative_files, old$file)

    can_reuse <- fastScan &
      !is.na(previous_index) &
      !is.na(old$filesize[previous_index]) &
      old$filesize[previous_index] == file_sizes

    pb <- txtProgressBar(min = 0, max = length(files), style = 3)

    checksums <- vapply(seq_along(files), function(i) {
      if (can_reuse[i] & !is.na(old$sha256[previous_index[i]])) {
        old$sha256[previous_index[i]]
      } else {
        setTxtProgressBar(pb, i)

        digest(
          files[[i]],
          algo = "sha256",
          file = TRUE
        )
      }
    }, character(1))

    close(pb)

    current <- data.frame(
      file = strip_directories(files, paths$path),
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

  # merge IDs of files that have the same file name, file size, and ID
  na_rows <- is.na(old$sha256)

  keep <- rep(TRUE, nrow(old))
  keep[na_rows] <- !duplicated(
    old[na_rows, c("ID", "filesize", "file")]
  )

  old <- old[keep, ]

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
    FILE_FILES
  )

  write.csv(
    df_files,
    raw_files_file,
    row.names = FALSE,
    quote = TRUE
  )

  raw_files_file
}



#' helper function to remove paths
#' @noRd
strip_directories <- function(f, p) {
  f <- unlist(f, use.names = FALSE)
  p <- unlist(p, use.names = FALSE)

  clean <- function(x) {
    x <- gsub("\\\\", "/", x)
    sub("/+$", "", x)
  }

  f <- clean(f)
  p <- clean(p)

  # Try longest roots first
  p <- p[order(nchar(p), decreasing = TRUE)]

  vapply(f, function(file) {
    matches <- vapply(
      p,
      function(root) {
        file == root || startsWith(file, paste0(root, "/"))
      },
      logical(1)
    )

    if (any(matches)) {
      root <- p[which(matches)[1]]
      sub("^/", "", substring(file, nchar(root) + 1))
    } else {
      file
    }
  }, character(1))
}
