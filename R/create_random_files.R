#' Creates N random files for testing
#'
#' @param test_dir Directory in which to create the test data directory.
#' @param N Number of files to create.
#' @param data_folder Name of the directory that will contain the files.
#' @return The path to the directory containing the generated files.
#' @noRd
create_N_random_files <- function(test_dir, N=10, data_folder = "raw_source") {
  # Create the directory containing the random RAW files.
  raw_source_dir <- file.path(test_dir, data_folder)
  dir.create(
    raw_source_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  files <- file.path(
    raw_source_dir,
    sprintf("random_%03d.txt", seq_len(N))
  )

  for (file in files) {
    random_text <- paste(
      sample(c(letters, LETTERS, 0:9), size = 122, replace = TRUE),
      collapse = ""
    )

    writeLines(random_text, file)
  }

  return(raw_source_dir)
}


