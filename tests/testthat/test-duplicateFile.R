set.seed(1234)

create_N_random_files <- function(test_dir, N=5, data_folder = "raw_source") {
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
      sample(c(letters, LETTERS, 0:9), size = 100, replace = TRUE),
      collapse = ""
    )

    writeLines(random_text, file)
  }

  return(raw_source_dir)
}


test_that("duplicate and missing files", {
  old_wd <- getwd()
  test_dir <- tempfile("raw-")
  dir.create(test_dir, recursive = TRUE)
  on.exit({
    setwd(old_wd)
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })
  setwd(test_dir)

  # Create a minimal DataLad-like dataset marker.
  #dir.create(".rawdata2")
  p <- raw_init()
  raw_source_dir <- create_N_random_files(test_dir, 5)

  # Register the RAW directory for recursive searching.
  raw_path_append(
    path = test_dir,
    searchable = TRUE
  )

  # Generate RAW_files.csv.
  result <- raw_update()
  ## duplicate file
  df <- raw_files_read()
  dup_dir = file.path(test_dir,"duplicate")
  dir.create(dup_dir, recursive = TRUE)
  f <- raw_file_by_id(df$ID[1])
  ID2 <- df$ID2[1]
  f_new = file.path(dup_dir, basename(df$file[1]))
  file.copy(f, f_new)
  raw_update()
  filename <- raw_file_by_id(ID2)
  expect_equal(length(filename),1L)
  expect_true(file.exists(filename[1]))
})
