test_that("raw_find can match two literal filename fragments",  suppressMessages({
  test_dir <- tempfile("raw-find-")
  raw_init(make_paths_unsearchable = TRUE)
  source_dir <- file.path(test_dir, "source")
  dir.create(source_dir, recursive = TRUE)

  filenames <- c(
    "202609.Topography.txt",
    "202609XTopography.txt",
    "Topography_202609.txt"
  )
  for (filename in filenames) {
    writeLines(filename, file.path(source_dir, filename))
  }
  raw_path_append(source_dir)
  catalogue <- raw_update()

  matching_id <- catalogue$ID2[catalogue$file == filenames[1]]
  expect_equal(
    raw_find("202609.", "Topography.txt"),
    matching_id
  )
  expect_equal(
    raw_find("Topography.txt", "202609."),
    matching_id
  )

  unlink(test_dir, recursive = TRUE, force = TRUE)

}))
