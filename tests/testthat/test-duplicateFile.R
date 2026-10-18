test_that("duplicate and missing files", suppressMessages({
  test_dir <- tempfile("raw-")
  dir.create(test_dir, recursive = TRUE)


  # Create a minimal DataLad-like dataset marker.
  #dir.create(".rawdata2")
  p <- raw_init(make_paths_unsearchable = TRUE)
  N = 5L
  raw_source_dir <- create_N_random_files(test_dir, N)

  # Register the RAW directory for recursive searching.
  raw_path_append(
    path = test_dir,
    searchable = TRUE
  )

  # Generate RAW_files.csv.
  result <- raw_update()
  df <- raw_list()
  expect_equal(nrow(df), 5L)

  ## duplicate file
  dup_dir = file.path(test_dir,"duplicate")
  dir.create(dup_dir, recursive = TRUE)
  f <- raw_file_by_id(df$ID[1])
  ID2 <- df$ID2[1]
  f_new = file.path(dup_dir, basename(df$filename[1]))
  file.copy(f, f_new)
  raw_update()
  filename <- raw_file_by_id(ID2)
  expect_equal(length(filename),1L)
  expect_true(file.exists(filename[1]))

  # check raw_list()
  raw_id_by_file("RaNdOm", ignore.case = TRUE) -> m
  raw_list(m) -> f
  expect_equal(nrow(f), N)

  raw_find("random") -> m
  raw_list(m) -> f
  expect_equal(nrow(f), N)

  raw_find(basename(filename[1])) -> m
  raw_list(m) -> f
  expect_equal(nrow(f), 1L)

  unlink(test_dir, recursive = TRUE, force = TRUE)
}))
