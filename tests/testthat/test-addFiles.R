test_that("raw_update handles renamed files and updates", suppressMessages({
  N <- 3L  # Change this to any value from 1 through 40
  test_dir <- tempfile("raw-test-")
  dir.create(test_dir, recursive = TRUE)

  # create and register N files
  p <- raw_init(make_paths_unsearchable = TRUE)
  raw_source_dir <- create_N_random_files(test_dir, N)
  raw_path_append(path = raw_source_dir)
  raw_update() # Generate RAW_files.csv.

  # ===================
  # test raw_update works
  # ===================
  result <- raw_list()
  expect_equal(nrow(result), N)
  expect_equal(length(unique(result$ID2)), N)
  expect_true(all(result$filesize > 0))
  ## expect lowest ID to be at least 7
  expect_true(min(result$ID)>=7L)

  # Add more files in a different directory
  raw_source_dir2 <- create_N_random_files(test_dir, N, "newdir")
  raw_path_append(raw_source_dir2)
  raw_update()

  result <- raw_list()
  expect_equal(nrow(result), N+N)
  expect_equal(max(result$ID), N+N+min(result$ID)-1)

  # ===================
  # two files with the same filename but different content
  # ===================
  # two files with same length and same name: random_001.txt, load correct one
  raw_find("random_001") |> raw_list() -> rnd_files
  expect_equal(nrow(rnd_files), 2L)
  f1 <- raw_file_by_id(rnd_files$ID2[1])
  f2 <- raw_file_by_id(rnd_files$ID2[2])
  expect_equal(dirname(f1), normalizePath(raw_source_dir))
  expect_equal(dirname(f2), normalizePath(raw_source_dir2))

  # delete one file and make sure it is missing
  files <- list.files(
    raw_source_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  expect_true( length(files) >= 1L)
  expect_true( file.remove(files[1]) )
  raw_update()
  result <- raw_list()
  expect_equal(nrow(result), N+N-1)

  unlink(test_dir, recursive = TRUE, force = TRUE)
}))


test_that("raw_update handles renaming files", suppressMessages({
  N <- 5L  # Change this to any value from 1 through 40
  test_dir <- tempfile("raw-rename-")
  dir.create(test_dir, recursive = TRUE)

  # create and register N files
  p <- raw_init(make_paths_unsearchable = TRUE)
  raw_source_dir <- create_N_random_files(test_dir, N)
  raw_path_append(path = raw_source_dir)

  # ===================
  # rename one file and expect it to have the same ID
  # ===================
  files <- list.files(
    raw_source_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  expect_true( length(files) >= 1)
  filename_orig <- files[1]
  ID2 <- raw_getID(filename_orig)$ID2
  new_filename = sub("random_","new43f_", filename_orig)
  file.rename(from=filename_orig, to = new_filename)
  raw_update()

  ID_new <- raw_id_by_file("new43f")
  find_renamed <- raw_list(ID_new)
  expect_true(ID2 %in% find_renamed$ID2)

  unlink(test_dir, recursive = TRUE, force = TRUE)
}))

test_that("raw_update handles moving file to different directory", suppressMessages({
  N <- 4L  # Change this to any value from 1 through 40
  test_dir <- tempfile("raw-dir1-")
  dir.create(test_dir, recursive = TRUE)
  test_dir2 <- tempfile("raw-dir2-")
  dir.create(test_dir2, recursive = TRUE)

  # create and register N files
  p <- raw_init(make_paths_unsearchable = TRUE)
  raw_source_dir <- create_N_random_files(test_dir, N, "orig")
  raw_path_append(path = dirname(raw_source_dir))
  raw_source_dir2 <- create_N_random_files(test_dir2, N, "new")
  raw_path_append(path = dirname(raw_source_dir2))
  raw_update()

  # read paths:
  d_paths <- raw_paths_read()
  expect_equal(nrow(d_paths), 2)

  # move one file to another folder and check that the ID remains the same
  files_found <- raw_list()
  expect_equal(nrow(files_found), N+N)

  files <- list.files(
    raw_source_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  filename_orig = files[1]
  ID2 <- raw_getID(filename_orig)$ID2
  filename_new = sub(raw_source_dir, raw_source_dir2, filename_orig)
  filename_new = sub("random_", "rnd_moved_", filename_new)

  expect_true( file.rename(from = filename_orig, to=filename_new) )
  expect_warning( raw_update(verify=TRUE) )
  result <- raw_list()
  filename <- raw_file_by_id(ID2)
  expect_true( grepl(".*raw-dir2.*rnd_moved.*", filename) )
  expect_equal( nrow(result), N+N)

  unlink(test_dir, recursive = TRUE, force = TRUE)
  unlink(test_dir2, recursive = TRUE, force = TRUE)
}))
