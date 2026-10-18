test_that("find root directory", {
  # start code here
  raw_init()
  p_root <- find_raw_root()
  expect_true(dir.exists(p_root))

  # add not-valid path
  p_not = "dir-doesnotexist"
  raw_path_append(p_not, remote=FALSE)
  p <- raw_paths_read()
  expect_true("remote" %in% names(p))

  idx <- which(p$path==p_not)
  expect_equal(length(idx),1L)
  expect_true(!p$remote[idx])
})


test_that("check log file from raw_file_by_id", {
  p_root <- find_raw_root()
  raw_init(make_paths_unsearchable = TRUE)
  test_dir <- tempfile("raw-test1-")
  N = 4L
  raw_dir <- create_N_random_files(test_dir, N, "RAW")
  raw_update(raw_dir)
  m <- raw_list()
  expect_equal(nrow(m), N)

  # test raw_info

  ID2 <- m$ID2[1]
  q <- raw_info(ID2)
  expect_equal(q$filename, "random_001.txt")
  expect_equal(q$filesize, 123)
  expect_true(q$found)

  filename <- raw_file_by_id(ID2, log=TRUE) # creates LOG file
  request_log <- file.path(p_root, RAWDATA_FOLDER, FILE_LOG)
  expect_true(file.exists(request_log))
  log_lines <- readLines(request_log)

  f <- raw_file_by_id(ID2, log=TRUE)
  log_lines_new <- readLines(request_log)

  expect_equal(length(log_lines_new) - length(log_lines), 1L)
  last_log_line = length(log_lines)
  expect_match(
    log_lines[last_log_line],
    paste0("^\\d+\t.*\t[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:")
  )
})


test_that("raw_path_append records local paths", {
  raw_init(make_paths_unsearchable = TRUE)
  raw_path_append("local-path", remote = FALSE)

  paths <- raw_paths_read()
  expect_equal(nrow(paths), 1L)
  raw_path_trim()
  paths <- raw_paths_read()
  expect_equal(nrow(paths), 0L)
})

# test_that("raw_path_append migrates legacy path files", {
#   test_dir <- tempfile("raw-path-legacy-")
#   raw_init(make_paths_unsearchable = TRUE)
#   dir.create(file.path(test_dir, RAWDATA_FOLDER_LEGACY), recursive = TRUE)
#
#   write.csv(
#     data.frame(
#       ID = 1L,
#       path = "legacy-path",
#       searchable = TRUE,
#       user = "legacy-user",
#       date = "2026-01-01",
#       stringsAsFactors = FALSE
#     ),
#     file.path(RAWDATA_FOLDER_LEGACY, FILE_PATHS),  # [1] ".rawdata2/RAW_paths.csv"
#     row.names = FALSE
#   )
#
#   paths <- raw_paths_read()
#   expect_true(paths$remote)
#
#   appended <- raw_path_append("new-path", remote = FALSE)
#   expect_equal(appended$remote, c(TRUE, FALSE))
#
#   unlink(test_dir, recursive = TRUE, force = TRUE)
# })
