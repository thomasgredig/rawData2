test_that("find root directory", {
  test_dir <- tempfile("raw-test1-")

  # start code here
  raw_init(make_paths_unsearchable = TRUE)
  p_root <- find_raw_root()
  expect_true(dir.exists(p_root))

  # add not-valid path
  raw_path_append("dir-notexist")
  p <- raw_paths_read()
  expect_true("remote" %in% names(p))
  expect_true(p$remote)
  expect_warning({result=raw_update()})


  request_log <- file.path(p_root, RAWDATA_FOLDER, FILE_LOG)
  expect_true(file.exists(request_log))
  log_lines <- readLines(request_log)

  expect_warning({f <- raw_file_by_id(8778, log=TRUE)})
  expect_true(is.na(f))
  log_lines_new <- readLines(request_log)

  expect_equal(length(log_lines_new) - length(log_lines), 1L)
  last_log_line = length(log_lines)
  expect_match(
    log_lines[last_log_line],
    paste0("^8778\t\t[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:")
  )
  request_date <- substr(strsplit(log_lines[last_log_line], "\t", fixed = TRUE)[[1]][3], 1L, 10L)
  expect_equal(raw_id2_by_date(request_date), list())
  expect_warning({f <- raw_file_record_by_id(22288)})
  expect_true(is.null(f))

  # remove path that is not-valid
  expect_equal(nrow(p),1L)
  raw_path_trim()
  p <- raw_paths_read()
  expect_equal(nrow(p),0L)

  unlink(test_dir, recursive = TRUE, force = TRUE)
})

test_that("raw_path_append records local paths", {
  test_dir <- tempfile("raw-path-")

  raw_init(make_paths_unsearchable = TRUE)
  raw_path_append("local-path", remote = FALSE)
  paths <- raw_paths_read()

  expect_equal(nrow(paths),1)
  expect_false(paths$remote)
  expect_equal(
    names(paths),
    c("ID", "path", "searchable", "remote", "user", "date")
  )

  unlink(test_dir, recursive = TRUE, force = TRUE)
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
