test_that("find root directory", {
  old_wd <- getwd()
  test_dir <- tempfile("raw-test1-")
  dir.create(test_dir, recursive = TRUE)
  on.exit({
    setwd(old_wd)
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })
  setwd(test_dir)

  # start code here
  raw_init()
  p_root <- find_raw_root()
  expect_true(dir.exists(p_root))

  # add not-valid path
  raw_path_append("abc")
  p <- raw_paths_read()
  expect_warning({result=raw_update()})
  expect_equal(nrow(result),0L)
  expect_warning({f <- raw_file_by_id(8778)})

  expect_true(is.na(f))
  request_log <- file.path(test_dir, ".rawdata2", "RAW_file_by_id.txt")
  expect_true(file.exists(request_log))
  log_lines <- readLines(request_log)
  expect_length(log_lines, 2L)
  expect_match(
    log_lines[2],
    paste0("^8778\t\t[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:")
  )
  request_date <- substr(strsplit(log_lines[2], "\t", fixed = TRUE)[[1]][3], 1L, 10L)
  expect_equal(raw_id2_by_date(request_date), list())
  expect_warning({f <- raw_file_record_by_id(88)})
  expect_true(is.null(f))

  # remove path that is not-valid
  expect_equal(nrow(p),1L)
  raw_path_trim()
  p <- raw_paths_read()
  expect_equal(nrow(p),0L)
})
