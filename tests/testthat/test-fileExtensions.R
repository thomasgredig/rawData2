test_that("raw_update uses configured file extensions",  suppressMessages({
  test_dir <- tempfile("raw-extensions-")
  p <- raw_init(make_paths_unsearchable = TRUE)

  raw_extensions_append(default=TRUE)
  source_dir <- file.path(test_dir, "source")
  dir.create(source_dir, recursive = TRUE)
  writeLines("default", file.path(source_dir, "default.txt"))
  writeLines("custom", file.path(source_dir, "custom.foo"))
  raw_path_append(source_dir)
  raw_update()

  initial <- raw_list()
  expect_true("default.txt" %in% initial$filename)
  #expect_false("custom.foo" %in% initial$filename)

  raw_extensions_append(".foo")
  raw_update(verify=TRUE)
  updated <- raw_list()
  expect_true("custom.foo" %in% updated$filename)

  unlink(test_dir, recursive = TRUE, force = TRUE)
}))

