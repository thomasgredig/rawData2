write_legacy_rawdata <- function(folder) {
  txt <- '"ID","size","missing","filename"
7,101,FALSE,"legacy7.csv"
8,108,FALSE,"legacy8.csv"'

  df <- read.csv(text = txt)

  write.csv(df, file.path(folder, "rawBase_index.csv"), row.names = FALSE)
}

add_test_files <- function(folder, numFiles) {
  files <- file.path(
    folder,
    sprintf("testFile_%03d.txt", seq_len(numFiles))
  )

  for (file in files) {
    random_text <- paste(
      sample(c(letters, LETTERS, 0:9), size = 55, replace = TRUE),
      collapse = ""
    )

    writeLines(random_text, file)
  }
}

test_that("test registers",  suppressMessages({
  # go to temporary directory for testing
  test_dir = tempfile(pattern="rawtestreg-")
  dir.create(test_dir, recursive = TRUE)

  # create a legacy file
  folder_data = file.path(test_dir, "data")

  dir.create(folder_data, recursive = TRUE)
  write_legacy_rawdata(folder_data)
  raw_init(make_paths_unsearchable = TRUE)

  raw_list() -> df
  expect_equal(nrow(df),3L)

  expect_true(all(is.na(df$sha256)))

  # add 2 test files + 1 is the legacy8.csv file
  folder_raw = file.path(test_dir, "RAW")
  dir.create(folder_raw, recursive = TRUE)
  raw_path_append(folder_raw)
  add_test_files(folder_raw, 2L)
  file_legacy = file.path(folder_raw,"legacy8.csv")
  write.csv(df,  file_legacy, row.names = FALSE)

  # expect_warning(raw_init()) # ID collision(s).
  raw_update()
  raw_list()  -> df

  expect_equal(nrow(df), 3L)
  raw_update() # should remove duplicate legacy8.csv
  raw_list()  -> df

  expect_equal(nrow(df), 3L)


  expect_true(dir.exists(test_dir))
  file_register <- raw_export_register(test_dir)
  expect_true(file.exists(file_register))


  unlink(test_dir, recursive = TRUE, force = TRUE)
}))

