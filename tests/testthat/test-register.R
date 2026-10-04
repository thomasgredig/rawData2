write_legacy_rawdata <- function(folder) {
  txt <- '"ID","size","missing","filename"
7,21,FALSE,"legacy7.csv"
8,21,FALSE,"legacy8.csv"'

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

test_that("test registers", {
  # go to temporary directory for testing
  old_wd <- getwd()
  test_dir <- tempfile("rawdata2")
  dir.create(test_dir, recursive = TRUE)
  on.exit({
    setwd(old_wd)
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })
  setwd(test_dir)

  # create a legacy file
  folder_data = file.path(test_dir, "data")

  dir.create(folder_data, recursive = TRUE)
  write_legacy_rawdata(folder_data)
  raw_init()

  raw_files_read()  -> df
  expect_equal(nrow(df),2)

  # add 2 test files
  folder_raw = file.path(test_dir, "raw-data-folder")
  dir.create(folder_raw, recursive = TRUE)
  raw_path_append(folder_raw)
  add_test_files(folder_raw, 2L)

  expect_warning(raw_init()) # ID collision(s).
  raw_update()
  raw_files_read()  -> df
  expect_equal(nrow(df),4)

  ID2 <- df[which(df$found==TRUE)[1],'ID2']
  sha256 <- df[which(df$found==TRUE)[1],'sha256']
  filename <- raw_file_by_id(ID2)
  expect_equal(sha256, raw_getID(filename)$sha256)

  expect_true(dir.exists(test_dir))
  file_register <- raw_export_register(test_dir)
  expect_true(file.exists(file_register))

  d <- raw_info(ID2)
  expect_equal(d[['ID2']], ID2)
})

