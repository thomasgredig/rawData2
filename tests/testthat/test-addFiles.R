set.seed(47)

create_N_random_files <- function(test_dir, N=10, data_folder = "raw_source") {
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
      sample(c(letters, LETTERS, 0:9), size = 122, replace = TRUE),
      collapse = ""
    )

    writeLines(random_text, file)
  }

  return(raw_source_dir)
}


test_that("raw_update uses configured file extensions", {
  test_dir <- tempfile("raw-extensions-")
  p <- raw_init(make_paths_unsearchable = TRUE)

  raw_extensions_append(default=TRUE)
  source_dir <- file.path(test_dir, "source")
  dir.create(source_dir, recursive = TRUE)
  writeLines("default", file.path(source_dir, "default.txt"))
  writeLines("custom", file.path(source_dir, "custom.foo"))
  raw_path_append(source_dir)

  initial <- raw_list()
  expect_true("default.txt" %in% initial$filename)
  #expect_false("custom.foo" %in% initial$filename)

  raw_extensions_append(".foo")
  raw_update(verify=TRUE)
  updated <- raw_list()
  expect_true("custom.foo" %in% updated$filename)

  unlink(test_dir, recursive = TRUE, force = TRUE)
})




test_that("raw_update finds random RAW files", {
  N <- 10L  # Change this to any value from 1 through 40

  test_dir <- tempfile("raw-test-")
  dir.create(test_dir, recursive = TRUE)


  # Create a minimal DataLad-like dataset marker.
  #dir.create(".rawdata2")
  p <- raw_init(make_paths_unsearchable = TRUE)
  expect_true(dir.exists(p))

  raw_source_dir <- create_N_random_files(test_dir, N)
  expect_true(dir.exists(raw_source_dir))

  # Register the RAW directory for recursive searching.
  raw_path_append(
    path = raw_source_dir,
    searchable = TRUE
  )

  # Generate RAW_files.csv.
  raw_update()

  # Verify the results.
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
  expect_true(dir.exists(raw_source_dir2))
  raw_path_append(raw_source_dir2)
  raw_update()

  result <- raw_list()
  expect_equal(nrow(result), N+N)
  expect_equal(max(result$ID), N+N+min(result$ID)-1)

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
  file.remove(files[3])
  raw_update()
  result <- raw_list()
  expect_equal(nrow(result), N+N-1)

  # rename one file and expect it to have the same ID
  IDs <- raw_id_by_file(basename(files[2]))
  raw_list(IDs) -> df
  expect_equal(nrow(df), 1)
  ID2 <- df$ID2[1]
  new_filename = sub("random_","new43f_",raw_file_by_id(ID2))
  file.rename(from=raw_file_by_id(ID2), to = new_filename)
  raw_update()
  ID_new <- raw_id_by_file("new43f")
  find_renamed <- raw_list(ID_new)
  expect_true(ID2 %in% find_renamed$ID2)

  # read paths:
  d_paths <- raw_paths_read()
  expect_equal(nrow(d_paths), 2)

  # move one file to another folder and check that the ID remains the same
  raw_source_dir_sub <- file.path(test_dir, "_folder")
  dir.create(
    raw_source_dir_sub,
    recursive = TRUE,
    showWarnings = FALSE
  )
  ID <- raw_id_by_file(basename(files[7]))
  filename_old = raw_file_by_id(raw_list(ID)$ID2[1])
  new_filename = file.path(raw_source_dir_sub, basename(filename_old))
  file.rename(from = filename_old, to=new_filename)
  raw_update()
  raw_init() # just for testing init again, then update again
  result <- raw_update()
  raw_list()
  ID_new <- raw_id_by_file(basename(files[7]))
  expect_true(all(ID %in% ID_new))

  # f <- raw_file_by_id(7)  # CHECK !!!!
  # expect_true(nchar(f)>0)

  # d <- raw_file_record_by_id(7)
  # expect_equal(ncol(d), 6L)
  # expect_true(is.numeric(d$filesize))
  # expect_true(d$filesize > 0)

  # Verify that the catalogue was saved.
  # expect_true(
  #   file.exists(file.path(test_dir, RAWDATA_FOLDER, FILE_FILES))
  # )


  unlink(test_dir, recursive = TRUE, force = TRUE)
})
