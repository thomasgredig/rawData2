set.seed(1234)

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
      sample(c(letters, LETTERS, 0:9), size = 100, replace = TRUE),
      collapse = ""
    )

    writeLines(random_text, file)
  }

  return(raw_source_dir)
}


test_that("raw_update finds random RAW files", {
  N <- 10L  # Change this to any value from 1 through 40

  old_wd <- getwd()
  test_dir <- tempfile("raw-test-")
  dir.create(test_dir, recursive = TRUE)
  on.exit({
    setwd(old_wd)
    unlink(test_dir, recursive = TRUE, force = TRUE)
  })

  test_that("raw_update fastScan reuses same-size file checksums", {
    old_wd <- getwd()
    test_dir <- tempfile("raw-fast-scan-")
    dir.create(test_dir, recursive = TRUE)
    on.exit({
      setwd(old_wd)
      unlink(test_dir, recursive = TRUE, force = TRUE)
    })
    setwd(test_dir)

    raw_init()
    raw_source_dir <- create_N_random_files(test_dir, 1L)
    initial <- raw_update(raw_source_dir)
    scan_file <- file.path(raw_source_dir, "random_001.txt")
    old_hash <- initial$sha256[initial$file == "random_001.txt"]

    writeLines(strrep("z", 100), scan_file)
    fast <- raw_update()
    expect_equal(fast$sha256[fast$file == "random_001.txt"], old_hash)

    full <- raw_update(fastScan = FALSE)
    expect_true(any(full$found & full$sha256 != old_hash))
  })
  setwd(test_dir)

  # Create a minimal DataLad-like dataset marker.
  #dir.create(".rawdata2")
  p <- raw_init()
  expect_true(dir.exists(p))

  raw_source_dir <- create_N_random_files(test_dir, N)
  expect_true(dir.exists(raw_source_dir))

  # Register the RAW directory for recursive searching.
  raw_path_append(
    path = raw_source_dir,
    searchable = TRUE
  )

  # Generate RAW_files.csv.
  result <- raw_update()

  # Verify the results.
  # ===================
  # test raw_update works
  # ===================
  expect_equal(nrow(result), N)
  expect_equal(sum(result$found), N)
  expect_equal(length(unique(result$ID)), N)
  expect_equal(length(unique(result$sha256)), N)
  #expect_true(all(file.exists(result$file)))
  expect_true(all(nzchar(result$sha256)))
  expect_true(all(result$filesize > 0))

  ## expect lowest ID to be at least 7
  expect_true(min(result$ID)>=7L)

  # Add more files in a different directory
  raw_source_dir2 <- create_N_random_files(test_dir, N, "newdir")
  expect_true(dir.exists(raw_source_dir2))
  raw_path_append(raw_source_dir2)
  result <- raw_update()

  expect_equal(nrow(result), N+N)
  expect_equal(sum(result$found), N+N)
  expect_equal(max(result$ID), N+N+min(result$ID)-1)

  # delete one file and make sure it is missing
  files <- list.files(
    raw_source_dir,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  file.remove(files[3])
  result <- raw_update()
  expect_equal(nrow(result), N+N)
  expect_equal(sum(result$found), N+N-1)

  # rename one file and expect it to have the same ID
  ID <- raw_id_by_file(basename(files[2]))[1]
  new_filename = sub("random_","new43f_",files[2])
  file.rename(from=files[2], to = new_filename)
  result <- raw_update()
  ID_new <- raw_id_by_file("new43f")
  expect_equal(ID, ID_new)

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
  ID <- raw_id_by_file(basename(files[7]))[1]
  new_filename = file.path(raw_source_dir_sub, basename(files[7]))
  file.rename(from = files[7], to=new_filename)
  result <- raw_update()
  raw_init() # just for testing init again, then update again
  result <- raw_update()
  ID_new <- raw_id_by_file(basename(files[7]))[1]
  expect_equal(ID, ID_new)

  f <- raw_file_by_id(7)
  expect_true(nchar(f)>0)

  d <- raw_file_record_by_id(7)
  expect_equal(ncol(d), 6L)
  expect_true(is.numeric(d$filesize))
  expect_true(d$filesize > 0)

  # Verify that the catalogue was saved.
  expect_true(
    file.exists(file.path(test_dir, ".rawdata2", "RAW_files.csv"))
  )
})
