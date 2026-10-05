test_that("strip_directory removes matching directory prefixes", {
  files <- c(
    "/main/RAW/file1.txt",
    "/main/RAW/sub/file2.txt",
    "/main/sub/special/reserve/sub/a/b/file3.txt",
    "/main/sub/special/reserves/file4.txt"
  )

  paths <- c(
    "sub/special",
    "/main/RAW",
    "/main/sub/special/reserve"
  )

  result <- unname(strip_directories(files, paths))

  expected <- c(
    "file1.txt",
    "sub/file2.txt",
    "sub/a/b/file3.txt",
    "/main/sub/special/reserves/file4.txt"
  )

  expect_equal(result, expected)
})
