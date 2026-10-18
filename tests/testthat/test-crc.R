test_that("CRC code works", {
  test_dir <- tempdir()
  file = file.path(test_dir, "getCRC.txt")
  random_text="Hello World!"
  writeLines(random_text, file)

  crc = raw_getCRC(file)
  expect_equal(crc, 148756670)
  unlink(test_dir, recursive = TRUE, force = TRUE)
})
