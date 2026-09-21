test_that("find root directory", {
  raw_init()
  p_root <- find_raw_root()
  expect_true(dir.exists(p_root))
})
