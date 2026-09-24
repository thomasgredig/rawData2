test_that("base64 conversion", {
  expect_equal(base64(0L), "0")
  expect_equal(base64(61:65), c("z",".","?", "10" ,"11"))
  expect_equal(base64(64*64), "100")
  expect_equal(base64(64*64*64), "1000")
  expect_equal(base64(64*64*64-1), "???")
  expect_equal(base64(64*64*64*64*64*64-3),"?????z")
  expect_equal(base64(64*64*64*64*64*64*64*64-3),"???????z")
})
