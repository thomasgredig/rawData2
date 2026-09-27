#' Converts a number to a base64 string
#' @param numbers A numeric vector of non-negative integer values. `NA`
#'   values are represented by `"-"`.
#' @description
#' This makes a number shorter while using mostly letters and numbers with only two symbols
#' @return A character vector containing one base64-style value for each
#'   element of `numbers`.
#'
#' @noRd
base64 <- function(numbers) {

  digits <- strsplit(
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.?",
    ""
  )[[1]]

  convert_one <- function(number) {
    if (is.na(number)) {
      return("-")
    }
    if (length(number) != 1L ||
        !is.numeric(number) ||
        is.na(number) ||
        !is.finite(number) ||
        number < 0 ||
        number != floor(number)) {
      stop("number must be one non-negative integer:", number)
    }

    if (number == 0) {
      return("0")
    }

    result <- character()

    while (number > 0) {
      remainder <- number %% 64
      result <- c(digits[remainder + 1], result)
      number <- floor(number / 64)
    }

    paste0(result, collapse = "")
  }

  vapply(numbers, convert_one, character(1))
}
