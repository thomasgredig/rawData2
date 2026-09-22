#' Get the configured Git username
#' @export
git_username <- function() {
  result <- tryCatch(
    system2(
      "git",
      c("config", "--get", "user.name"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character()
  )

  if (length(result) == 0) {
    return(NA_character_)
  }

  username <- trimws(result[[1]])

  if (identical(username, "")) {
    NA_character_
  } else {
    username
  }
}
