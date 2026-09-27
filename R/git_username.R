#' Get the configured Git username or system's username
#' @return The configured Git username, the system username when Git has no
#'   configured username, or `NA_character_` when the configured value is
#'   empty.
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
    username <- Sys.info()[["user"]]
    return(username)
  }

  username <- trimws(result[[1]])

  if (identical(username, "")) {
    return(NA_character_)
  } else {
    username
  }
}
