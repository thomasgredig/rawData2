#' Finds
#' @export
raw_getID <- function(filename) {
  if (file.exists(filename)) {
    sha256 = digest(
      filename,
      algo = "sha256",
      file = TRUE
    )
  } else {
    sha256 = ""
    warning("File not found: ", filename)
  }
  data.frame(
    ID = NA,
    ID2 = base64(strtoi(substr(sha256, 1, 7),16L)),
    file = basename(filename),
    sha256 = sha256,
    filesize = file.size(filename),
    found=TRUE,
    stringsAsFactors = FALSE
  )
}
