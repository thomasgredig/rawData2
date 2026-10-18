#' CRC code for file (per rawData)
#' @description
#' Uses MD5 checksum or 32-byte MD5 hash for file, then shortens a bit
#' to store as a simple integer
#'
#' @param filename A path to the file to checksum.
#' @return The shortened checksum as an integer, or `NA` when the file does not
#'   exist.
#' @importFrom tools md5sum
#' @export
raw_getCRC <- function(filename) {
  crc = NA
  if(file.exists(filename)) {
    crc = strtoi( substr(md5sum(filename),1,7), base = 16 )
  }
  crc
}
