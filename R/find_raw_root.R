#' Finds the directory with .rawdata2
#'
#' Either it finds the .rawdata2 directory or the nearest .git directory
#' @return The normalized path of the nearest directory containing `.rawdata2`
#'   or `.git`, or `NA_character_` when neither is found.
#' @export
find_raw_root <- function() {
  getRPojectRoot()
}

#' Finds a directory for the root (rawdata2 > 0.4)
#' @importFrom tools R_user_dir
#' @importFrom rprojroot find_root is_r_package
#' @noRd
getRPojectRoot <- function() {
  # check if there is a package
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_r_package),
    error = function(e) NULL
  )

  if (!is.null(root)) { # if there is a package then
    if (!dir.exists(root)) root <- find_raw_root() else {
      pgkname = basename(root)
      user_dir = tools::R_user_dir(pgkname,which="data")
      # message("User Directory:   ", user_dir)
    }
  } else { # in case of no package
    # use general directory on Windows or MacOS
    user_dir = Sys.getenv("HOME")
  }

  user_dir
}

#' @noRd
getLegacyRoot <- function() {
  path = getwd()
  repeat {
    if (grepl("\\.Rcheck$", basename(path))) {
      package_dir <- file.path(
        dirname(path),
        sub("\\.Rcheck$", "", basename(path))
      )

      if (dir.exists(package_dir) &&
          (dir.exists(file.path(package_dir, ".git")) ||
           dir.exists(file.path(package_dir, RAWDATA_FOLDER_LEGACY)))) {
        return(normalizePath(package_dir, mustWork = TRUE))
      }
    }

    if (dir.exists(file.path(path, RAWDATA_FOLDER_LEGACY)) ||
        dir.exists(file.path(path, ".git"))) {
      return(path)
    }

    parent <- dirname(path)
    if (parent == path) return(NA_character_)
    path <- parent
  }
}
