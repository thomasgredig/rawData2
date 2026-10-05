#' Initialize SQL database
#'
#' Create and initialize a database, if none exists. The SQLite
#' database can store images and large data files, which would
#' otherwise slow down the R package. The database contains the
#' name of the package, includes the version number and ends
#' with .sqlite. Before creating a new database, it checks whether
#' a database already exists; it might have an older version. If
#' so, then it updates (renames) the file to correspond to the
#' latest version. The rawBase object stores information to what
#' is stored in this separate database, in case it gets lost or
#' needs to be recreated.
#'
#' @param rawBase object, use create_rawBase()
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom cli cli_inform
#'
#' @export
raw_initDB <- function() {
  dbName = raw_get_SQL_database()

  if (file.exists(dbName)) {
    # database already exists
    # does not need to be initialized
    warning("SQL database already exists.")
    return(dbName)
  }

  message("Creating new database:", dbName, "\n")

  mydb <- dbConnect(RSQLite::SQLite(), dbName)
  .writeSQLdatabaseInit(mydb)
  .updateSQLhistory(mydb, git_username(), "init")
  dbDisconnect(mydb)

  return(dbName)
}


#' returns the name of the SQL database
#' @noRd
raw_get_SQL_database <- function() {
  root_dir <- find_raw_root()
  if (is.na(root_dir)) {
    stop("No root directory found.")
  }

  rawdata_dir <- file.path(root_dir, ".rawdata2")

  if (!dir.exists(rawdata_dir)) {
    dir.create(rawdata_dir, recursive = TRUE)
  }

  file.path(rawdata_dir, FILE_DB)
}




#' writes a SQLite database initialization
#' @param mydb database connection from DBI::dbConnect
#' @param verbose logical to output extra information
#' @importFrom DBI dbCreateTable
#' @noRd
.writeSQLdatabaseInit <- function(mydb, verbose=FALSE) {
  tbl <- .getSQLtableNames()
  dfAFM_empty = data.frame(ID = integer(),
                           channel = character(),
                           x.conv = integer(),
                           y.conv = integer(),
                           x.pixels = integer(),
                           y.pixels = integer(),
                           z.units = character(),
                           instrument = character(),
                           history = character(),
                           date = character(),
                           description = character(),
                           fullFilename = character())
  dbCreateTable(mydb, tbl$tblNameAFM, dfAFM_empty)
  if (verbose) print(paste("Created data table:",tbl$tblNameAFM))

  dfhist_empty = data.frame(ID = integer(),
                            token = numeric(),
                            date = character(),
                            version = character(),
                            description = character())
  dbCreateTable(mydb, tbl$tblNameHistory, dfhist_empty)
  if (verbose) print(paste("Created data table:",tbl$tblNameHistory))

  invisible(TRUE)
}


#' reads the sqlHistory table
#' @param mydb database connection from DBI::dbConnect
#' @importFrom DBI dbReadTable
#' @noRd
.readSQLhistory<- function(mydb) {
  tbl <- .getSQLtableNames()
  DBI::dbReadTable(mydb, tbl$tblNameHistory)
}

#' Prints the history of the SQLite database
#' @param rawBase rawBase class
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @noRd
raw.showHistoryDB <- function(rawBase) {
  dbFilename = raw.getDatabase(rawBase)
  print(paste("DB name:",dbFilename))
  if (file.exists(dbFilename)) {
    mydb <- dbConnect(RSQLite::SQLite(), dbFilename)
    tbl <- .readSQLhistory(mydb)
    dbDisconnect(mydb)
    print(tbl)
  }
}

#' updates the sqlHistory table
#' @param mydb database connection from DBI::dbConnect
#' @param token a number representing the time
#' @param description string with description of update
#' @importFrom DBI dbWriteTable
#' @noRd
.updateSQLhistory <- function(mydb, token, description) {
  tbl <- .getSQLtableNames()
  tblHist <- .readSQLhistory(mydb)
  if (nrow(tblHist)>0) { ID = max(tblHist$ID)+1 } else { ID = 1 }
  new_row <- data.frame(ID = ID,
                        token = token,
                        date = format(Sys.Date(), "%Y-%m-%d"),
                        version = 0,
                        description = description)

  dbWriteTable(mydb, tbl$tblNameHistory,
               new_row, append = TRUE, row.names = FALSE)
}


#' Returns SQLite table names
#' @noRd
.getSQLtableNames <- function() {
  list(
    tblNameAFM = paste0('afmData'),
    tblNameHistory = paste0('sqlHistory')
  )
}
