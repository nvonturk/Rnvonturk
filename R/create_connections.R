#' Create Database Connections
#'
#' Establishes connections to SQL Server and optionally to WRDS and SFTP servers.
#'
#' @param include_wrds Logical. If TRUE, creates WRDS database connection.
#' @param include_sftp Logical. If TRUE, creates SFTP connection.
#' @param sftp_folder Character. Folder path for SFTP connection.
#'
#' @return A list containing connection objects.
#' @export
create_connections <- function(include_wrds = FALSE,
                               include_sftp = FALSE,
                               sftp_folder = NULL) {

  dotenv::load_dot_env()

  if (!nzchar(Sys.getenv("mssql_uid")) || !nzchar(Sys.getenv("mssql_pw"))) {
    stop("mssql_uid / mssql_pw not set in environment variables")
  }

  sql_conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "{ODBC Driver 17 for SQL Server}",
    Server = Sys.getenv("sqlserver"),
    Database = "master",
    UID = Sys.getenv("mssql_uid"),
    PWD = Sys.getenv("mssql_pw")
  )

  connections <- list(sql_conn = sql_conn)

  if (isTRUE(include_wrds)) {
    if (!nzchar(Sys.getenv("WRDS_USER")) || !nzchar(Sys.getenv("WRDS_PASSWORD"))) {
      stop("WRDS_USER / WRDS_PASSWORD not set in environment variables")
    }

    connections$wrds <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = "wrds-pgdata.wharton.upenn.edu",
      port = 9737,
      dbname = "wrds",
      user = Sys.getenv("WRDS_USER"),
      password = Sys.getenv("WRDS_PASSWORD"),
      sslmode = "require"
    )
  }

  if (isTRUE(include_sftp)) {
    if (is.null(sftp_folder) || !nzchar(sftp_folder)) {
      stop("sftp_folder must be provided if include_sftp=TRUE.")
    }
    if (!nzchar(Sys.getenv("sftp_username")) || !nzchar(Sys.getenv("sftp_pw"))) {
      stop("sftp_username / sftp_pw not set in environment variables")
    }

    connections$sftp_con <- sftp::sftp_connect(
      server = Sys.getenv("sftpserver"),
      folder = sftp_folder,
      username = Sys.getenv("sftp_username"),
      password = Sys.getenv("sftp_pw"),
      port = 22,
      timeout = 60 * 3
    )
  }

  connections
}
