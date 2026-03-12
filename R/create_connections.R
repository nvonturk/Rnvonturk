#' Create Database Connections
#'
#' Establishes connections to SQL Server, WRDS, and/or SFTP servers.
#'
#' @param include_sql Logical. If TRUE, creates SQL Server database connection.
#' @param include_wrds Logical. If TRUE, creates WRDS database connection.
#' @param include_sftp Logical. If TRUE, creates SFTP connection.
#' @param sftp_folder Character. Folder path for SFTP connection.
#' @param env_file Character. Path to a .env file. Defaults to ".env" in the current working directory.
#'
#' @return A list containing connection objects.
#' @export
create_connections <- function(include_sql = FALSE,
                               include_wrds = FALSE,
                               include_sftp = FALSE,
                               sftp_folder = NULL,
                               env_file = ".env") {

  if (!include_sql && !include_wrds && !include_sftp) {
    warning("No connections requested. Set at least one of include_sql, include_wrds, or include_sftp to TRUE.")
    return(list())
  }

  dotenv::load_dot_env(file = env_file)

  connections <- list()

  if (isTRUE(include_sql)) {
    if (!nzchar(Sys.getenv("mssql_uid")) || !nzchar(Sys.getenv("mssql_pw"))) {
      stop("mssql_uid / mssql_pw not set in environment variables")
    }

    connections$sql_conn <- DBI::dbConnect(
      odbc::odbc(),
      Driver = "{ODBC Driver 17 for SQL Server}",
      Server = Sys.getenv("sqlserver"),
      Database = "master",
      UID = Sys.getenv("mssql_uid"),
      PWD = Sys.getenv("mssql_pw")
    )
  }

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
