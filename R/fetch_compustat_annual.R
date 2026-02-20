# Suppress R CMD check notes for NSE column names
utils::globalVariables(c(
  "indfmt", "datafmt", "consol", "curcd", "datadate",
  "gvkey", "seq", "ceq", "at", "lt", "txditc", "txdb", "itcb",
  "pstkrv", "pstkl", "pstk", "capx", "oancf", "sale", "cogs",
  "xint", "xrd", "xsga"
))

#' Fetch Compustat Annual Fundamentals
#'
#' Retrieves annual accounting data from the Compustat Fundamentals Annual
#' (comp.funda) table via WRDS, filtered to standard US industrial firms
#' reporting in USD.
#'
#' @details
#' The function applies the following standard filters:
#' \itemize{
#'   \item Industrial format (indfmt = "INDL")
#'   \item Standardized data format (datafmt = "STD")
#'   \item Consolidated statements (consol = "C")
#'   \item US dollar currency (curcd = "USD")
#' }
#'
#' Requires WRDS credentials set in environment variables (WRDS_USER, WRDS_PASSWORD).
#'
#' Based on the \href{https://www.tidy-finance.org/}{Tidy Finance} implementation.
#'
#' @param start_date Character or Date. Start of the sample period (inclusive).
#' @param end_date Character or Date. End of the sample period (inclusive).
#'
#' @return A tibble with columns: gvkey, datadate, seq, ceq, at, lt, txditc,
#'   txdb, itcb, pstkrv, pstkl, pstk, capx, oancf, sale, cogs, xint, xrd, xsga.
#'
#' @importFrom dplyr tbl filter select collect
#' @export
#'
#' @examples
#' \dontrun{
#' compustat <- fetch_compustat_annual(
#'   start_date = "2000-01-01",
#'   end_date = "2023-12-31"
#' )
#' }
fetch_compustat_annual <- function(start_date, end_date) {
  dbs <- create_connections(include_wrds = TRUE)
  wrds <- dbs$wrds
  on.exit(purrr::walk(dbs, DBI::dbDisconnect))

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  funda_db <- tbl(wrds, I("comp.funda"))

  compustat_annual <- funda_db |>
    filter(
      indfmt == "INDL" &
        datafmt == "STD" &
        consol == "C" &
        curcd == "USD" &
        datadate >= start_date &
        datadate <= end_date
    ) |>
    select(
      gvkey,
      datadate,
      seq,
      ceq,
      at,
      lt,
      txditc,
      txdb,
      itcb,
      pstkrv,
      pstkl,
      pstk,
      capx,
      oancf,
      sale,
      cogs,
      xint,
      xrd,
      xsga
    ) |>
    collect()

  compustat_annual
}
