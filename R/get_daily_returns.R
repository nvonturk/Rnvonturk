# Suppress R CMD check notes for NSE column names
utils::globalVariables(c(
  "permno", "sharetype", "securitytype", "securitysubtype", "usincflg",
  "issuertype", "primaryexch", "conditionaltype", "tradingstatusflg",
  "secinfostartdt", "secinfoenddt", "dlycaldt", "dlyret", "dlyprevcap",
  "siccd"
))

#' Get Daily Stock Returns from CRSP
#'
#' Retrieves daily stock returns from the CRSP daily stock file (dsf_v2) via
#' WRDS, filtered to common US equities. Uses parallel batch processing for
#' large queries.
#'
#' @details
#' The function filters to common US equities using the CRSP security info
#' history table (\code{crsp.stksecurityinfohist}) with the following criteria:
#' \itemize{
#'   \item Share type: "NS" (common stock)
#'   \item Security type: "EQTY", subtype "COM"
#'   \item US incorporated (usincflg = "Y")
#'   \item Issuer type: "ACOR" or "CORP"
#'   \item Primary exchange: NYSE ("N"), AMEX ("A"), or NASDAQ ("Q")
#'   \item Conditional type: "RW" or "NW"
#'   \item Actively trading (tradingstatusflg = "A")
#' }
#'
#' PERMNOs are split into batches of 500 and fetched in parallel (up to 4
#' threads) using \code{parallel::makeCluster()} and \code{pbapply::pblapply()}.
#'
#' Requires WRDS credentials set in environment variables (WRDS_USER, WRDS_PASSWORD).
#'
#' @param permnos Integer vector of PERMNOs to fetch. If \code{NULL} (default),
#'   fetches all distinct PERMNOs from the security info table.
#' @param start_date Start date for returns (character or Date). Default "1962-01-01".
#' @param end_date End date for returns (character or Date). Default is today.
#'
#' @return A tibble with columns: permno, date, ret (daily return), prevcap
#'   (previous day market cap), siccd (SIC code).
#'
#' @importFrom dplyr tbl filter select collect inner_join join_by distinct pull bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch returns for specific PERMNOs
#' returns <- get_daily_returns(permnos = c(10107, 14593), start_date = "2020-01-01")
#'
#' # Fetch all common US equity returns
#' all_returns <- get_daily_returns(start_date = "2023-01-01")
#' }
get_daily_returns <- function(permnos = NULL,
                             start_date = "1962-01-01",
                             end_date = lubridate::today()) {

  if (is.null(permnos)) {

    wrds <- Rnvonturk::create_connections(include_wrds = TRUE)$wrds
    on.exit(DBI::dbDisconnect(wrds), add = TRUE)

    stksecurityinfohist_db <- tbl(wrds, I("crsp.stksecurityinfohist"))

    permnos <- stksecurityinfohist_db |>
      distinct(permno) |>
      pull(permno)

    DBI::dbDisconnect(wrds)
  }

  batch_size <- 500
  permno_batches <- split(permnos, ceiling(seq_along(permnos) / batch_size))

  n_cores <- max(1, parallel::detectCores(logical = FALSE) - 1)
  n_workers <- min(n_cores, 4, length(permno_batches))

  fetch_batch <- function(permno_batch) {
    batch_wrds <- Rnvonturk::create_connections(include_wrds = TRUE)$wrds
    on.exit(DBI::dbDisconnect(batch_wrds), add = TRUE)

    batch_dsf <- tbl(batch_wrds, I("crsp.dsf_v2"))
    batch_secinfo <- tbl(batch_wrds, I("crsp.stksecurityinfohist")) |>
      filter(
        sharetype == "NS" &
          securitytype == "EQTY" &
          securitysubtype == "COM" &
          usincflg == "Y" &
          issuertype %in% c("ACOR", "CORP") &
          primaryexch %in% c("N", "A", "Q") &
          conditionaltype %in% c("RW", "NW") &
          tradingstatusflg == "A"
      ) |>
      select(permno, secinfostartdt, secinfoenddt)

    batch_dsf |>
      filter(permno %in% permno_batch) |>
      filter(dlycaldt >= start_date & dlycaldt <= end_date) |>
      inner_join(batch_secinfo, join_by(permno)) |>
      filter(dlycaldt >= secinfostartdt & dlycaldt <= secinfoenddt) |>
      select(permno, date = dlycaldt, ret = dlyret, prevcap = dlyprevcap, siccd) |>
      collect()
  }

  if (n_workers > 1) {
    cl <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    results <- pbapply::pblapply(permno_batches, fetch_batch, cl = cl)
  } else {
    results <- pbapply::pblapply(permno_batches, fetch_batch)
  }

  bind_rows(results)
}
