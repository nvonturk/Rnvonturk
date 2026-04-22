# Suppress R CMD check notes for NSE column names
utils::globalVariables(c(
  "dlycaldt", "siccd", "primaryexch", "conditionaltype", "tradingstatusflg",
  "sharetype", "securitytype", "securitysubtype", "usincflg", "issuertype",
  "permno", "secinfostartdt", "secinfoenddt", "dlyret", "shrout", "dlyprc",
  "date", "mktcap", "prc", "exchange",
  "linktype", "linkprim", "lpermno", "linkdt", "linkenddt", "gvkey"
))

#' Fetch CRSP Daily Stock Returns
#'
#' Retrieves daily stock return data from the CRSP Daily Stock File
#' (crsp.dsf_v2) via WRDS, filtered to common US equities on major exchanges.
#' Computes market capitalization and merges CRSP-Compustat linking
#' information. Queries WRDS in batches of permnos to avoid timeouts on
#' large pulls.
#'
#' @details
#' The function applies the same filters as
#' \code{\link{fetch_crsp_monthly}} via the CRSP security info history table
#' (crsp.stksecurityinfohist):
#' \itemize{
#'   \item Common shares (sharetype = "NS", securitytype = "EQTY",
#'         securitysubtype = "COM")
#'   \item US-incorporated firms (usincflg = "Y")
#'   \item Corporate issuers (issuertype in "ACOR", "CORP")
#'   \item Major exchanges (primaryexch in "N", "A", "Q")
#'   \item Standard conditional types (conditionaltype in "RW", "NW")
#'   \item Actively trading (tradingstatusflg = "A")
#' }
#'
#' Market capitalization is computed as shares outstanding times price,
#' expressed in millions. CRSP-Compustat linking uses the CCM linking table
#' with link types "LU" and "LC" and primary link indicators "P" and "C".
#'
#' Requires WRDS credentials set in environment variables (WRDS_USER, WRDS_PASSWORD).
#'
#' Based on the \href{https://www.tidy-finance.org/}{Tidy Finance} implementation.
#'
#' @param start_date Character or Date. Start of the sample period (inclusive).
#' @param end_date Character or Date. End of the sample period (inclusive).
#' @param permnos Optional integer vector of CRSP permnos to restrict the pull
#'   to. If NULL (default), all permnos passing the standard filters are used.
#' @param batch_size Integer. Number of permnos queried per WRDS batch.
#'   Default 500.
#'
#' @return A tibble with columns: permno, date, ret, shrout, prc, primaryexch,
#'   siccd, mktcap, exchange, gvkey.
#'
#' @importFrom dplyr tbl filter select collect inner_join left_join join_by mutate na_if case_when distinct pull bind_rows arrange
#' @importFrom lubridate ymd today
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' \dontrun{
#' crsp_daily <- fetch_crsp_daily(
#'   start_date = "2020-01-01",
#'   end_date = "2023-12-31",
#'   permnos = c(10107, 14593)
#' )
#' }
fetch_crsp_daily <- function(start_date, end_date, permnos = NULL, batch_size = 500) {
  dbs <- create_connections(include_wrds = TRUE)
  wrds <- dbs$wrds
  on.exit(purrr::walk(dbs, DBI::dbDisconnect))

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  dsf_db <- tbl(wrds, I("crsp.dsf_v2"))
  stksecurityinfohist_db <- tbl(wrds, I("crsp.stksecurityinfohist"))

  stksecurityinfohist_filtered <- stksecurityinfohist_db |>
    filter(
      sharetype == "NS" &
        securitytype == "EQTY" &
        securitysubtype == "COM" &
        usincflg == "Y" &
        issuertype %in% c("ACOR", "CORP") &
        primaryexch %in% c("N", "A", "Q") &
        conditionaltype %in% c("RW", "NW") &
        tradingstatusflg == "A"
    )

  if (is.null(permnos)) {
    permnos <- stksecurityinfohist_filtered |>
      distinct(permno) |>
      pull(permno)
  }

  batches <- ceiling(length(permnos) / batch_size)
  crsp_daily_list <- vector("list", batches)

  for (j in seq_len(batches)) {
    permno_batch <- permnos[
      ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
    ]

    crsp_daily_sub <- dsf_db |>
      filter(permno %in% permno_batch) |>
      filter(dlycaldt >= start_date & dlycaldt <= end_date) |>
      select(-c(siccd, primaryexch, conditionaltype, tradingstatusflg)) |>
      inner_join(
        stksecurityinfohist_filtered |>
          select(permno, secinfostartdt, secinfoenddt, primaryexch, siccd),
        join_by(permno)
      ) |>
      filter(dlycaldt >= secinfostartdt & dlycaldt <= secinfoenddt) |>
      select(
        permno,
        date = dlycaldt,
        ret = dlyret,
        shrout,
        prc = dlyprc,
        primaryexch,
        siccd
      ) |>
      collect() |>
      mutate(
        date = ymd(date),
        shrout = shrout * 1000
      )

    crsp_daily_list[[j]] <- crsp_daily_sub

    message(sprintf(
      "Batch %d out of %d done (%.0f%%)",
      j, batches, 100 * j / batches
    ))
  }

  crsp_daily <- bind_rows(crsp_daily_list)

  crsp_daily <- crsp_daily |>
    mutate(
      mktcap = shrout * prc / 10^6,
      mktcap = na_if(mktcap, 0),
      exchange = case_when(
        primaryexch == "N" ~ "NYSE",
        primaryexch == "A" ~ "AMEX",
        primaryexch == "Q" ~ "NASDAQ",
        .default = "Other"
      )
    )

  ccm_linking_table_db <- tbl(wrds, I("crsp.ccmxpf_lnkhist"))

  ccm_linking_table <- ccm_linking_table_db |>
    filter(
      linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")
    ) |>
    select(permno = lpermno, gvkey, linkdt, linkenddt) |>
    collect() |>
    mutate(linkenddt = replace_na(linkenddt, today()))

  ccm_links <- crsp_daily |>
    inner_join(
      ccm_linking_table,
      join_by(permno),
      relationship = "many-to-many"
    ) |>
    filter(
      !is.na(gvkey) &
        (date >= linkdt & date <= linkenddt)
    ) |>
    select(permno, gvkey, date)

  crsp_daily <- crsp_daily |>
    left_join(ccm_links, join_by(permno, date)) |>
    arrange(permno, date)

  crsp_daily
}
