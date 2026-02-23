# Suppress R CMD check notes for NSE column names
utils::globalVariables(c(
  "mthcaldt", "siccd", "primaryexch", "conditionaltype", "tradingstatusflg",
  "sharetype", "securitytype", "securitysubtype", "usincflg", "issuertype",
  "permno", "secinfostartdt", "secinfoenddt", "mthret", "shrout", "mthprc",
  "date", "mktcap", "prc", "exchange", "mktcap_lag",
  "linktype", "linkprim", "lpermno", "linkdt", "linkenddt", "gvkey"
))

#' Fetch CRSP Monthly Stock Returns
#'
#' Retrieves monthly stock return data from the CRSP Monthly Stock File
#' (crsp.msf_v2) via WRDS, filtered to common US equities on major exchanges.
#' Computes market capitalization, lagged market capitalization, and merges
#' CRSP-Compustat linking information.
#'
#' @details
#' The function applies the following standard filters via the CRSP
#' security info history table (crsp.stksecurityinfohist):
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
#' expressed in millions. Lagged market capitalization is the prior month's
#' value. CRSP-Compustat linking uses the CCM linking table with link types
#' "LU" and "LC" and primary link indicators "P" and "C".
#'
#' Requires WRDS credentials set in environment variables (WRDS_USER, WRDS_PASSWORD).
#'
#' Based on the \href{https://www.tidy-finance.org/}{Tidy Finance} implementation.
#'
#' @param start_date Character or Date. Start of the sample period (inclusive).
#' @param end_date Character or Date. End of the sample period (inclusive).
#'
#' @return A tibble with columns: permno, date, ret, shrout, prc, primaryexch,
#'   siccd, mktcap, mktcap_lag, exchange, gvkey.
#'
#' @importFrom dplyr tbl filter select collect inner_join left_join join_by mutate na_if case_when
#' @importFrom lubridate floor_date ymd %m+% today
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' \dontrun{
#' crsp_monthly <- fetch_crsp_monthly(
#'   start_date = "2000-01-01",
#'   end_date = "2023-12-31"
#' )
#' }
fetch_crsp_monthly <- function(start_date, end_date) {
  dbs <- create_connections(include_wrds = TRUE)
  wrds <- dbs$wrds
  on.exit(purrr::walk(dbs, DBI::dbDisconnect))

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  msf_db <- tbl(wrds, I("crsp.msf_v2"))
  stksecurityinfohist_db <- tbl(wrds, I("crsp.stksecurityinfohist"))

  crsp_monthly <- msf_db |>
    filter(mthcaldt >= start_date & mthcaldt <= end_date) |>
    select(-c(siccd, primaryexch, conditionaltype, tradingstatusflg)) |>
    inner_join(
      stksecurityinfohist_db |>
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
        select(permno, secinfostartdt, secinfoenddt, primaryexch, siccd),
      join_by(permno)
    ) |>
    filter(mthcaldt >= secinfostartdt & mthcaldt <= secinfoenddt) |>
    mutate(date = floor_date(mthcaldt, "month")) |>
    select(
      permno,
      date,
      ret = mthret,
      shrout,
      prc = mthprc,
      primaryexch,
      siccd
    ) |>
    collect() |>
    mutate(
      date = ymd(date),
      shrout = shrout * 1000
    )

  crsp_monthly <- crsp_monthly |>
    mutate(
      mktcap = shrout * prc / 10^6,
      mktcap = na_if(mktcap, 0)
    )

  mktcap_lag <- crsp_monthly |>
    mutate(date = date %m+% months(1)) |>
    select(permno, date, mktcap_lag = mktcap)

  crsp_monthly <- crsp_monthly |>
    left_join(mktcap_lag, join_by(permno, date))

  crsp_monthly <- crsp_monthly |>
    mutate(
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

  ccm_links <- crsp_monthly |>
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

  crsp_monthly <- crsp_monthly |>
    left_join(ccm_links, join_by(permno, date))

  crsp_monthly
}
