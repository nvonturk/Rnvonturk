# Suppress R CMD check notes for NSE column names
utils::globalVariables(c(
  "security_level", "slob", "security_pledge", "asset_backed", "defeased",
  "defeased_date", "bond_type", "pay_in_kind", "pay_in_kind_exp_date",
  "yankee", "canadian", "foreign_currency", "coupon_type", "fix_frequency",
  "coupon_change_indicator", "interest_frequency", "rule_144a",
  "private_placement", "defaulted", "filing_date", "settlement",
  "convertible", "exchange", "putable", "unit_deal", "exchangeable",
  "perpetual", "preferred_security", "complete_cusip", "maturity",
  "offering_amt", "offering_date", "dated_date", "coupon",
  "last_interest_date", "issue_id", "issuer_id", "sic_code",
  "country_domicile", "action_type", "effective_date"
))

#' Fetch FISD Sample Data
#'
#' Retrieves a filtered sample of corporate bond data from the FISD (Fixed Income
#' Securities Database) via WRDS. Applies standard filters commonly used in
#' corporate bond research to exclude non-standard securities.
#'
#' @details
#' The function filters bonds to include only:
#' \itemize{
#'   \item Senior securities (security_level = "SEN")
#'   \item Non-asset-backed, non-defeased bonds
#'   \item Standard corporate debt types (CDEB, CMTN, CMTZ, CZ, USBN)
#'   \item Fixed or zero coupon bonds
#'   \item US-domiciled issuers
#'   \item Non-convertible, non-putable, non-exchangeable bonds
#'   \item Non-144A, non-private placement securities
#' }
#'
#' Requires WRDS credentials set in environment variables (WRDS_USER, WRDS_PASSWORD).
#'
#' Based on the TidyFinance implementation.
#'
#' @return A data frame containing bond characteristics including complete_cusip,
#'   maturity, offering_amt, offering_date, dated_date, interest_frequency,
#'   coupon, last_interest_date, issue_id, issuer_id, sic_code, action_type
#'   (B=matured, E=exchanged, IM=called), and effective_date of the action.
#'
#' @importFrom dplyr tbl filter select collect inner_join join_by group_by ungroup union_all distinct left_join
#' @export
#'
#' @examples
#' \dontrun{
#' fisd_data <- fetch_fisd_sample()
#' }
fetch_fisd_sample <- function() {
  dbs <- create_connections(include_wrds = TRUE)
  wrds <- dbs$wrds
  on.exit(purrr::walk(dbs, DBI::dbDisconnect))
  
  fisd_mergedissue_db <- tbl(wrds, I("fisd.fisd_mergedissue"))
  
  fisd <- fisd_mergedissue_db |>
    filter(
      security_level == "SEN",
      slob == "N" | is.na(slob),
      is.na(security_pledge),
      asset_backed == "N" | is.na(asset_backed),
      defeased == "N" | is.na(defeased),
      is.na(defeased_date),
      bond_type %in% c(
        "CDEB",
        "CMTN",
        "CMTZ",
        "CZ",
        "USBN"
      ), 
      pay_in_kind != "Y" | is.na(pay_in_kind),
      is.na(pay_in_kind_exp_date),
      yankee == "N" | is.na(yankee),
      canadian == "N" | is.na(canadian),
      foreign_currency == "N",
      coupon_type %in% c(
        "F",
        "Z"
      ), 
      is.na(fix_frequency),
      coupon_change_indicator == "N",
      interest_frequency %in% c(
        "0",
        "1",
        "2",
        "4",
        "12"
      ),
      rule_144a == "N",
      private_placement == "N" | is.na(private_placement),
      defaulted == "N",
      is.na(filing_date),
      is.na(settlement),
      convertible == "N",
      is.na(exchange),
      putable == "N" | is.na(putable),
      unit_deal == "N" | is.na(unit_deal),
      exchangeable == "N" | is.na(exchangeable),
      perpetual == "N",
      preferred_security == "N" | is.na(preferred_security)
    ) |> 
    select(
      complete_cusip, maturity,
      offering_amt, offering_date,
      dated_date, 
      interest_frequency, coupon,
      last_interest_date, 
      issue_id, issuer_id
    ) |>
    collect()
  
  fisd_mergedissuer_db <- tbl(wrds, I("fisd.fisd_mergedissuer")) 
  
  fisd_issuer <- fisd_mergedissuer_db |>
    select(issuer_id, sic_code, country_domicile) |>
    collect()
  
  fisd <- fisd |>
    inner_join(fisd_issuer, join_by(issuer_id)) |>
    filter(country_domicile == "USA") |>
    select(-country_domicile)

  amt_out <- tbl(wrds, dbplyr::in_schema("fisd", "fisd_amount_outstanding"))
  amt_out_hist <- tbl(wrds, dbplyr::in_schema("fisd", "fisd_amt_out_hist"))

  # Collect actions which lead to end of life for a bond (B=matured, E=exchanged, IM=called)
  fisd_amtout <- purrr::map(
    list(amt_out, amt_out_hist),
    \(df) df |>
      filter(action_type %in% c("B", "E", "IM")) |>
      select(issue_id, action_type, effective_date)
  ) |>
    purrr::reduce(union_all) |>
    distinct()

  # For each issue_id, take the earliest action
  fisd_amtout <- fisd_amtout |>
    group_by(issue_id) |>
    filter(effective_date == min(effective_date, na.rm = TRUE)) |>
    ungroup() |>
    collect()

  fisd <- fisd |>
    left_join(fisd_amtout, by = join_by(issue_id == issue_id))

  fisd
}