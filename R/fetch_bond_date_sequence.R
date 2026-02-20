# Suppress R CMD check notes for NSE column names
utils::globalVariables(c("date", "day_of_week", "trading"))

#' Fetch Bond Trading Date Sequence
#'
#' Generates a sequence of dates with trading day indicators based on the
#' US Government Bond calendar (QuantLib), which approximates the SIFMA
#' corporate bond trading calendar.
#'
#' @param start Date. The start date of the sequence.
#' @param end Date. The end date of the sequence.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{date}{Date. Each date in the sequence.}
#'   \item{day_of_week}{Character. The day of the week (e.g., "Monday").}
#'   \item{trading}{Logical. TRUE if the date is a trading day.}
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_bond_date_sequence(as.Date("2024-01-01"), as.Date("2024-12-31"))
#' }
fetch_bond_date_sequence <- function(start, end) {
  # Load QuantLib US Government Bond calendar (closest to SIFMA corporate bond calendar)
  suppressMessages(bizdays::load_quantlib_calendars(from = start, to = end))

  data.frame(date = seq.Date(start, end, by = "day")) |>
    dplyr::mutate(
      day_of_week = weekdays(date),
      trading = bizdays::is.bizday(date, "QuantLib/UnitedStates/GovernmentBond")
    ) |>
    tibble::as_tibble()
}
