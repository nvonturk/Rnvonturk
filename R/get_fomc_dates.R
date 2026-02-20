utils::globalVariables(c(
  "date_time", "Unscheduled", "datetime", "unscheduled",
  "Date", "Time", "time_parsed", "date", "source"
))

#' Get FOMC Meeting Dates
#'
#' Downloads FOMC meeting dates and announcement times from two SF Fed Excel
#' files, deduplicates, and returns a combined tibble. Files are downloaded to
#' a temporary directory and cleaned up automatically.
#'
#' Data sources:
#' \itemize{
#'   \item USMPD.xlsx — Sheet "Statements" (1994–present)
#'   \item monetary-policy-surprises-data.xlsx — Sheet "FOMC (update 2023)" (1988–2023)
#' }
#'
#' @return A tibble with columns: datetime (POSIXct), date (Date), unscheduled (integer).
#' @export
#' @importFrom httr2 request req_headers req_retry req_perform
#' @importFrom readxl read_excel
#' @importFrom lubridate force_tz parse_date_time
#' @importFrom dplyr mutate select filter bind_rows arrange
#' @importFrom stringr str_to_lower
get_fomc_dates <- function() {
  tmp_dir <- tempdir()
  on.exit(unlink(file.path(tmp_dir, c("USMPD_statements.xlsx", "USMPD_surprises.xlsx"))))

  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

  # --- Download files to temp dir ---
  usmpd_path <- file.path(tmp_dir, "USMPD_statements.xlsx")
  httr2::request("https://www.frbsf.org/wp-content/uploads/USMPD.xlsx") |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 5) |>
    httr2::req_perform(path = usmpd_path)

  surprises_path <- file.path(tmp_dir, "USMPD_surprises.xlsx")
  httr2::request("https://www.frbsf.org/wp-content/uploads/monetary-policy-surprises-data.xlsx") |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 5) |>
    httr2::req_perform(path = surprises_path)

  # --- Sheet 1: USMPD.xlsx, "Statements" (1994-present) ---
  stmts_raw <- readxl::read_excel(usmpd_path, sheet = "Statements") |>
    dplyr::select(date_time, Unscheduled)

  stmts <- stmts_raw |>
    dplyr::mutate(
      datetime = lubridate::force_tz(date_time, tzone = "America/New_York"),
      date = as.Date(datetime),
      unscheduled = as.integer(Unscheduled),
      source = "statements"
    ) |>
    dplyr::select(datetime, date, unscheduled, source)

  # --- Sheet 2: monetary-policy-surprises-data.xlsx, "FOMC (update 2023)" (1988-2023) ---
  fomc_raw <- readxl::read_excel(surprises_path, sheet = "FOMC (update 2023)") |>
    dplyr::select(Date, Time, Unscheduled)

  fomc <- fomc_raw |>
    dplyr::mutate(
      date = as.Date(Date),
      time_parsed = lubridate::parse_date_time(Time, "I:Mp", quiet = TRUE),
      datetime = as.POSIXct(paste(date, format(time_parsed, "%H:%M:%S")),
                            tz = "America/New_York"),
      unscheduled = as.integer(Unscheduled),
      source = "surprises"
    ) |>
    dplyr::select(datetime, date, unscheduled, source)

  # --- Combine: surprises before statements cutoff, then statements ---
  stmts_start <- min(stmts$date)
  surprises_pre <- fomc |> dplyr::filter(date < stmts_start)

  result <- dplyr::bind_rows(surprises_pre, stmts) |>
    dplyr::select(-source) |>
    dplyr::arrange(datetime)

  result
}
