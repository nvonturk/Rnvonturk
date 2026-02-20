utils::globalVariables(c(
  "date", "release_id", "release_name", "time", "datetime"
))

#' Get BLS Macro Announcement Release Dates
#'
#' Downloads release dates for BLS series (Employment Situation, CPI, PPI)
#' from the FRED API using the fredr package. Requires a FRED API key to be
#' set via \code{fredr::fredr_set_key()} or the \code{FRED_API_KEY} environment
#' variable.
#'
#' All BLS releases are at 8:30 AM ET, except Employment Situation on
#' 1998-11-05 which was delayed to 1:30 PM ET.
#'
#' @return A tibble with columns: datetime (POSIXct), date (Date), time (character),
#'   release_id (integer), release_name (character).
#' @export
#' @importFrom fredr fredr_release_dates
#' @importFrom dplyr mutate select arrange transmute if_else
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tribble tibble
get_bls_release_dates <- function() {
  releases <- tibble::tribble(
    ~release_id, ~release_name,
    50L,         "Employment Situation",
    10L,         "Consumer Price Index",
    46L,         "Producer Price Index"
  )

  message("Downloading BLS release dates from FRED...")
  result <- releases |>
    purrr::pmap_dfr(function(release_id, release_name) {
      message(sprintf("  %s (id=%d)...", release_name, release_id))

      dates <- fredr::fredr_release_dates(
        release_id = release_id,
        include_release_dates_with_no_data = TRUE
      )

      if (nrow(dates) == 0) {
        warning(sprintf("No dates found for %s", release_name))
        return(tibble::tibble(date = Date(), release_id = integer(), release_name = character()))
      }

      dates |>
        dplyr::transmute(
          date = as.Date(date),
          release_id = as.integer(release_id),
          release_name = release_name
        )
    })

  # All BLS releases at 8:30 AM ET, except Employment Situation on 1998-11-05
  # which was delayed to 1:30 PM ET
  result <- result |>
    dplyr::mutate(
      time = dplyr::if_else(
        release_name == "Employment Situation" & date == as.Date("1998-11-05"),
        "1:30pm", "8:30am"
      ),
      datetime = as.POSIXct(
        paste(date, dplyr::if_else(time == "1:30pm", "13:30:00", "08:30:00")),
        tz = "America/New_York"
      )
    ) |>
    dplyr::select(datetime, date, time, release_id, release_name) |>
    dplyr::arrange(release_name, date)

  result
}
