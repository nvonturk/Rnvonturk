utils::globalVariables(c(
  "Start", "End"
))

#' Get NBER Recession Dates
#'
#' Returns a tibble of NBER business cycle recession start and end dates
#' using the \code{tis} package.
#'
#' @return A tibble with columns: start (Date), end (Date).
#'
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' recessions <- get_nber_recessions()
#' }
get_nber_recessions <- function() {
  nber_mat <- tis::nberDates()
  tibble::tibble(
    start = as.Date(as.character(nber_mat[, "Start"]), format = "%Y%m%d"),
    end   = as.Date(as.character(nber_mat[, "End"]),   format = "%Y%m%d")
  )
}

#' @rdname nber_shading
#' @usage NULL
StatNberShading <- ggplot2::ggproto("StatNberShading", ggplot2::Stat,
  compute_panel = function(data, scales) {
    recessions <- get_nber_recessions()
    x_range <- range(data$x, na.rm = TRUE)
    # Filter to recessions that overlap the data range
    recessions <- recessions[recessions$end >= x_range[1] &
                             recessions$start <= x_range[2], ]
    if (nrow(recessions) == 0) return(data.frame())
    # Clip to data range
    recessions$start <- pmax(recessions$start, as.Date(x_range[1], origin = "1970-01-01"))
    recessions$end   <- pmin(recessions$end,   as.Date(x_range[2], origin = "1970-01-01"))
    data.frame(
      xmin = as.numeric(recessions$start),
      xmax = as.numeric(recessions$end),
      ymin = -Inf,
      ymax = Inf
    )
  },
  required_aes = character(0)
)

#' NBER Recession Shading for ggplot2
#'
#' Adds light-blue shaded rectangles for NBER recession periods to a
#' ggplot2 time-series plot. Use as \code{+ nber_shading()} in a ggplot
#' pipeline. The shading automatically clips to the x-axis range of the
#' plotted data so it does not extend beyond your time series.
#'
#' @param fill Fill colour for recession bars. Default \code{"lightblue"}.
#' @param alpha Transparency. Default \code{0.5}.
#'
#' @return A ggplot2 layer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(date = seq(as.Date("2000-01-01"),
#'                             as.Date("2023-12-31"), by = "month"),
#'                  value = cumsum(rnorm(288)))
#' ggplot(df, aes(date, value)) +
#'   nber_shading() +
#'   geom_line()
#' }
nber_shading <- function(fill = "lightblue", alpha = 0.5) {
  ggplot2::layer(
    stat     = StatNberShading,
    geom     = ggplot2::GeomRect,
    data     = NULL,
    mapping  = ggplot2::aes(),
    position = "identity",
    inherit.aes = TRUE,
    show.legend = FALSE,
    params   = list(fill = fill, alpha = alpha, na.rm = TRUE)
  )
}
