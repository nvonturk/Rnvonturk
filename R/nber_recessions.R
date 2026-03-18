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

#' NBER Recession Shading for ggplot2
#'
#' Adds light-blue shaded rectangles for NBER recession periods to a
#' ggplot2 time-series plot. Use as \code{+ nber_shading()} in a ggplot
#' pipeline. The layer is added behind existing plot layers so it does not
#' obscure data.
#'
#' @param fill Fill colour for recession bars. Default \code{"lightblue"}.
#' @param alpha Transparency. Default \code{0.5}.
#'
#' @return A \code{ggplot2::annotate} layer.
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
  recessions <- get_nber_recessions()
  ggplot2::annotate(
    "rect",
    xmin  = recessions$start,
    xmax  = recessions$end,
    ymin  = -Inf,
    ymax  = Inf,
    fill  = fill,
    alpha = alpha
  )
}
