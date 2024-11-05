#' Convert PPM to Percentage
#'
#' This function converts a value from parts per million (PPM) to a percentage.
#'
#' @param ppm_value Numeric value representing the concentration in parts per million.
#' @return A numeric value representing the concentration in percentage.
#' @examples
#' ppm_to_percent(10000) # returns 1
#' ppm_to_percent(5000)  # returns 0.5
#' @export
ppm_to_percent <- function(ppm_value) {
  ppm_value / 10000
}
