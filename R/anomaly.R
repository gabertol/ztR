#' Element anomaly (e.g. Eu/Eu*, Ce/Ce*)
#'
#' Ratio between the normalized concentration of an element and the geometric mean of its two
#' neighbours: `REE / sqrt(LREE * HREE)`.
#'
#' @param REE Normalized concentration of the element (e.g. Eu_N).
#' @param LREE Normalized concentration of the lighter neighbour (e.g. Sm_N).
#' @param HREE Normalized concentration of the heavier neighbour (e.g. Gd_N).
#' @return Numeric vector.
#' @examples
#' anomaly(REE = 0.5, LREE = 1, HREE = 4)   # Eu/Eu* = 0.25
#' @export
anomaly <- function(REE, LREE, HREE) {
  REE / sqrt(LREE * HREE)
}
