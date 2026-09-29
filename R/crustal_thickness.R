#' Crustal thickness from zircon Eu/Eu*
#'
#' Linear calibration `84.92 * Eu/Eu* + 24.5` (km). The coefficients are attributed to Tang et al.
#' (2021) and still need to be checked against the publication.
#'
#' @param eu_eu_N Zircon Eu/Eu* (chondrite-normalized), see [anomaly()].
#' @return Crustal thickness in km.
#' @export
crustal_thickness <- function(eu_eu_N) {
  84.92 * eu_eu_N + 24.5
}
