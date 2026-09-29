#' Pressure from Ti in zircon
#'
#' Solves 0.013 P^2 - 0.21 P + (3.41 - log10(Ti)) = 0 for P (GPa) and returns the smallest real
#' root. Vectorized. Returns `NA` when there is no real solution (with these coefficients, for
#' Ti below ~365 ppm).
#'
#' The source of the coefficients is not documented yet; check before use.
#'
#' @param ti Ti in zircon (ppm). Numeric vector.
#' @return Pressure in GPa (numeric vector, `NA` where there is no real root).
#' @examples
#' zircon_ti_p(c(10, 400, 1000))
#' @export
zircon_ti_p <- function(ti) {
  a <- 0.013; b <- -0.21; c <- 3.41 - log10(ti)
  disc <- b^2 - 4 * a * c
  out <- (-b - sqrt(pmax(disc, 0))) / (2 * a)
  out[is.na(disc) | disc < 0] <- NA_real_
  out
}
