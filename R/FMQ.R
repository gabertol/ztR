#' Zircon oxybarometer (Loucks et al. 2020)
#'
#' Delta FMQ = 3.998 * log10(Ce / sqrt(Ui * Ti)) + 2.284, with concentrations in ppm and Ui the
#' initial U content (U corrected for decay since crystallization).
#'
#' @param ce Ce in ppm (raw, not chondrite-normalized).
#' @param u U in ppm (present-day).
#' @param ti Ti in ppm.
#' @param age Crystallization age in Ma (used to correct U).
#' @param correct_u If `TRUE` (default) U is corrected to its initial value with [u_corrector()].
#' @references Loucks, R.R., Fiorentini, M.L. and Henríquez, G.J., 2020. New magmatic oxybarometer
#'   using trace elements in zircon. *Journal of Petrology*, 61(3), egaa034.
#' @return Delta FMQ. `NA` where any concentration is zero, negative or missing.
#' @export
FMQ <- function(ce, u, ti, age, correct_u = TRUE) {
  ui <- if (correct_u) u_corrector(u, age) else u
  bad <- is.na(ce) | is.na(ui) | is.na(ti) | ce <= 0 | ui <= 0 | ti <= 0
  out <- rep(NA_real_, length(bad))
  out[!bad] <- 3.998 * log10(ce[!bad] / sqrt(ui[!bad] * ti[!bad])) + 2.284
  out
}
