#' Calculate the Fraction of Ti on the Si-Site in Zircon
#'
#' This function calculates the fraction of titanium (Ti) on the Si-site in zircon as a function of pressure.
#'
#' @param P Numeric. Pressure in gigapascals (GPa).
#'
#' @return Numeric. The fraction of Ti on the Si-site in zircon.
#'
#' @examples
#' # Example usage:
#' zircon_ti_f_site(1.5)
#'
#'
zircon_ti_f_site <- function(P) {
  f <- 1 / (1 + 10^-(3.37 - 0.77 * P))
  return(f)
}
