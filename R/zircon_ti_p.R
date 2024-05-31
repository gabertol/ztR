#' Calculate the Pressure for a Given Ti Concentration in Zircon
#'
#' This function calculates the pressure (in GPa) for a given Ti concentration in zircon (Ti concentration in ppm).
#' The function uses a quadratic equation to solve for pressure.
#'
#' @param ti Numeric. The value of Ti concentration in ppm.
#'
#' @return Numeric. The pressure in gigapascals (GPa) or NA if no real solution exists.
#'
#' @examples
#' # Example usage:
#' zircon_ti_p(3.0)
#'
#'
zircon_ti_p <- function(ti) {
  l_ti<-log10(ti)

  a <- 0.013
  b <- -0.21
  c <- 3.41 - l_ti

  # Solver
  coeficients <- c(a, b, c)
  solutions <- pracma::roots(coeficients)

  if (is.numeric(solutions)) {
    return(min(solutions))
  }

  else {
    return(NA)
  }
}
