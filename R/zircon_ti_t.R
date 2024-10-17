#' Calculate zircon crystallization temperature
#'
#' This function calculates the crystallization temperature of zircon based on various parameters.
#'
#' References
#' Crisp, Laura J., Andrew J. Berry, Antony D. Burnham, Laura A. Miller, and Matthew Newville. 2023. “The Ti-in-Zircon Thermometer Revised: The Effect of Pressure on the Ti Site in Zircon.” Geochimica Et Cosmochimica Acta 360 (November): 241–58. https://doi.org/10.1016/j.gca.2023.04.031.
#' Watson, E. B., D. A. Wark, and J. B. Thomas. 2006. “Crystallization Thermometers for Zircon and Rutile.” Contributions to Mineralogy and Petrology 151 (4): 413–33. https://doi.org/10.1007/s00410-006-0068-5.
#'
#' @param pressure Pressure in giga Pascal.
#' @param aSiO2 Activity fraction of SiO2 (default: 1).
#' @param aTiO2 Activity fraction of TiO2 (default: 1).
#' @param ti_ppm Concentration of titanium in parts per million.
#' @param equation Equation to use for calculation: "crisp" or "watson" (default: "crisp").
#'
#' @return Crystallization temperature of zircon in Celsius.
#'
#' @examples
#' zircon_ti_t(pressure = 5, ti_ppm = 50)
#'
#' @export
zircon_ti_t <- function(pressure, aSiO2 = 1, aTiO2 = 1, ti_ppm, equation = "watson") {


  l_ti<-log10(ti_ppm)


  if (equation == "crisp") {

  f <- zircon_ti_f_site(pressure)
    log_Ti_f <-  log10(ti_ppm* f)

    T <- (4800 / (5.84 - log_Ti_f + 0.12 * pressure + 0.0056 * pressure^3 + log10(aSiO2) - log10(aTiO2)))- 273.15
  }

  if (equation == 'watson') {
    T <- (5080 / (6.01 - l_ti)) - 273
  }

  return(T)

}

