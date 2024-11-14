#' Calculate Whole-Rock Crustal Thickness
#'
#' This function estimates the whole-rock crustal thickness based on La/Yb normalized trace element ratios in zircon.
#' The data and method are based on the work of Profeta, L., Ducea, M.N., Chapman, J.B., Paterson, S.R., Gonzales, S.M.H., Kirsch, M., Petrescu, L. and DeCelles, P.G., 2015. Quantifying crustal thickness over time in magmatic arcs. Scientific reports, 5(1), p.17786.
#'
#' @param BD A data frame containing the trace element data.
#'   It must include columns `la139_WR_N` and `yb172_WR_N` for normalized La and Yb values, respectively.
#' @return A data frame with an additional column `WR_crust` representing the calculated whole-rock crustal thickness.
#' @references Chapman, J.B., Gehrels, G.E., Ducea, M.N., Giesler, N. and Pullen, A. (2016). A new method for estimating parent rock trace element concentrations from zircon. *Chemical Geology*, 439, pp.59-70.
#' @examples
#' \dontrun{
#'   BD <- data.frame(la139_WR_N = c(0.1, 0.2), yb172_WR_N = c(0.05, 0.1))
#'   WR_crustal_thickness(BD)
#' }
#' @export
WR_crustal_thickness <- function(BD) {

  BD %>%
    mutate(WR_crust = 21.277 * log(1.0204 * (la139_WR_N / yb172_WR_N)))

}
