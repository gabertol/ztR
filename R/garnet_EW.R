#' garnet_EW
#'
#' @return a tibble of the elements, atomic weight of the cations, number of oxigens, and crystallographic site for each element of the mineral garnet.
#' @export
#'
#' @examples
#' garnet_EW()
garnet_EW <- function() {

  tibble::tribble(~element,~weight,~no_cations,~no_oxigens,~position,
                  "SiO2",28.0855,1,2,4,
                  "Al2O3",26.98154,2,3,3,
                  "FeO",55.847,1,1,2,
                  "Fe2O3",55.847,2,3,3,
                  "MgO",24.305,1,1,2,
                  "CaO",40.08,1,1,2,
                  "Na2O",22.98977,2,1,2,
                  "TiO2",47.90,1,2,4,
                  "MnO",54.938044,1,1,2,
                  "K2O",94.196,2,1,2,
                  "BaO",137.77,1,1,2,
                  "Cr2O3",51.996,2,3,3) |>
    dplyr::mutate(mol_wt=(no_cations*weight)+(no_oxigens*15.9994),
                  r_ox_cat=no_oxigens/no_cations)
}
