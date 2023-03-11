#' garnet_site_occupancy
#'
#' @param database a dataframe from garnet_atom_units() function
#'
#' @return a dataframe wit
#' @export
#'
#' @examples
#'
#' garnet_atom_units(garnet_benchmark_01(),garnet_EW()) |>
#' garnet_site_occupancy(.)
#'
garnet_site_occupancy <- function(database) {

    database |>
      dplyr::select(specimen,sample,element,atom_units)  |>
      dplyr::group_by(specimen)  |>
      tidyr::pivot_wider(names_from = element,
                  values_from = atom_units)  |>
      dplyr::mutate(sum_f4=Si,
             sum_f6=Al+Ti+Cr+Fe3,
             sum_f8=Fe2+Mn+Mg+Ca)
}
