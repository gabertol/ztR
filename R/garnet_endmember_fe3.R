#' garnet_endmember_fe3
#'
#' @param database a dataframe with element atomic units per crystallographic site or a dataframe from garnet_site_occupancy()
#'
#' @return a dataframe with garnet endmembers proportion using the Fe+3 for spessartine, uvarovite, andradite and Ca-Ti garnets, without the presence of grossularia
#' @export
#'
#' @examples
#' garnet_atom_units(garnet_benchmark_01()) %>%  # convert the weight% chemistry data to atomic units
#' garnet_site_occupancy(.) %>%  # define site occupancy per element
#' garnet_endmember_fe3(.)
#'
garnet_endmember_fe3<-function(database){
  database %>%
    mutate(almandine_fe3=100*(Fe2/(Fe2+Mn+Mg+Ca)),
           pyrope_fe3=100*(Mg/(Fe2+Mn+Mg+Ca)),
           grossular_fe3=0,
           spessartine_fe3=100*(Mn/(Fe2+Mn+Mg+Ca)),
           uvarovite_fe3=100*((Cr/(Ti+Al+Cr+Fe3))*(Ca/(Fe2+Mn+Mg+Ca))),
           andradite_fe3=100*((Fe3/(Ti+Al+Cr+Fe3))*(Ca/(Fe2+Mn+Mg+Ca))),
           ca_ti_garnet__fe3=100*((Ti/(Ti+Al+Cr+Fe3))*(Ca/(Fe2+Mn+Mg+Ca))),
           sum_fe3=almandine_fe3+pyrope_fe3+grossular_fe3+spessartine_fe3+uvarovite_fe3+andradite_fe3+ca_ti_garnet__fe3) %>%
    dplyr::select(almandine_fe3:sum_fe3)
}
