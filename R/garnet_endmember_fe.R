#' garnet_endmember_fe
#'
#' @param database a dataframe with element atomic units per crystallographic site or a dataframe from garnet_site_occupancy()
#'
#' @return a dataframe with garnet endmembers proportion using the Fe3 and Fe2 for grossularia, pyrope, almandine, and spessartine- without the presence of spessartine, uvarovite, andradite and Ca-Ti garnets
#' @export
#'
#' @examples
#' #' garnet_atom_units(garnet_benchmark_01()) %>%  # convert the weight% chemistry data to atomic units
#' garnet_site_occupancy(.) %>%  # define site occupancy per element
#' garnet_endmember_fe(.)
garnet_endmember_fe<-function(database){
  database %>%
    mutate(almandine_fe=100*((Fe2+Fe3)/(Fe3+Fe2+Mn+Mg+Ca)),
           pyrope_fe=100*(Mg/(Fe3+Fe2+Mn+Mg+Ca)),
           grossular_fe=100*(Ca/(Fe3+Fe2+Mn+Mg+Ca)),
           spessartine_fe=100*(Mn/(Fe3+Fe2+Mn+Mg+Ca)),
           uvarovite_fe=0,
           andradite_fe=0,
           ca_ti_garnet__fe=0,
           sum_fe=almandine_fe+pyrope_fe+grossular_fe+spessartine_fe+uvarovite_fe+andradite_fe+ca_ti_garnet__fe) %>%
    dplyr::select(almandine_fe:sum_fe)
}

