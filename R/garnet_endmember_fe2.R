#' garnet_endmember_fe2
#'
#' @param function to calculate the proportion of garnet end-members without the calculated Fe+3, to calculate Almandine, Pyrope, Grossularia, spessartine. A more generic classification, use it when your're not so sure about the mineral closure and dont have volatile data.
#'
#' @return a dataframe with garnet endmembers proportion using the Fe3 and Fe2 for grossularia, pyrope, almandine, and spessartine - without the presence of spessartine, uvarovite, andradite and Ca-Ti garnets
#' @export
#'
#' @examples
#' #' garnet_atom_units(garnet_benchmark_01()) %>%  # convert the weight% chemistry data to atomic units
#' garnet_site_occupancy(.) %>%  # define site occupancy per element
#' garnet_endmember_fe2(.)

garnet_endmember_fe2<-function(database){
  database %>%
    mutate(almandine_fe2=100*((Fe2)/(Fe2+Mn+Mg+Ca)),
           pyrope_fe2=100*(Mg/(Fe2+Mn+Mg+Ca)),
           grossular_fe2=100*(Ca/(Fe2+Mn+Mg+Ca)),
           spessartine_fe2=100*(Mn/(Fe2+Mn+Mg+Ca)),
           uvarovite_fe2=0,
           andradite_fe2=0,
           ca_ti_garnet__fe2=0,
           sum_fe2=almandine_fe2+pyrope_fe2+grossular_fe2+spessartine_fe2+uvarovite_fe2+andradite_fe2+ca_ti_garnet__fe2) %>%
    dplyr::select(almandine_fe2:sum_fe2)

}
