#' garnet_atom_units
#'
#' @param database A database with mineral specimen,sample and list of elements with the SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,FeO,MnO,MgO,CaO for garnet
#' @param weights A dataframe with elements weight,number of cation and anions for each oxide,crystallographic site for garnet. The function EW_garnet() provides the element weights but another DF with different values can input
#'
#' @return a dataframe with the atomic units for each element including SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,FeO,MnO,MgO,CaO
#' @export
#'
#' @examples
#' A<-garnet_benchmark_01()
#' garnet_atom_units(A)
#'
garnet_atom_units <- function(database,weights=garnet_EW()) {

  weigths<-weights

 database %>%
    dplyr::mutate(Fe2O3=0)  %>%
    dplyr::select(everything(),SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,FeO,MnO,MgO,CaO) %>%
    tidyr::pivot_longer(cols=SiO2:ncol(.),values_to="value",names_to="element") %>%
    dplyr::left_join(.,weights,by="element")  %>%
    dplyr::mutate(mole_cations=(value*no_cations)/mol_wt,
                  mole_oxigens=(value*no_oxigens)/mol_wt) %>%
    dplyr::group_by(specimen) %>%
    dplyr::mutate(sum_MO=sum(mole_oxigens),        #Sum of anions
                  sum_MC=sum(mole_cations),        #Sum of cations
                  norm_cations=(8*mole_cations)/sum_MC,        #normalize cations to 8 cations
                  norm_oxygen=norm_cations*r_ox_cat,           #normalize cations to 8 cations
                  sum_charges=norm_cations*position,
                  charge_norm_cations=sum(sum_charges),
                  charge_diff=24-charge_norm_cations,
                  atom_units_FE3=ifelse(element=="Fe2O3",ifelse(charge_norm_cations>0,charge_diff,0),norm_cations),
                  atom_units=ifelse(element=="FeO",ifelse(charge_norm_cations>0,norm_cations-charge_diff,norm_cations),atom_units_FE3),
                  normalized_oxygen_FE3=ifelse(element=="Fe2O3",atom_units*r_ox_cat,norm_oxygen),
                  normalized_oxygen=ifelse(element=="FeO",atom_units*r_ox_cat,normalized_oxygen_FE3),
                  sum_AU=sum(atom_units),                 #summatory of atom units
                  sum_NO=sum(normalized_oxygen),          #summatory of anions
                  element=c("Si","Ti","Al","Cr","Fe3","Fe2","Mn","Mg","Ca"))

}

