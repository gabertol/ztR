#' chemical formula
#'
#' @param variables
#'
#'        dataframe - a dataframe with element oxides in %weight
#'        oxigens - number of oxigens used for base calculation
#'        cations - number of cations used for base calculation
#'        weight_location - the data location of an inhoude oxides weight dataframe, otherwise will use the one in folder ./data
#'
#' @return dataframe with atoms per formula square for each element without a crystallographic site balancing.
#' @export
#'
#' @examples
#'
#' #tourmaline
#' read.csv("./data/tourmaline_DHZ.csv") %>%
#' chemical_formula(.,31,16)
#'
#' # Epidote
#' read.csv("./data/epidote_DHZ.csv") %>%
#' chemical_formula(.,oxigens=c(12.5,13,12.5,12.5,12.5,13),8)
#'
chemical_formula<- function(dataframe,oxigens,cations,weight_location="/data/element_weights.csv") {

    local<-paste(paste(find.package("ztR"),"./data/element_weights.csv",sep="")
  weight<-read.csv(local) %>%
    dplyr::mutate(mol_wt=(no_cations*weight)+(no_oxigens*15.9994),
                  r_ox_cat=no_oxigens/no_cations,
                  r_cat_ox=no_cations/no_oxigens,
                  r_cat_an=weight/mol_wt,
                  r_an_cat=mol_wt/weight)


    dataframe %>%
    mutate(base_oxigen=oxigens,
           base_cation=cations) %>%
    dplyr::select(specimen,mineral,base_oxigen,base_cation,SiO2,everything(),-X) %>%
    tidyr::pivot_longer(cols=SiO2:ncol(.),values_to="value",names_to="element") %>%
    left_join(.,weight,by="element") %>%
    mutate(ox_mol_prop=value/mol_wt,
           atom_prop_an_per_mol=ox_mol_prop*no_oxigens,
           atom_prop_cat_per_mol=ox_mol_prop*no_cations) %>%
    group_by(specimen) %>%
    mutate(sum_an=sum(atom_prop_an_per_mol,na.rm = TRUE),
           fac_an=base_oxigen/sum_an,
           no_an_ox=atom_prop_an_per_mol*fac_an,
           APFU=no_an_ox*r_cat_ox,
           APFU=ifelse(is.na(APFU),0,APFU)) %>%
    dplyr::select(specimen,mineral,element,APFU) %>%
    pivot_wider(values_from = APFU,names_from = element)

}
