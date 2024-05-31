#' Calculate Atoms Per Formula Unit (APFU) for Minerals
#'
#' This function calculates the atoms per formula unit (APFU) for each element in a given mineral dataset.
#' The calculations can be based on either the number of oxygens (anions) or cations.
#'
#' @param dataframe DataFrame. A dataframe with element oxides in %weight.
#' @param oxigens Numeric. Number of oxygens used for base calculation.
#' @param cations Numeric. Number of cations used for base calculation.
#' @param weight_location String. The file location of an in-house oxides weight dataframe, otherwise will use the one in the folder ./data.
#' @param base String. Either "anions" or "cations" to indicate the basis of the calculation.
#'
#' @return DataFrame with atoms per formula unit (APFU) for each element without crystallographic site balancing.
#' @export
#'
#' @examples
#' # Example for Tourmaline
#' read.csv("./data/tourmaline_DHZ.csv") %>%
#'   chemical_formula(oxigens = 31, cations = 18, base = "anions")
#'
#' # Example for Epidote
#' read.csv("./data/epidote_DHZ.csv") %>%
#'   chemical_formula(oxigens = c(12.5, 13, 12.5, 12.5, 12.5, 13), cations = 8, base = "cations")

chemical_formula <- function(dataframe, oxigens, cations, weight_location = "/data/element_weights.csv", base = "anions") {



  local <- paste(find.package("ztR"), weight_location, sep = "")
  local_logic <- ifelse(weight_location == "/data/element_weights.csv", local, weight_location)



  weight <- read.csv(local_logic) %>%
    dplyr::mutate(
      mol_wt = (no_cations * weight) + (no_oxigens * 15.9994), # Molecular weight of oxides
      r_ox_cat = no_oxigens / no_cations, # Oxygen/Cation ratio
      r_cat_ox = no_cations / no_oxigens, # Cation/Oxygen ratio
      r_cat_an = weight / mol_wt, # Cation/Anion ratio
      r_an_cat = mol_wt / weight  # Anion/Cation ratio
    )

  elements_select <- unique(weight$element)
  elements_remove <- c("H2O_plus", "H2O_minus")

  dataframe %>%
    dplyr::mutate(
      base_oxigen = oxigens,  # Number of oxygens used as base
      base_cation = cations,  # Number of cations used as base
      across(SiO2:ncol(.), ~ replace(., is.na(.), 0)), # Replace NA values with 0
      H2O = ifelse("H2O_plus" %in% colnames(.), H2O_plus + H2O_minus, 0) # Calculate H2O if columns are present
    ) %>%
    dplyr::select(specimen, base_oxigen, base_cation, dplyr::any_of(elements_select), -dplyr::any_of(elements_remove)) %>%
    tidyr::pivot_longer(cols = SiO2:ncol(.), values_to = "value", names_to = "element") %>%
    dplyr::left_join(weight, by = "element") %>%
    dplyr::mutate(
      ox_mol_prop = value / mol_wt, # Molar proportion of oxygens
      atom_prop_an_per_mol = ox_mol_prop * no_oxigens, # Atomic proportion of anions per mol
      atom_prop_cat_per_mol = ox_mol_prop * no_cations # Atomic proportion of cations per mol
    ) %>%
    dplyr::group_by(specimen) %>%
    dplyr::mutate(
      sum_an = sum(atom_prop_an_per_mol, na.rm = TRUE), # Sum of anion proportions
      sum_cat = sum(atom_prop_cat_per_mol, na.rm = TRUE), # Sum of cation proportions
      fac_an = if(base == "anions") base_oxigen / sum_an else 1, # Anion adjustment factor
      fac_cat = if(base == "cations") base_cation / sum_cat else 1, # Cation adjustment factor
      no_an_ox = atom_prop_an_per_mol * fac_an, # Adjusted anion proportions
      no_cat_ox = atom_prop_cat_per_mol * fac_cat, # Adjusted cation proportions
      APFU_an = no_an_ox * r_cat_ox, # APFU based on anion calculations
      APFU_cat = no_cat_ox # APFU based on cation calculations
    ) %>%
    dplyr::mutate(
      APFU = if(base == "anions") APFU_an else APFU_cat # Choose APFU based on selected base
    ) %>%
    dplyr::select(specimen, element = "pure", APFU) %>%
    tidyr::pivot_wider(values_from = APFU, names_from = element)
}




