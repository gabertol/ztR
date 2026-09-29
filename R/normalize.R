#' Normalize Elemental Data
#'
#' This function normalizes elemental concentration data using reference values from selected geochemical databases.
#' Users can choose from two available datasets:
#' - `"mcdon_sun_1995.csv"` based on McDonough, W.F. and Sun, S.S., 1995. The composition of the Earth. *Chemical Geology*, 120(3-4), pp.223-253.
#' - `"taylor_mcclennan_1985.csv"` based on Taylor, S.R., 1985. The continental crust: Its composition and evolution. *Geoscience Texts*, 312.
#'
#' @param BD A data frame containing elemental concentration data.
#' @param element_vector A range of elements to include for normalization (default: al27:ta181).
#' @param database The name of the CSV file containing reference values (`"mcdon_sun_1995.csv"` or `"taylor_mcclennan_1985.csv"`).
#' @param element_plus_size Logical indicating if elements with size should be used from `database` (default is TRUE).
#' @param error_tag The suffix to ignore in `BD` when selecting columns (default is `"_2s"`).
#' @param normalized Logical indicating if the result should be normalized (default is TRUE).
#' @param tag Optional tag to append to elements in the output (default is "").
#' @details The reference file is read with `.read_ref()`, which removes a UTF-8 BOM, parses numbers
#'   written with thousands separators and converts wt% to ppm. The McDonough & Sun (1995) CI table
#'   shipped with the package is stored entirely in ppm.
#' @return A data frame with normalized values for each element.
#' @export
normalize <- function(BD,
                      element_vector = al27:ta181,
                      database = "mcdon_sun_1995.csv",
                      element_plus_size = TRUE,
                      error_tag = "_2s",
                      normalized = TRUE,
                      tag = "") {

  if (!is.data.frame(BD)) {
    stop("BD argument must be a dataframe.")
  }

  ref   <- .read_ref(database)
  normA <- ref %>% dplyr::select(-element, element = "element_2")
  normB <- ref %>% dplyr::select(-element_2)

  pre_norm <- if (element_plus_size) normA else normB

  norm <- if (normalized) pre_norm %>% mutate(element = paste0(element, tag)) else pre_norm

  BD_long <- BD %>%
    select({{ element_vector }}, -contains(error_tag)) %>%
    mutate(name = row_number()) %>%
    pivot_longer(cols = -name, names_to = "element", values_to = "value_BD")

  BD_normalized <- BD_long %>%
    left_join(norm, by = "element") %>%
    mutate(
      value_BD = as.numeric(value_BD),  # Convert value_BD to numeric
      value = as.numeric(value),        # Convert value to numeric
      normalized_value = if_else(
        is.na(value_BD) | is.na(value) | value_BD < 0 | value < 0,
        NA_real_,
        value_BD / value
      )
    )

  # Convert back to wide format and add "_N" suffix
  BD_normalized_wide <- BD_normalized %>%
    select(name, element, normalized_value) %>%
    pivot_wider(names_from = element, values_from = normalized_value, names_sep = "") %>%
    rename_with(~ paste0(., "_N"), -name)

  # Remove "name_N" and return the final dataframe
  result <- BD %>%
    bind_cols(BD_normalized_wide %>% select(-name))

  return(result)
}
