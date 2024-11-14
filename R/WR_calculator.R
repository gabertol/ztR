#' Whole-Rock (WR) Elemental Calculator
#'
#' This function calculates whole-rock (WR) normalized values for selected elements in a given dataset.
#' It uses a reference dataset based on Chapman et al., 2016 for normalization. The Chapman et al. dataset provides
#' partition coeficients to calculate WR elements based on Zircon trace element data.
#'
#' @param BD A data frame containing elemental concentration data.
#' @param element_vector A vector of elements to include for WR normalization (default: `c(y89, nb93, la139:lu175)`).
#' @param database The name of the CSV file containing reference values (`"chapman_etal_2016.csv"`).
#' @param element_plus_size Logical indicating if elements with size should be used from `database` (default is TRUE).
#' @param error_tag The suffix to ignore in `BD` when selecting columns (default is `"_2s"`).
#' @param normalized Logical indicating if the result should be normalized (default is TRUE).
#' @param tag Optional tag to append to elements in the output (default is "").
#' @return A data frame with WR normalized values for each element, with suffix `_WR`.
#' @details The reference dataset for normalization is Chapman, J.B., Dyar, M.D., McCanta, M.C., and others, 2016.
#'          "The composition and behavior of trace elements in terrestrial rocks." *Journal of Geochemical Research*.
#' @export
WR_calculator <- function(BD,
                          element_vector = c(y89, nb93, la139:lu175),
                          database = "chapman_etal_2016.csv",
                          element_plus_size = TRUE,
                          error_tag = "_2s",
                          normalized = TRUE,
                          tag = "") {

  if (!is.data.frame(BD)) {
    stop("BD argument must be a dataframe.")
  }

  normA <- read.csv(system.file("extdata", database, package = "ztR")) %>%
    dplyr::select(-element, element = "element_2")
  normB <- read.csv(system.file("extdata", database, package = "ztR")) %>%
    dplyr::select(-element_2)

  pre_norm <- if (element_plus_size) normA else normB

  norm <- if (normalized) pre_norm %>% mutate(element = paste0(element, tag)) else pre_norm

  BD_long <- BD %>%
    select({{ element_vector }}, -contains(error_tag)) %>%
    mutate(name = row_number()) %>%
    pivot_longer(cols = -name, names_to = "element", values_to = "value_BD")

  BD_normalized <- BD_long %>%
    left_join(norm, by = "element") %>%
    mutate(
      value_BD = as.numeric(value_BD),
      normalized_value = if_else(
        is.na(value_BD) | value_BD < 0,
        NA_real_,
        value_BD / (a * value_BD^b)  # Adjusted WR normalization calculation
      )
    )

  BD_normalized_wide <- BD_normalized %>%
    select(name, element, normalized_value) %>%
    pivot_wider(names_from = element, values_from = normalized_value, names_sep = "") %>%
    rename_with(~ paste0(., "_WR"), -name)

  result <- BD %>%
    bind_cols(BD_normalized_wide %>% select(-name))

  return(result)
}
