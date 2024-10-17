normalize <- function(BD, element_vector = al27:ta181, database = "mcdon_sun_1995.csv", element_plus_size = TRUE,error_tag="_2s") {


  if (!is.data.frame(BD)) {
    stop("BD argument must be a dataframe.")
  }


  normA <- read.csv(system.file("extdata", database, package = "ztR")) %>% dplyr::select(-element,element="element_2")
  normB <- read.csv(system.file("extdata", database, package = "ztR")) %>% dplyr::select(-element_2)


  norm <- if (element_plus_size) normA else normB

  BD_long <- BD %>%
    select({{ element_vector }}, -contains(error_tag)) %>%
    mutate(name = row_number()) %>%
    pivot_longer(cols = -name, names_to = "element", values_to = "value_BD")


  BD_normalized <- BD_long %>%
    left_join(norm, by = "element") %>%
    mutate(
      value_BD = as.numeric(value_BD),  # Converte value_BD para numérico
      value = as.numeric(value),        # Converte value para numérico
      normalized_value = if_else(
        is.na(value_BD) | is.na(value) | value_BD < 0 | value < 0,
        NA_real_,
        value_BD / value
      )
    )

  # Convertendo de volta para o formato largo e adicionando o sufixo "_N"
  BD_normalized_wide <- BD_normalized %>%
    select(name, element, normalized_value) %>%
    pivot_wider(names_from = element, values_from = normalized_value, names_sep = "") %>%
    rename_with(~ paste0(., "_N"), -name)  # Adiciona o sufixo "_N" às colunas, exceto "name"

  # Removendo a coluna "name_N" e retornando o dataframe final
  result <- BD %>%
    bind_cols(BD_normalized_wide %>% select(-name))

  return(result)
}
