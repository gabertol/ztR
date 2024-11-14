#' Classify Rock Types Based on Geochemical Data
#'
#' This function classifies rock types based on various geochemical parameters, using specific thresholds.
#' The classification scheme includes categories such as "Ne-Syenite", "Syenite", "Dolerite", "Carbonatite",
#' "Kimberlite", "Basalt", "Larvikite", and various types of "Granitoid" based on Belousova et al. (2002).
#' Belousova, E. A., Griffin, W. L., O'Reilly, S. Y., & Fisher, N. L. (2002). Igneous zircon: trace element composition as an indicator of source rock type. Contributions to mineralogy and petrology, 143, 602-622.
#'
#' @param data A data frame containing geochemical data for classification.
#' @param lu_col Column name as a string for the `lu175` parameter.
#' @param u_col Column name as a string for the `approx_u` parameter.
#' @param ta_col Column name as a string for the `ta181` parameter.
#' @param hf_col Column name as a string for the `hf` parameter.
#' @param ce_ce_col Column name as a string for the `ce_ce` parameter.
#' @param nb_col Column name as a string for the `nb93` parameter.
#' @param th_u_col Column name as a string for the `th_u` parameter.
#' @return A data frame with an added `classification` column specifying the classified rock type.
#' @examples
#' \dontrun{
#' # Example usage:
#' classify_rock(data = geochemical_data)
#' }
#' @export
classify_rock_long <- function(data,
                               lu_col = "lu175",
                               u_col = "approx_u",
                               ta_col = "ta181",
                               hf_col = "hf",
                              ce_ce_col = "ce_ce",
                              nb_col = "nb93",
                              th_u_col = "th_u") {
  data %>%
    mutate(
      classification = case_when(
        # First branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] < 38 ~ "Ne-Syenite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] < 0.5 ~ "Syenite",

        # Second branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] < 0.58 ~ "Dolerite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] > 0.5 &
          .data[[lu_col]] > 2.3 ~ "Carbonatite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] > 0.5 &
          .data[[lu_col]] < 2.3 ~ "Kimberlite",

        # Third branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] < 0.8 ~  "Basalt",

        # Fourth branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[ce_ce_col]] > 3.9 ~  "Larvikite",

        # Fifth branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[ce_ce_col]] < 3.9  &
          .data[[nb_col]] > 170 ~ "Granitoid (>75% SiO2)",

        # Sixth branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[ce_ce_col]] < 3.9 &
          .data[[nb_col]] < 170 &
          .data[[th_u_col]] > 0.44  ~ "Granitoid (65-70% SiO2)",

        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[ce_ce_col]] < 3.9 &
          .data[[nb_col]] < 170 &
          .data[[th_u_col]] < 0.44 ~ "Granitoid (70-75% SiO2)",

        # Catch-all for unclassified data
        TRUE ~ "Unclassified"
      )
    )
}

classify_rock_short <- function(data, lu_col = "lu175",
                                u_col = "approx_u",
                                hf_col = "hf",
                                y_col="y89",
                                yb_col="yb172") {
  data %>%
    mutate(
      classification = case_when(
        # First branch
        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] > 601 ~ "Ne-Syenite",

        .data[[lu_col]] < 20.71 &
          .data[[hf_col]] < 0.62 ~ "Syenite",

        # Second branch
        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] < 0.8 ~ "Basalt",

        .data[[lu_col]] < 20.71 &
          .data[[hf_col]] > 0.62 &
          .data[[lu_col]] > 2.7 ~ "Carbonatite",

        .data[[lu_col]] < 20.71 &
          .data[[hf_col]] > 0.62 &
          .data[[lu_col]] < 2.7 ~ "Kimberlite",

        # fourth branch

        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] > 0.8 &
          .data[[y_col]] > 4433 &
          .data[[u_col]] < 1149~ "Monzonite",

        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] > 0.8 &
          .data[[y_col]] > 4433 &
          .data[[u_col]] > 1149~ "Granitoid (>75 SiO2)",

        # fifth branch
        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] > 0.8 &
          .data[[y_col]] < 4433 &
          .data[[hf_col]] < 1.015~ "Dolerite",
        #
        # # Sixth branch
        #
        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] > 0.8 &
          .data[[y_col]] < 4433 &
          .data[[hf_col]] > 1.015 &
          .data[[yb_col]] > 501~ "Granitoid (70-75% SiO2)",

        #
        .data[[lu_col]] > 20.71 &
          .data[[lu_col]] < 601 &
          .data[[hf_col]] > 0.8 &
          .data[[y_col]] < 4433 &
          .data[[hf_col]] > 1.015 &
          .data[[yb_col]] < 501~ "Granitoid (<65% SiO2)",

        # Catch-all for unclassified data
        TRUE ~ "Unclassified"
      )
    )
}

classify_rock_long_no_ce <- function(data,
                               lu_col = "lu175",
                               u_col = "approx_u",
                               ta_col = "ta181",
                               hf_col = "hf",
                               nb_col = "nb93",
                               th_u_col = "th_u") {
  data %>%
    mutate(
      classification = case_when(
        # First branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] < 38 ~ "Ne-Syenite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] < 0.5 ~ "Syenite",

        # Second branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] < 0.58 ~ "Dolerite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] > 0.5 &
          .data[[lu_col]] > 2.3 ~ "Carbonatite",

        .data[[lu_col]] < 20.7 &
          .data[[ta_col]] > 0.5 &
          .data[[lu_col]] < 2.3 ~ "Kimberlite",

        # Third branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] < 0.8 ~  "Basalt",

          # Fifth branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[nb_col]] > 170 ~ "Granitoid (>75% SiO2)",

        # Sixth branch
        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[nb_col]] < 170 &
          .data[[th_u_col]] > 0.44  ~ "Granitoid (65-70% SiO2)",

        .data[[lu_col]] > 20.7 &
          .data[[u_col]] > 38 &
          .data[[ta_col]] > 0.58 &
          .data[[hf_col]] > 0.8 &
          .data[[nb_col]] < 170 &
          .data[[th_u_col]] < 0.44 ~ "Granitoid (70-75% SiO2)",

        # Catch-all for unclassified data
        TRUE ~ "Unclassified"
      )
    )
}
