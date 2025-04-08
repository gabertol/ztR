#' Calculate U-Pb Ages Using IsoplotR
#'
#' This function calculates U-Pb ages using `IsoplotR::age`, handling possible errors and returning
#' a data frame with age values and associated uncertainties.
#'
#' @param df A data frame with columns for isotopic ratios and uncertainties (`pb207_u235`, `pb207_u235_2s`,
#' `pb206_u238`, `pb206_u238_2s`, `pb207_pb206`, `pb207_pb206_2s`, `rho_207pb_206pb_v_238u_206pb`).
#' @param age_type Numeric specifying the type of age to compute in `IsoplotR::age`. Default is 1.
#'
#' @return A data frame combining the input `df` with calculated ages (`age_75`, `age_68`, `age_76`, and concordia age).
#' @export
#' @examples
#' # Assuming `df` is a data frame with the required columns
#' use_isoplotr(df, age_type = 1)
use_isoplotr <- function(df, age_type = 1) {

  # Define column names for the result data frame
  col_name <- c("age_75", "s_2_75", "age_68", "s_2_68", "age_76", "s_2_76", "age_concordia", "s_2_concordia","discordance_concordia")

  # Initialize an empty error data frame with the defined columns
  error_df <- data.frame(matrix(NA, nrow = 1, ncol = length(col_name)), stringsAsFactors = FALSE) %>%
    `colnames<-`(col_name)

  # Create a safe wrapper for the IsoplotR age function to handle errors
  safe_age <- purrr::possibly(.f = IsoplotR::age, otherwise = error_df)

  # Calculate age using IsoplotR, handling errors with `safe_age`
  result <- df %>%
    dplyr::select(pb207_u235, pb207_u235_2s, pb206_u238, pb206_u238_2s, pb207_pb206, pb207_pb206_2s, rho_207pb_206pb_v_238u_206pb) %>%
    IsoplotR::read.data(method = "U-Pb", type = 3, ierr = 2) %>%
    safe_age(type = age_type,discordance=IsoplotR::discfilter(option='c'))

  # Rename columns to match defined names
  colnames(result) <- col_name

  # Return combined data frame
  return(bind_cols(df, result))
}

#' Calculate U-Pb Ages in a Tidy Format
#'
#' This function applies `use_isoplotr` to each row of a data frame, calculating U-Pb ages for each sample
#' and returning a combined result in a tidy format.
#'
#' @param df A data frame with columns for isotopic ratios and uncertainties required by `use_isoplotr`.
#' @param age_type Numeric specifying the type of age to compute in `use_isoplotr`. Default is 1.
#'
#' @return A data frame with U-Pb ages and uncertainties for each row in `df`.
#' @export
#' @examples
#' # Calculate U-Pb ages in a tidy format for each row of `df`
#' tidy_isoplotr(df, age_type = 1)
tidy_isoplotr <- function(df, age_type = 1) {
  df %>%
    split(1:nrow(.)) %>%
    purrr::map_dfr(~ use_isoplotr(., age_type = age_type))
}
