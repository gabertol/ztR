#' Add Concordia Line and Age Markers to Plot
#'
#' This function adds a concordia line to a plot, calculated based on decay constants for U-Pb dating.
#' It also marks selected age intervals along the concordia line, with customizable labels.
#'
#' @param x_interval Interval between age markers in millions of years. Default is 100.
#' @param lambda_235 Decay constant for U-235 to Pb-207. Default is 9.8485e-10.
#' @param lambda_238 Decay constant for U-238 to Pb-206. Default is 1.55125e-10.
#'
#' @return A list of ggplot2 layers: the concordia line, age markers as points, and age labels as text.
#' @export
#' @examples
#' # Add concordia line with default settings
#' ggplot() + add_concordia_line()
#'
#' # Customize age marker interval to 200 million years
#' ggplot() + add_concordia_line(x_interval = 200)
add_concordia_line <- function(x_interval = 100, lambda_235 = 9.8485e-10, lambda_238 = 1.55125e-10) {

  # Generate ages for the concordia line in intervals of 1 million years
  ages_line <- seq(0, 4500, by = 1) * 1e6

  # Calculate points for the concordia line
  concordia_line_df <- data.frame(
    x = exp(lambda_238 * ages_line) - 1,  # Pb206/U238
    y = exp(lambda_235 * ages_line) - 1   # Pb207/U235
  )

  # Generate ages for age markers at intervals defined by x_interval
  ages_labels <- seq(0, 4500, by = x_interval) * 1e6

  # Calculate points for age markers
  concordia_labels_df <- data.frame(
    age = ages_labels / 1e6,  # age in millions of years
    x = exp(lambda_238 * ages_labels) - 1,
    y = exp(lambda_235 * ages_labels) - 1
  )

  # Return a list of ggplot2 layers to be added to the plot
  list(
    geom_line(data = concordia_line_df, aes(x = x, y = y), color = "blue", linetype = "solid"),
    geom_point(data = concordia_labels_df, aes(x = x, y = y), color = "red"),
    geom_text(data = concordia_labels_df, aes(x = x, y = y, label = round(age, 1)),
              hjust = -0.2, vjust = 0.5, size = 3)
  )
}
