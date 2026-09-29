#' Add a concordia line (legacy orientation)
#'
#' Kept for compatibility. **Note the axes:** x = 206Pb/238U and y = 207Pb/235U, which is the
#' transpose of the usual Wetherill diagram. For the standard orientation (x = 207Pb/235U,
#' y = 206Pb/238U) or a Tera-Wasserburg diagram use [geom_concordia_line()].
#'
#' The layers use `inherit.aes = FALSE`, so they can be added to a plot whose global aesthetics
#' include `sigma_x`, `sigma_y`, `rho` etc. (this failed before version 0.0.0.9001).
#'
#' @param x_interval Interval between age markers (Ma).
#' @param lambda_235,lambda_238 Decay constants (1/yr).
#' @return A list of ggplot2 layers.
#' @export
add_concordia_line <- function(x_interval = 100, lambda_235 = 9.8485e-10, lambda_238 = 1.55125e-10) {

  ages_line <- seq(0, 4500, by = 1) * 1e6
  concordia_line_df <- data.frame(
    x = exp(lambda_238 * ages_line) - 1,  # Pb206/U238
    y = exp(lambda_235 * ages_line) - 1   # Pb207/U235
  )
  ages_labels <- seq(0, 4500, by = x_interval) * 1e6
  concordia_labels_df <- data.frame(
    age = ages_labels / 1e6,
    x = exp(lambda_238 * ages_labels) - 1,
    y = exp(lambda_235 * ages_labels) - 1
  )

  list(
    ggplot2::geom_line(data = concordia_line_df, ggplot2::aes(x = x, y = y),
                       color = "blue", linetype = "solid", inherit.aes = FALSE),
    ggplot2::geom_point(data = concordia_labels_df, ggplot2::aes(x = x, y = y),
                        color = "red", inherit.aes = FALSE),
    ggplot2::geom_text(data = concordia_labels_df, ggplot2::aes(x = x, y = y, label = round(age, 1)),
                       hjust = -0.2, vjust = 0.5, size = 3, inherit.aes = FALSE)
  )
}

#' Concordia line for Wetherill or Tera-Wasserburg diagrams
#'
#' @param type `"wetherill"` (x = 207Pb/235U, y = 206Pb/238U) or `"tw"`
#'   (x = 238U/206Pb, y = 207Pb/206Pb).
#' @param age_range Age range of the line (Ma).
#' @param ticks Ages (Ma) of the labelled markers. Default: `pretty()` over `age_range`.
#' @param lambda_235,lambda_238 Decay constants (1/yr).
#' @param u238_u235 Present-day 238U/235U (used in the Tera-Wasserburg line).
#' @param colour,linewidth Line colour and width.
#' @param label_size Text size of the tick labels (`NA` hides the labels).
#' @return A list of ggplot2 layers (all with `inherit.aes = FALSE`).
#' @examples
#' \dontrun{
#' ggplot(df, aes(pb207_u235, pb206_u238, sigma_x = pb207_u235_2s / 2,
#'                sigma_y = pb206_u238_2s / 2, rho = rho)) +
#'   geom_concordia_line(age_range = c(200, 700)) +
#'   geom_concordia()
#' }
#' @export
geom_concordia_line <- function(type = c("wetherill", "tw"), age_range = c(1, 4500), ticks = NULL,
                                lambda_235 = 9.8485e-10, lambda_238 = 1.55125e-10,
                                u238_u235 = 137.818, colour = "grey30", linewidth = 0.4,
                                label_size = 3) {
  type <- match.arg(type)
  age_range[1] <- max(age_range[1], 0.1)   # TW is undefined at t = 0
  xy <- function(t) {
    r68 <- exp(lambda_238 * t * 1e6) - 1
    r75 <- exp(lambda_235 * t * 1e6) - 1
    if (type == "wetherill") data.frame(age = t, x = r75, y = r68)
    else data.frame(age = t, x = 1 / r68, y = r75 / (u238_u235 * r68))
  }
  line <- xy(seq(age_range[1], age_range[2], length.out = 1000))
  if (is.null(ticks)) ticks <- pretty(age_range)
  ticks <- ticks[ticks >= age_range[1] & ticks <= age_range[2]]
  tk <- xy(ticks)

  out <- list(
    ggplot2::geom_path(data = line, ggplot2::aes(x = x, y = y), colour = colour,
                       linewidth = linewidth, inherit.aes = FALSE),
    ggplot2::geom_point(data = tk, ggplot2::aes(x = x, y = y), colour = colour,
                        inherit.aes = FALSE)
  )
  if (!is.na(label_size)) {
    out <- c(out, list(ggplot2::geom_text(data = tk, ggplot2::aes(x = x, y = y, label = age),
                                          hjust = -0.3, size = label_size, colour = colour,
                                          inherit.aes = FALSE)))
  }
  out
}
