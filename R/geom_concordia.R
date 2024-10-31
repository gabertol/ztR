#' Add Concordia Ellipses to a Plot
#'
#' This function adds concordia ellipses to a plot, representing the uncertainty in isotope ratio data.
#' Each ellipse is calculated based on the provided covariance structure of the data points.
#'
#' @param mapping Aesthetic mappings for the plot, passed to `ggplot2::aes`.
#' @param data Optional dataset for the layer. If `NULL`, the default, the data is inherited from the plot.
#' @param stat The statistical transformation to use on the data, defaults to "identity".
#' @param position The position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If `FALSE` (default), missing values are removed with a warning. If `TRUE`, they are removed silently.
#' @param show.legend Should this layer be included in the legends? `NA` (default) includes if any aesthetics are mapped.
#' @param inherit.aes If `TRUE`, inherit the default aesthetics from the plot. If `FALSE`, overrides them.
#' @param ... Other arguments passed to `ggplot2::layer`.
#'
#' @return A ggplot2 layer with concordia ellipses.
#' @export
#' @examples
#' # Assuming `data` contains x, y, sigma_x, sigma_y, rho, and fill columns
#' ggplot(data, aes(x = x, y = y, sigma_x = sigma_x, sigma_y = sigma_y, rho = rho, fill = fill)) +
#'   geom_concordia()
geom_concordia <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatConcordia,
    data = data,
    mapping = mapping,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Concordia Statistical Transformation
#'
#' A ggproto object for computing concordia ellipses based on input data's covariance structure.
#' This transformation generates ellipse points centered at each (x, y) coordinate with the
#' specified covariance values, using `sigma_x`, `sigma_y`, and `rho` as inputs.
#'
#' @format A ggproto object.
#' @export
StatConcordia <- ggproto("StatConcordia", Stat,
                         compute_group = function(data, scales, ...) {
                           ellipses <- lapply(1:nrow(data), function(i) {
                             x <- data$x[i]
                             y <- data$y[i]
                             sigma_x <- data$sigma_x[i]
                             sigma_y <- data$sigma_y[i]
                             rho <- data$rho[i]
                             fill_value <- data$fill[i]

                             # Check for missing values to avoid errors
                             if (is.na(x) || is.na(y) || is.na(sigma_x) || is.na(sigma_y) || is.na(rho)) {
                               return(NULL)
                             }

                             # Covariance matrix to calculate ellipse
                             cov_matrix <- matrix(c(sigma_x^2, rho * sigma_x * sigma_y, rho * sigma_x * sigma_y, sigma_y^2), nrow = 2)

                             # Generate points for the ellipse
                             angles <- seq(0, 2 * pi, length.out = 100)
                             unit_circle <- cbind(cos(angles), sin(angles))
                             ellipse_points <- t(chol(cov_matrix)) %*% t(unit_circle)

                             # Adjust points to the center position (x, y)
                             ellipse_df <- as.data.frame(t(ellipse_points))
                             ellipse_df$x <- ellipse_df$V1 + x
                             ellipse_df$y <- ellipse_df$V2 + y
                             ellipse_df$fill <- fill_value
                             ellipse_df$group <- i

                             return(ellipse_df[, c("x", "y", "fill", "group")])
                           })

                           ellipses <- do.call(rbind, Filter(Negate(is.null), ellipses))
                           return(ellipses)
                         },
                         required_aes = c("x", "y", "sigma_x", "sigma_y", "rho", "fill")
)
