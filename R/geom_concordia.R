#' Error ellipses for concordia diagrams
#'
#' Draws one error ellipse per row from `x`, `y`, `sigma_x`, `sigma_y` and `rho`.
#'
#' @param mapping,data,position,na.rm,show.legend,inherit.aes As in [ggplot2::layer()].
#' @param stat Kept for compatibility; the stat is always `StatConcordia`.
#' @param level Confidence level of the ellipse (default 0.95). Use `NULL` to draw the raw
#'   1-sigma ellipse, as in versions before 0.0.0.9001.
#' @param sigma_level Level of the uncertainties given in `sigma_x`/`sigma_y`: `1` (default,
#'   standard errors) or `2` (2-sigma). With the package convention of `*_2s` columns, either map
#'   `sigma_x = pb207_u235_2s` and set `sigma_level = 2`, or divide by 2 in the mapping.
#' @param n Number of vertices per ellipse.
#' @param ... Other arguments passed to the polygon geom (e.g. `alpha`, `colour`, `fill`).
#'
#' @details Required aesthetics: `x`, `y`, `sigma_x`, `sigma_y`, `rho`. `fill` (and any other
#'   aesthetic) is optional. Each row gets its own polygon, so ellipses never merge, whatever the
#'   grouping.
#' @export
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = pb207_u235, y = pb206_u238, sigma_x = pb207_u235_2s,
#'                sigma_y = pb206_u238_2s, rho = rho_206pb_238u_v_207pb_235u, fill = sample)) +
#'   geom_concordia(sigma_level = 2, alpha = 0.5) +
#'   geom_concordia_line(age_range = c(900, 1200))
#' }
geom_concordia <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           level = 0.95, sigma_level = 1, n = 100, ...) {
  ggplot2::layer(
    stat = StatConcordia,
    data = data,
    mapping = mapping,
    geom = ggplot2::GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, level = level, sigma_level = sigma_level, n = n, ...)
  )
}

#' @rdname geom_concordia
#' @format NULL
#' @usage NULL
#' @export
StatConcordia <- ggplot2::ggproto("StatConcordia", ggplot2::Stat,
  required_aes = c("x", "y", "sigma_x", "sigma_y", "rho"),
  dropped_aes = c("sigma_x", "sigma_y", "rho"),

  compute_panel = function(data, scales, level = 0.95, sigma_level = 1, n = 100) {
    k <- if (is.null(level)) 1 else sqrt(stats::qchisq(level, df = 2))
    angles <- seq(0, 2 * pi, length.out = n)
    unit <- cbind(cos(angles), sin(angles))
    keep_cols <- setdiff(names(data), c("x", "y", "sigma_x", "sigma_y", "rho", "group"))

    polys <- lapply(seq_len(nrow(data)), function(i) {
      d <- data[i, , drop = FALSE]
      sx <- d$sigma_x / sigma_level; sy <- d$sigma_y / sigma_level; r <- d$rho
      if (anyNA(c(d$x, d$y, sx, sy, r)) || sx <= 0 || sy <= 0 || abs(r) > 1) return(NULL)
      cv <- matrix(c(sx^2, r * sx * sy, r * sx * sy, sy^2), 2)
      pts <- k * unit %*% chol(cv)
      out <- data.frame(x = d$x + pts[, 1], y = d$y + pts[, 2], group = i)
      if (length(keep_cols)) out <- cbind(out, d[rep(1, n), keep_cols, drop = FALSE])
      out
    })
    dropped <- sum(vapply(polys, is.null, logical(1)))
    if (dropped > 0) warning(dropped, " ellipse(s) skipped (NA, non-positive sigma or |rho| > 1).",
                             call. = FALSE)
    out <- do.call(rbind, polys)
    rownames(out) <- NULL
    out
  }
)
