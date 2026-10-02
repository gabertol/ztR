# Error ellipses for concordia diagrams

Draws one error ellipse per row from `x`, `y`, `sigma_x`, `sigma_y` and
`rho`.

## Usage

``` r
geom_concordia(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  level = 0.95,
  sigma_level = 1,
  n = 100,
  ...
)
```

## Arguments

- mapping, data, position, na.rm, show.legend, inherit.aes:

  As in
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- stat:

  Kept for compatibility; the stat is always `StatConcordia`.

- level:

  Confidence level of the ellipse (default 0.95). Use `NULL` to draw the
  raw 1-sigma ellipse, as in versions before 0.0.0.9001.

- sigma_level:

  Level of the uncertainties given in `sigma_x`/`sigma_y`: `1` (default,
  standard errors) or `2` (2-sigma). With the package convention of
  `*_2s` columns, either map `sigma_x = pb207_u235_2s` and set
  `sigma_level = 2`, or divide by 2 in the mapping.

- n:

  Number of vertices per ellipse.

- ...:

  Other arguments passed to the polygon geom (e.g. `alpha`, `colour`,
  `fill`).

## Details

Required aesthetics: `x`, `y`, `sigma_x`, `sigma_y`, `rho`. `fill` (and
any other aesthetic) is optional. Each row gets its own polygon, so
ellipses never merge, whatever the grouping.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x = pb207_u235, y = pb206_u238, sigma_x = pb207_u235_2s,
               sigma_y = pb206_u238_2s, rho = rho_206pb_238u_v_207pb_235u, fill = sample)) +
  geom_concordia(sigma_level = 2, alpha = 0.5) +
  geom_concordia_line(age_range = c(900, 1200))
} # }
```
