# Concordia line for Wetherill or Tera-Wasserburg diagrams

Concordia line for Wetherill or Tera-Wasserburg diagrams

## Usage

``` r
geom_concordia_line(
  type = c("wetherill", "tw"),
  age_range = c(1, 4500),
  ticks = NULL,
  lambda_235 = 9.8485e-10,
  lambda_238 = 1.55125e-10,
  u238_u235 = 137.818,
  colour = "grey30",
  linewidth = 0.4,
  label_size = 3
)
```

## Arguments

- type:

  `"wetherill"` (x = 207Pb/235U, y = 206Pb/238U) or `"tw"` (x =
  238U/206Pb, y = 207Pb/206Pb).

- age_range:

  Age range of the line (Ma).

- ticks:

  Ages (Ma) of the labelled markers. Default:
  [`pretty()`](https://rdrr.io/r/base/pretty.html) over `age_range`.

- lambda_235, lambda_238:

  Decay constants (1/yr).

- u238_u235:

  Present-day 238U/235U (used in the Tera-Wasserburg line).

- colour, linewidth:

  Line colour and width.

- label_size:

  Text size of the tick labels (`NA` hides the labels).

## Value

A list of ggplot2 layers (all with `inherit.aes = FALSE`).

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(pb207_u235, pb206_u238, sigma_x = pb207_u235_2s / 2,
               sigma_y = pb206_u238_2s / 2, rho = rho)) +
  geom_concordia_line(age_range = c(200, 700)) +
  geom_concordia()
} # }
```
