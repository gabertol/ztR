# Add a concordia line (legacy orientation)

Kept for compatibility. **Note the axes:** x = 206Pb/238U and y =
207Pb/235U, which is the transpose of the usual Wetherill diagram. For
the standard orientation (x = 207Pb/235U, y = 206Pb/238U) or a
Tera-Wasserburg diagram use
[`geom_concordia_line()`](https://gabertol.github.io/ztR/reference/geom_concordia_line.md).

## Usage

``` r
add_concordia_line(
  x_interval = 100,
  lambda_235 = 9.8485e-10,
  lambda_238 = 1.55125e-10
)
```

## Arguments

- x_interval:

  Interval between age markers (Ma).

- lambda_235, lambda_238:

  Decay constants (1/yr).

## Value

A list of ggplot2 layers.

## Details

The layers use `inherit.aes = FALSE`, so they can be added to a plot
whose global aesthetics include `sigma_x`, `sigma_y`, `rho` etc. (this
failed before version 0.0.0.9001).
