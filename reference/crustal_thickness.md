# Crustal thickness from zircon Eu/Eu\*

Linear calibration `84.92 * Eu/Eu* + 24.5` (km). The coefficients are
attributed to Tang et al. (2021) and still need to be checked against
the publication.

## Usage

``` r
crustal_thickness(eu_eu_N)
```

## Arguments

- eu_eu_N:

  Zircon Eu/Eu\* (chondrite-normalized), see
  [`anomaly()`](https://gabertol.github.io/ztR/reference/anomaly.md).

## Value

Crustal thickness in km.
