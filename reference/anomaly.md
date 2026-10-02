# Element anomaly (e.g. Eu/Eu\*, Ce/Ce\*)

Ratio between the normalized concentration of an element and the
geometric mean of its two neighbours: `REE / sqrt(LREE * HREE)`.

## Usage

``` r
anomaly(REE, LREE, HREE)
```

## Arguments

- REE:

  Normalized concentration of the element (e.g. Eu_N).

- LREE:

  Normalized concentration of the lighter neighbour (e.g. Sm_N).

- HREE:

  Normalized concentration of the heavier neighbour (e.g. Gd_N).

## Value

Numeric vector.

## Examples

``` r
anomaly(REE = 0.5, LREE = 1, HREE = 4)   # Eu/Eu* = 0.25
#> [1] 0.25
```
