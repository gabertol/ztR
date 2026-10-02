# Pressure from Ti in zircon

Solves 0.013 P^2 - 0.21 P + (3.41 - log10(Ti)) = 0 for P (GPa) and
returns the smallest real root. Vectorized. Returns `NA` when there is
no real solution (with these coefficients, for Ti below ~365 ppm).

## Usage

``` r
zircon_ti_p(ti)
```

## Arguments

- ti:

  Ti in zircon (ppm). Numeric vector.

## Value

Pressure in GPa (numeric vector, `NA` where there is no real root).

## Details

The source of the coefficients is not documented yet; check before use.

## Examples

``` r
zircon_ti_p(c(10, 400, 1000))
#> [1]       NA 6.319808 2.271906
```
