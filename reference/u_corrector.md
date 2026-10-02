# Initial U content of a zircon

Back-calculates the U content at crystallization from the present-day
content and the age, splitting U into 238U and 235U with
`u238_u235_ratio`.

## Usage

``` r
u_corrector(
  U_ppm,
  age_myr,
  lambda_238 = 1.55125e-10,
  lambda_235 = 9.8485e-10,
  u238_u235_ratio = 137
)
```

## Arguments

- U_ppm:

  Present-day U (ppm).

- age_myr:

  Crystallization age (Ma).

- lambda_238, lambda_235:

  Decay constants (1/yr).

- u238_u235_ratio:

  Present-day 238U/235U (default 137, kept for compatibility; the
  currently accepted value is 137.818, Hiess et al. 2012).

## Value

Initial U (ppm).

## Examples

``` r
u_corrector(100, 1000)
#> [1] 117.8743
```
