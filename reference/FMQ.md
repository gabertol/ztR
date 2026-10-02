# Zircon oxybarometer (Loucks et al. 2020)

Delta FMQ = 3.998 \* log10(Ce / sqrt(Ui \* Ti)) + 2.284, with
concentrations in ppm and Ui the initial U content (U corrected for
decay since crystallization).

## Usage

``` r
FMQ(ce, u, ti, age, correct_u = TRUE)
```

## Arguments

- ce:

  Ce in ppm (raw, not chondrite-normalized).

- u:

  U in ppm (present-day).

- ti:

  Ti in ppm.

- age:

  Crystallization age in Ma (used to correct U).

- correct_u:

  If `TRUE` (default) U is corrected to its initial value with
  [`u_corrector()`](https://gabertol.github.io/ztR/reference/u_corrector.md).

## Value

Delta FMQ. `NA` where any concentration is zero, negative or missing.

## References

Loucks, R.R., Fiorentini, M.L. and Henríquez, G.J., 2020. New magmatic
oxybarometer using trace elements in zircon. *Journal of Petrology*,
61(3), egaa034.
