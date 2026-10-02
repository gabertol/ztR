# Zircon trace-element ratios and proxies

Calculates common zircon ratios and proxies from chondrite-normalized
REE (`*_N` columns, as produced by
[`normalize()`](https://gabertol.github.io/ztR/reference/normalize.md))
and raw concentrations in ppm (`ce140`, `approx_u`, `ti49`, `hf177`,
`y89`).

## Usage

``` r
ratio_calculator(
  dataframe,
  equation = "watson",
  pressure = NULL,
  legacy = TRUE,
  ...
)
```

## Arguments

- dataframe:

  Data frame with `*_N` columns and raw `ce140`, `approx_u`, `ti49`,
  `hf177`, `y89` and `best_age` (Ma).

- equation:

  Ti-in-zircon calibration passed to
  [`zircon_ti_t()`](https://gabertol.github.io/ztR/reference/zircon_ti_t.md).

- pressure:

  Pressure in GPa passed to
  [`zircon_ti_t()`](https://gabertol.github.io/ztR/reference/zircon_ti_t.md)
  (needed only for `"crisp"`).

- legacy:

  If `TRUE` (default), returns the column names of previous versions,
  keeping their old definitions (see Details) and warning once per
  session. If `FALSE`, returns columns named after what they compute.

- ...:

  Further arguments passed to
  [`zircon_ti_t()`](https://gabertol.github.io/ztR/reference/zircon_ti_t.md)
  (e.g. `aSiO2`, `aTiO2`).

## Value

`dataframe` with the ratio columns added.

## Details

Column definitions with `legacy = TRUE` (kept for backwards
compatibility):

- `dy_nd` = Dy_N / Yb_N (despite the name, this is Dy/Yb);

- `yb_gd` = Y (ppm) / Gd_N (despite the name, this is Y/Gd, mixing raw
  and normalized values);

- `H_L_ree` = (La..Sm)\_N / (Eu, Gd, Tb, Ho..Lu)\_N, i.e. LREE/HREE,
  without Dy.

Column definitions with `legacy = FALSE`:

- `dy_yb` = Dy_N / Yb_N;

- `yb_gd` = Yb_N / Gd_N;

- `lree_hree` = (La..Sm)\_N / (Gd..Lu)\_N, Dy included.

In both modes `fmq` is computed with raw Ce, U and Ti in ppm (Loucks et
al. 2020), and `ti_temp` receives `pressure`. Before version 0.0.0.9001
`fmq` was computed with chondrite-normalized Ce, which was wrong.
