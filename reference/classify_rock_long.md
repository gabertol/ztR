# Classify Rock Types Based on Geochemical Data

This function classifies rock types based on various geochemical
parameters, using specific thresholds. The classification scheme
includes categories such as "Ne-Syenite", "Syenite", "Dolerite",
"Carbonatite", "Kimberlite", "Basalt", "Larvikite", and various types of
"Granitoid" based on Belousova et al. (2002). Belousova, E. A., Griffin,
W. L., O'Reilly, S. Y., & Fisher, N. L. (2002). Igneous zircon: trace
element composition as an indicator of source rock type. Contributions
to mineralogy and petrology, 143, 602-622.

## Usage

``` r
classify_rock_long(
  data,
  lu_col = "lu175",
  u_col = "approx_u",
  ta_col = "ta181",
  hf_col = "hf",
  ce_ce_col = "ce_ce",
  nb_col = "nb93",
  th_u_col = "th_u"
)

classify_rock_short(
  data,
  lu_col = "lu175",
  u_col = "approx_u",
  hf_col = "hf",
  y_col = "y89",
  yb_col = "yb172"
)

classify_rock_long_no_ce(
  data,
  lu_col = "lu175",
  u_col = "approx_u",
  ta_col = "ta181",
  hf_col = "hf",
  nb_col = "nb93",
  th_u_col = "th_u"
)
```

## Arguments

- data:

  A data frame containing geochemical data for classification.

- lu_col:

  Column name as a string for the `lu175` parameter.

- u_col:

  Column name as a string for the `approx_u` parameter.

- ta_col:

  Column name as a string for the `ta181` parameter.

- hf_col:

  Column name as a string for the `hf` parameter.

- ce_ce_col:

  Column name as a string for the `ce_ce` parameter.

- nb_col:

  Column name as a string for the `nb93` parameter.

- th_u_col:

  Column name as a string for the `th_u` parameter.

- y_col:

  Column name for `y89`.

- yb_col:

  Column name for `yb172`.

## Value

A data frame with an added `classification` column specifying the
classified rock type.

## Details

Each split uses `<` on the left branch and `>=` on the right branch, so
values exactly at a threshold are classified (before version 0.0.0.9001
they fell into "Unclassified"). Rows with `NA` in a variable needed by
the tree are returned as "Unclassified". Thresholds follow Belousova et
al. (2002); Hf thresholds are in wt% and the other elements in ppm.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
classify_rock(data = geochemical_data)
} # }
```
