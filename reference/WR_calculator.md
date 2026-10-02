# Whole-Rock (WR) Elemental Calculator

This function calculates whole-rock (WR) normalized values for selected
elements in a given dataset. It uses a reference dataset based on
Chapman et al., 2016 for normalization. The Chapman et al. dataset
provides partition coeficients to calculate WR elements based on Zircon
trace element data.

## Usage

``` r
WR_calculator(
  BD,
  element_vector = c(y89, nb93, la139:lu175),
  database = "chapman_etal_2016.csv",
  element_plus_size = TRUE,
  error_tag = "_2s",
  normalized = TRUE,
  tag = ""
)
```

## Arguments

- BD:

  A data frame containing elemental concentration data.

- element_vector:

  A vector of elements to include for WR normalization (default:
  `c(y89, nb93, la139:lu175)`).

- database:

  The name of the CSV file containing reference values
  (`"chapman_etal_2016.csv"`).

- element_plus_size:

  Logical indicating if elements with size should be used from
  `database` (default is TRUE).

- error_tag:

  The suffix to ignore in `BD` when selecting columns (default is
  `"_2s"`).

- normalized:

  Logical indicating if the result should be normalized (default is
  TRUE).

- tag:

  Optional tag to append to elements in the output (default is "").

## Value

A data frame with WR normalized values for each element, with suffix
`_WR`.

## Details

Parent-rock concentrations are estimated as
`C_zircon / (a * C_zircon^b)` with the coefficients of Chapman, J.B.,
Gehrels, G.E., Ducea, M.N., Giesler, N. and Pullen, A., 2016. A new
method for estimating parent rock trace element concentrations from
zircon. *Chemical Geology*, 439, 59-70.
