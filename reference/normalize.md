# Normalize Elemental Data

This function normalizes elemental concentration data using reference
values from selected geochemical databases. Users can choose from two
available datasets:

- `"mcdon_sun_1995.csv"` based on McDonough, W.F. and Sun, S.S., 1995.
  The composition of the Earth. *Chemical Geology*, 120(3-4),
  pp.223-253.

- `"taylor_mcclennan_1985.csv"` based on Taylor, S.R., 1985. The
  continental crust: Its composition and evolution. *Geoscience Texts*,
  312.

## Usage

``` r
normalize(
  BD,
  element_vector = al27:ta181,
  database = "mcdon_sun_1995.csv",
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

  A range of elements to include for normalization (default:
  al27:ta181).

- database:

  The name of the CSV file containing reference values
  (`"mcdon_sun_1995.csv"` or `"taylor_mcclennan_1985.csv"`).

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

A data frame with normalized values for each element.

## Details

The reference file is read with `.read_ref()`, which removes a UTF-8
BOM, parses numbers written with thousands separators and converts wt%
to ppm. The McDonough & Sun (1995) CI table shipped with the package is
stored entirely in ppm.
