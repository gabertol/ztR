# Calculate Atoms Per Formula Unit (APFU) for Minerals

This function calculates the atoms per formula unit (APFU) for each
element in a given mineral dataset. The calculations can be based on
either the number of oxygens (anions) or cations.

## Usage

``` r
chemical_formula(
  dataframe,
  oxigens,
  cations,
  weight_location = "/data/element_weights.csv",
  base = "anions"
)
```

## Arguments

- dataframe:

  DataFrame. A dataframe with element oxides in %weight.

- oxigens:

  Numeric. Number of oxygens used for base calculation.

- cations:

  Numeric. Number of cations used for base calculation.

- weight_location:

  String. The file location of an in-house oxides weight dataframe,
  otherwise will use the one in the folder ./data.

- base:

  String. Either "anions" or "cations" to indicate the basis of the
  calculation.

## Value

DataFrame with atoms per formula unit (APFU) for each element without
crystallographic site balancing.

## Examples

``` r
# Example for Tourmaline
read.csv(system.file("extdata", "tourmaline_DHZ.csv", package = "ztR")) %>%
  chemical_formula(oxigens = 31, cations = 18, base = "anions")
#> # A tibble: 6 × 17
#> # Groups:   specimen [6]
#>   specimen    Si     B    Al      Ti      Fe   Fe3      Mg      Mn      Zn    Li
#>      <int> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>
#> 1        1  6.08  3.07  4.63 0.206   0.973   1.27  2.18    0       0       0    
#> 2        2  5.84  3.22  5.13 0.0758  0.0557  0     3.68    0       0       0    
#> 3        3  5.93  3.01  6.45 0.0220  1.90    0.431 0.0180  0.130   0.0153  0    
#> 4        4  5.93  2.90  7.78 0       0.496   0     0.0219  0.145   0       0.833
#> 5        5  5.85  3.02  7.36 0.00620 0.0193  0     0.00246 1.14    0.00243 0.484
#> 6        6  4.91  4.11  8.56 0.00235 0.00653 0     0       0.00265 0.0104  0.352
#> # ℹ 6 more variables: K <dbl>, Ca <dbl>, Sr <dbl>, Na <dbl>, F <dbl>, OH <dbl>

# Example for Epidote
read.csv(system.file("extdata", "epidote_DHZ.csv", package = "ztR")) %>%
  chemical_formula(oxigens = c(12.5, 13, 12.5, 12.5, 12.5, 13), cations = 8, base = "cations")
#> # A tibble: 6 × 16
#> # Groups:   specimen [6]
#>   specimen    Si    Al      Ti     Fe     Fe3      Mg      Mn   Mn3       K
#>      <int> <dbl> <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl> <dbl>   <dbl>
#> 1        1  3.09  2.92 0.00112 0      0.00683 0       0.00314 0     0.00474
#> 2        2  3.01  2.86 0.00746 0.0223 0.103   0.0387  0.00194 0     0      
#> 3        3  2.87  2.72 0.00843 0      0.476   0.00669 0.00697 0     0      
#> 4        4  2.98  1.53 0.0303  0      1.55    0.00360 0.00682 0     0.00205
#> 5        5  3.18  2.21 0.0155  0      0.158   0.00639 0       0.437 0      
#> 6        6  3.01  1.71 0.0522  0.745  0.630   0.0656  0.427   0     0      
#> # ℹ 6 more variables: Ca <dbl>, Na <dbl>, Y <dbl>, Ce <dbl>, La <dbl>, OH <dbl>
```
