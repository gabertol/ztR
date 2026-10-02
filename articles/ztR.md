# Getting started with ztR

ztR (**Z**ircon–**T**ourmaline–**R**utile) is a small toolbox for
mineral chemistry and zircon geochronology that works the *tidy* way: a
data frame goes in, the same data frame comes out with new columns. It
wraps the heavy lifting of packages such as IsoplotR so you do not have
to deal with their own data classes.

``` r

library(ztR)
library(tidyverse)
```

## What is in the package

| Topic | Functions | Article |
|----|----|----|
| Mineral formulae (APFU) | [`chemical_formula()`](https://gabertol.github.io/ztR/reference/chemical_formula.md) | [`vignette("mineral-formula")`](https://gabertol.github.io/ztR/articles/mineral-formula.md) |
| Zircon trace elements | [`normalize()`](https://gabertol.github.io/ztR/reference/normalize.md), [`ratio_calculator()`](https://gabertol.github.io/ztR/reference/ratio_calculator.md), [`anomaly()`](https://gabertol.github.io/ztR/reference/anomaly.md) | [`vignette("zircon-geochemistry")`](https://gabertol.github.io/ztR/articles/zircon-geochemistry.md) |
| Thermometry and oxybarometry | [`zircon_ti_t()`](https://gabertol.github.io/ztR/reference/zircon_ti_t.md), [`zircon_ti_p()`](https://gabertol.github.io/ztR/reference/zircon_ti_p.md), [`FMQ()`](https://gabertol.github.io/ztR/reference/FMQ.md) | [`vignette("zircon-geochemistry")`](https://gabertol.github.io/ztR/articles/zircon-geochemistry.md) |
| Source-rock classification | [`classify_rock_long()`](https://gabertol.github.io/ztR/reference/classify_rock_long.md), [`classify_rock_short()`](https://gabertol.github.io/ztR/reference/classify_rock_long.md), [`classify_rock_long_no_ce()`](https://gabertol.github.io/ztR/reference/classify_rock_long.md) | [`vignette("zircon-geochemistry")`](https://gabertol.github.io/ztR/articles/zircon-geochemistry.md) |
| Parent-rock composition | [`WR_calculator()`](https://gabertol.github.io/ztR/reference/WR_calculator.md), [`WR_crustal_thickness()`](https://gabertol.github.io/ztR/reference/WR_crustal_thickness.md), [`crustal_thickness()`](https://gabertol.github.io/ztR/reference/crustal_thickness.md) | [`vignette("zircon-geochemistry")`](https://gabertol.github.io/ztR/articles/zircon-geochemistry.md) |
| U-Pb ages | [`use_isoplotr()`](https://gabertol.github.io/ztR/reference/use_isoplotr.md) | [`vignette("u-pb-geochronology")`](https://gabertol.github.io/ztR/articles/u-pb-geochronology.md) |
| Concordia diagrams | [`geom_concordia()`](https://gabertol.github.io/ztR/reference/geom_concordia.md), [`geom_concordia_line()`](https://gabertol.github.io/ztR/reference/geom_concordia_line.md) | [`vignette("u-pb-geochronology")`](https://gabertol.github.io/ztR/articles/u-pb-geochronology.md) |

## Conventions

ztR expects lower-case column names made of the element symbol and the
mass of the measured isotope, and uncertainties in a column with the
same name plus the suffix `_2s`:

| Quantity | Column | Uncertainty |
|----|----|----|
| La (ppm) | `la139` | `la139_2s` |
| U (ppm) | `approx_u` | `approx_u_2s` |
| 207Pb/235U | `pb207_u235` | `pb207_u235_2s` |
| 206Pb/238U | `pb206_u238` | `pb206_u238_2s` |
| 207Pb/206Pb | `pb207_pb206` | `pb207_pb206_2s` |
| Wetherill error correlation | `rho_206pb_238u_v_207pb_235u` | — |
| Tera-Wasserburg error correlation | `rho_207pb_206pb_v_238u_206pb` | — |

- Concentrations are in ppm.
- Uncertainties are **2-sigma absolute**.
- Ages are in Ma.
- Chondrite-normalized values get the suffix `_N`; whole-rock estimates
  the suffix `_WR`.

## Reading an Iolite export

The package ships a table of reference zircons (91500, Plešovice, FC-1
and PCIGR-1) exported from Iolite. Iolite names columns like
`la139_ppm_mean` and `la139_ppm_2se_int`; one
[`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html)
brings them to the ztR convention:

``` r

zircons <- read_csv(system.file("extdata", "stds_zircon.csv", package = "ztR"),
                    show_col_types = FALSE, name_repair = "unique_quiet") %>%
  rename_with(~ .x %>%
                str_replace_all("_mean|_ppm", "") %>%
                str_replace_all("_2se_int", "_2s")) %>%
  mutate(sample = recode(sample, "91500-G" = "91500", "FC1" = "FC-1"))

zircons %>% count(sample)
#> # A tibble: 4 × 2
#>   sample     n
#>   <chr>  <int>
#> 1 91500   1789
#> 2 FC-1     756
#> 3 PCIGR1    42
#> 4 PL       512
```

The same object is used in the other articles.

## A first pipeline

Every function adds columns, so they chain:

``` r

zircons %>%
  filter(sample == "PL") %>%
  slice(1:5) %>%
  normalize(element_vector = c(la139:lu175)) %>%
  mutate(eu_eu = anomaly(eu153_N, sm147_N, gd157_N),
         t_ti  = zircon_ti_t(ti_ppm = ti49)) %>%
  select(sample, eu_eu, t_ti)
#> # A tibble: 5 × 3
#>   sample eu_eu  t_ti
#>   <chr>  <dbl> <dbl>
#> 1 PL     0.403  985.
#> 2 PL     0.397 1004.
#> 3 PL     0.357 1013.
#> 4 PL     0.416  987.
#> 5 PL     0.395  993.
```

## Backwards compatibility

Version 0.0.0.9001 fixed several bugs (see `NEWS.md`). Old code keeps
running:

- [`ratio_calculator()`](https://gabertol.github.io/ztR/reference/ratio_calculator.md)
  and
  [`use_isoplotr()`](https://gabertol.github.io/ztR/reference/use_isoplotr.md)
  return their old columns by default (`legacy = TRUE`) and warn once
  per session about what those columns really contain;
- [`add_concordia_line()`](https://gabertol.github.io/ztR/reference/add_concordia_line.md)
  still exists, with its original (transposed) axes;
  [`geom_concordia_line()`](https://gabertol.github.io/ztR/reference/geom_concordia_line.md)
  is the new version.
