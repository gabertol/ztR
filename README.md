# ztR

**Tidy tools for mineral chemistry and zircon geochemistry and geochronology.**

ztR (Zircon–Tourmaline–Rutile) works the tidy way: a data frame goes in, the same data frame comes
out with new columns. It also wraps IsoplotR so U-Pb ages can be computed without dealing with its
own data classes.

## Installation

```r
install.packages("devtools")
devtools::install_github("gabertol/ztR")

# optional, for U-Pb ages
install.packages("IsoplotR")
```

## Quick example

```r
library(ztR)
library(tidyverse)

zircons <- read_csv(system.file("extdata", "stds_zircon.csv", package = "ztR")) %>%
  rename_with(~ .x %>% str_replace_all("_mean|_ppm", "") %>% str_replace_all("_2se_int", "_2s"))

zircons %>%
  normalize(element_vector = c(la139:lu175)) %>%
  mutate(eu_eu = anomaly(eu153_N, sm147_N, gd157_N),
         t_ti  = zircon_ti_t(ti_ppm = ti49)) %>%
  use_isoplotr(legacy = FALSE)
```

## What it does

| Topic | Functions |
|---|---|
| Mineral formulae (APFU) | `chemical_formula()` |
| Normalization | `normalize()` |
| Ratios and proxies | `ratio_calculator()`, `anomaly()`, `FMQ()`, `crustal_thickness()` |
| Ti-in-zircon | `zircon_ti_t()`, `zircon_ti_p()` |
| Source-rock classification | `classify_rock_long()`, `classify_rock_long_no_ce()`, `classify_rock_short()` |
| Parent-rock composition | `WR_calculator()`, `WR_crustal_thickness()` |
| U-Pb ages | `use_isoplotr()` |
| Concordia diagrams | `geom_concordia()`, `geom_concordia_line()` |

## Conventions

* Columns named after element and isotope mass: `la139`, `ti49`, `pb206_u238`.
* Uncertainties in a column with the suffix `_2s`, **2-sigma absolute**.
* Concentrations in ppm, ages in Ma.
* Normalized values get the suffix `_N`; whole-rock estimates, `_WR`.

## Articles

* `vignette("ztR")`: getting started
* `vignette("zircon-geochemistry")`: normalization, ratios, thermometry, classification, whole rock
* `vignette("u-pb-geochronology")`: ages with IsoplotR and concordia diagrams
* `vignette("mineral-formula")`: APFU of tourmaline, garnet and epidote

Online: <https://gabertol.github.io/ztR/>

## References

**Mineral formulae.** Deer, W.A., Howie, R.A. and Zussman, J., 1992. *An introduction to the
rock-forming minerals*. Longman, London. The package ships the garnet, tourmaline and epidote
analyses of the book as benchmark data.

**Normalization.**
* McDonough, W.F. and Sun, S.S., 1995. The composition of the Earth. *Chemical Geology*, 120,
  223–253 (`"mcdon_sun_1995.csv"`, default).
* Taylor, S.R. and McLennan, S.M., 1985. *The continental crust: its composition and evolution*.
  Blackwell (`"taylor_mcclennan_1985.csv"`).

**Ratios and proxies.**
* Loucks, R.R., Fiorentini, M.L. and Henríquez, G.J., 2020. New magmatic oxybarometer using trace
  elements in zircon. *Journal of Petrology*, 61, egaa034 (`FMQ()`).
* Tang, M., Ji, W.-Q., Chu, X., Wu, A. and Chen, C., 2021. Reconstructing crustal thickness
  evolution from europium anomalies in detrital zircons. *Geology*, 49, 76–80
  (`crustal_thickness()`).
* Profeta, L., Ducea, M.N., Chapman, J.B. et al., 2015. Quantifying crustal thickness over time in
  magmatic arcs. *Scientific Reports*, 5, 17786 (`WR_crustal_thickness()`).
* Sundell, K.E. et al., 2020 (`ratio_calculator()`).

**Ti-in-zircon.**
* Watson, E.B. and Harrison, T.M., 2005. *Science*, 308, 841–844.
* Ferry, J.M. and Watson, E.B., 2007. *Contributions to Mineralogy and Petrology*, 154, 429–437.
* Crisp, L.J. et al., 2023. *Geochimica et Cosmochimica Acta*, 360, 241–258.

**Classification.** Belousova, E.A., Griffin, W.L., O'Reilly, S.Y. and Fisher, N.I., 2002. Igneous
zircon: trace element composition as an indicator of source rock type. *Contributions to
Mineralogy and Petrology*, 143, 602–622.

**Parent-rock composition.** Chapman, J.B., Gehrels, G.E., Ducea, M.N., Giesler, N. and Pullen,
A., 2016. A new method for estimating parent rock trace element concentrations from zircon.
*Chemical Geology*, 439, 59–70.

**U-Pb.** Vermeesch, P., 2018. IsoplotR: a free and open toolbox for geochronology. *Geoscience
Frontiers*, 9, 1479–1493.

<b>How to use</b>                                                  
1- Install devtools in R
install.packages("devtools")

2- Import dev tools from library and use install_github to download this package
library(devtools)
devtools::install_github("gabertol/ztR")
