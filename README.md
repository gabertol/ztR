# ztR
<b>Functions to deal with mineral geochemistry, zircon geochemistry normalization and convertion to whole-rock trace elements, ratios and proxies calculations</b>


<b>1. Mineral end-members and crystal chemistry calculations for SEM-EDS, EMPA or any mineral chemistry dataset.</b>

Function chemical_formula()

Calculation based on the seminal book "An introduction to the rock-forming minerals" annex 1. The calculatios are based on the no of oxigens in the mineral formula- which works virtually with every mineral in the book. 

The package is a simple wrap-up function of tidy- style operation to transform raw mineral chemistry from EPMA,SEM, LA-ICP to APFU values.

The data includes 3 mineral chemistry raw data from DHZ- garnet, tourmaline, and epidote- for benchmarkig (see vignette).

Howie, R.A., Zussman, J. and Deer, W., 1992. An introduction to the rock-forming minerals (p. 696). London, UK: Longman.

<b>2. Zircon geochemistry normalization.</b>

Function normalize()

- "mcdon_sun_1995.csv" based on McDonough, W.F. and Sun, S.S., 1995. The composition of the Earth. *Chemical Geology*, 120(3-4), pp.223-253.
- "taylor_mcclennan_1985.csv"` based on Taylor, S.R., 1985. The continental crust: Its composition and evolution. *Geoscience Texts*, 312.

<b>3. Zircon geochemistry whole-rock calculation.</b>

Function WR_calculator()
- based on Chapman, J.B., Gehrels, G.E., Ducea, M.N., Giesler, N. and Pullen, A., 2016. A new method for estimating parent rock trace element concentrations from zircon. Chemical Geology, 439, pp.59-70.

<b>4. Zircon geochemistry ratios and proxies.</b>

- FMQ() based on Loucks et al
- crustal_thickness() based on Tang et al
- WR_crustal_thickness() based on Profeta et al 2015
- ratio_calculator based on Sundell et al 2020
- anomaly() for Eu/Eu* or Ce/Ce* calculation

<b>5. Zircon source rock classification based on geochemistry</b>  

- classify_rock_long() to classify zircons based on long CART from Belosouva et al 2002
- classify_rock_long_no_ce() to classify zircons based on long CART from Belosouva et al 2002 removing the Ce steps
- classify_rock_short() to classify zircons based on short CART from Belosouva et al 2002

<b>How to use</b>                                                  
1- Install devtools in R
install.packages("devtools")

2- Import dev tools from library and use install_github to download this package
library(devtools)
devtools::install_github("gabertol/ztR")
