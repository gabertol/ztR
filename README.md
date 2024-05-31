# ztR

<b>Mineral end-members and crystal chemistry calculations for SEM-EDS, EMPA or any mineral chemistry dataset.</b>

Calculation based on the seminal book "An introduction to the rock-forming minerals" annex 1. The calculatios are based on the no of oxigens in the mineral formula- which works virtually with every mineral in the book. 

The package is a simple wrap-up function of tidy- style operation to transform raw mineral chemistry from EPMA,SEM, LA-ICP to APFU values.

The data includes 3 mineral chemistry raw data from DHZ- garnet, tourmaline, and epidote- for benchmarkig (see vignette).

Howie, R.A., Zussman, J. and Deer, W., 1992. An introduction to the rock-forming minerals (p. 696). London, UK: Longman.

<b>How to use</b>                                                  
1- Install devtools in R
install.packages("devtools")

2- Import dev tools from library and use install_github to download this package
library(devtools)
devtools::install_github("gabertol/ztR")
