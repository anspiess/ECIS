# ECIS: Analysis of Electric Cell-substrate Impedance Sensing Data (R-scripts)

## Overview
Provides functions for the analysis of ECIS data, such as          

* Import of data (.xlsx or .csv), with optional selection of frequencies
* Plotting data in matrix or single format, with or without fitted curves
* Deleting, adding and selecting curves from/to a larger set
* Modification of ECIS data:
  + cutting of time points
  + baselining ECIS data
  + normalizing within [0, 1]
  + extending ECIS data on either side
* Anomaly (spike/outlier) detection and removal
* Fitting ECIS data with 
  + Smoothing cubic spline
  + 4-parameter logistic curve
  + Segmented linear regression
* Calculating integrals
* Acquisition of essential fit parameters, such as
  + First and second derivative maxima and corresponding time points
  + Quantile locations (10%, 20%, 50%)
  + Slopes
  + Fit coefficients
  + Segmentation knots

## Repo Contents
A file containing all functions (`allECIS.R`) and a file with Examples (`Examples.R`).
          
## Software Requirements
`R` version greater than 3.9.0 for Linux Ubuntu 16, Windows 7, 8, 10 or Mac.  

## Package dependencies
Users should install the following packages from an `R` terminal:
```
install.packages(c("minpack.lm", "segmented", "splines", "readxl"))
```
## Installation Guide
To have all functions available in your `R` environment, type the following in an `R` session:
```
if (!'devtools' %in% installed.packages()) install.packages(devtools)
library(devtools)
source_url("https://raw.githubusercontent.com/anspiess/ECIS/main/allECIS.R")

```
