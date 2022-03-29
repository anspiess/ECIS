# 29-03-2022

## New function `getECIS2()` for importing data in the format x [Time], y1 [Imp], y2 [Imp], y3 [Imp], ...
The new function `getECIS2(file = NULL, names = NULL, na.omit = TRUE)` has been added.  
In contrast to `getECIS()`, this function imports data in the "classical format" x [Time], y1 [Imp], y2 [Imp], y3 [Imp], ...  
The output of the function is similar to `getECIS()`, so that all downstream functions work.  
Parameters are:  
* `file`: path to file, otherwise a selection window will pop up
* `names`: possibly overwrite and define the column names of x, y1, y2, y3, ...
* `na.omit`: If `TRUE` (default), alll data rows with NAs are omitted, to avoid fitting errors. 

### Example
Using the file "Classical.xlsx", do the following:
```
if (!'devtools' %in% installed.packages()) install.packages(devtools)
library(devtools)
source_url("https://raw.githubusercontent.com/anspiess/ECIS/main/getECIS2.R")
IN <- getECIS2()
FIT <- fitECIS(IN)
parECIS(FIT)
plotECIS(FIT, style = "single", fit = "spl")
plotECIS(FIT, style = "matrix", fit = "log")
```
