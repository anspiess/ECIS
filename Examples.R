# 1) Import dataset
RES1 <- getECIS("C:\\Users\\User\\Desktop\\Pfaffl\\2020-03-06-wip-1001.xlsx")

# 2) Let us look at these two in matrix format with well legend
plotECIS(RES1, style = "matrix", legend = TRUE) # => 16 plots 

# 2b) Now also with line style
plotECIS(RES1, style = "matrix", legend = TRUE, type = "l")

# 3) In RES1, data 4/8/12/16 do not look well, so let's delete them and plot again
RES1.2 <- delECIS(RES1, which = c(4, 8, 12, 16))
plotECIS(RES1.2, style = "matrix", legend = TRUE) # => 12 plots

# 4) We could give each plot a different color
plotECIS(RES1.2, style = "matrix", legend = TRUE, col = rainbow(12)) # => 12 plots

# 5) We could even attach RES1 to RES1.2 and show the two groups by color
RES3 <- addECIS(RES1.2, RES1)
plotECIS(RES3, style = "matrix", legend = TRUE, col = rep(c("darkred", "blue"), c(12, 16))) # => 28 plots

# 6a) In RES1.2 we see a very long baseline with Imp ~ 400 Ohm.
plotECIS(RES1.2, style = "single")

# 6b) let us cut the long baseline off at 35 hours.
RES1.3 <- cutECIS(RES1.2, min = 35)
plotECIS(RES1.3, style = "single")

# 7c) We might also do some baselining to ~ 0 Ohm based on the average of the first 2 hours
RES1.4 <- baseECIS(RES1.3, type = "mean", window = 2)
plotECIS(RES1.4, style = "single")

# 8) Often, analyses work better if data is normalized to [0, 1]
RES1.5 <- normECIS(RES1.2)
plotECIS(RES1.5, style = "single")

# 9a) We might extend the data to -10 hours before baseline
RES1.6 <- extECIS(RES1.2, extend = -10)
plotECIS(RES1.6)

# 9b) We might extend the data to 120 hours after plateau
RES1.7 <- extECIS(RES1.2, extend = 120)
plotECIS(RES1.7)

# 10) Set a new zero-point at 30 h
RES1.8 <- setECIS(RES1.2, time = 30)
plotECIS(RES1.8)

# 11a) To the normalized data from 8) we will fit a spline, 4-parameter logistic and segmented model
FIT1 <- fitECIS(RES1.2) 

# 11b) We can also plot this fit raw
plotECIS(FIT1, style = "matrix")

# 11c) ... and add either the spline, logistic or segmented fit
plotECIS(FIT1, style = "matrix", fit = "spl")
plotECIS(FIT1, style = "matrix", fit = "log")
plotECIS(FIT1, style = "matrix", fit = "seg")

# 12a) But we can also fit the unprocessed data
FIT2 <- fitECIS(RES1.2, nknots = 200)
plotECIS(FIT2, style = "matrix", fit = "spl")

# 12b) ... or just select two curves
plotECIS(FIT2, which= c(2, 3), style = "matrix", col = c("red","blue"), fit = "spl") 

# 13) This is how we can calculate the Simpson's integrals on the normalized data
intECIS(RES1.5)

# 14) Finally, we obtain all essential parameters from the "fit list"
PAR <- parECIS(FIT2)
print(PAR)




